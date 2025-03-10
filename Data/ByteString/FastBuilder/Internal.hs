{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE RankNTypes #-}

-- | This is an internal module; its interface is unstable.
module Data.ByteString.FastBuilder.Internal
  (
  -- * Builder and related types
    Builder(..)
  , BuilderState
  , DataSink(..)
  , DynamicSink(..)
  , Queue(..)
  , Request(..)
  , Response(..)

  -- * Internally used exceptions
  , SuspendBuilderException(..)
  , ChunkOverflowException(..)

  -- * Builder building blocks
  , BuildM(..)
  , mkBuilder
  , useBuilder
  , getSink
  , getCur
  , getEnd
  , setCur
  , setEnd

  -- * Running builders
  , runBuilder
  , toLazyByteString
  , toLazyByteStringWith
  , toStrictByteString
  , hPutBuilder
  , hPutBuilderLen
  , hPutBuilderWith

  -- * Basic builders
  , primBounded
  , primFixed
  , primMapListBounded
  , primMapListFixed
  , byteString
  , byteStringThreshold
  , byteStringCopy
  , byteStringCopyNoCheck
  , byteStringInsert
  , unsafeCString
  , unsafeCStringLen
  , ensureBytes
  , getBytes

  -- * Performance tuning
  , rebuild
  ) where

import Control.Concurrent (forkIOWithUnmask, myThreadId)
import Control.Concurrent.MVar
import qualified Control.Exception as E
import Control.Monad
import qualified Data.ByteString as S
import qualified Data.ByteString.Internal as S
import qualified Data.ByteString.Unsafe as S
import qualified Data.ByteString.Lazy as L
import Data.IORef
import Data.Semigroup as Sem
import Data.String
import Data.Word
import Foreign.C.String
import Foreign.C.Types
import Foreign.ForeignPtr
import Foreign.ForeignPtr.Unsafe
import Foreign.Marshal.Utils
import Foreign.Ptr
import qualified System.IO as IO
import System.IO.Unsafe

import GHC.Exts (Addr#, State#, RealWorld, Ptr(..), Int(..), Int#)
import GHC.Magic (oneShot)
import GHC.IO (IO(..), unIO)
import GHC.CString (unpackCString#)

import qualified Data.ByteString.Builder.Prim as P
import qualified Data.ByteString.Builder.Prim.Internal as PI
import qualified Data.ByteString.Builder.Extra as X

-- | 'Builder' is an auxiliary type for efficiently generating a long
-- 'L.ByteString'. It is isomorphic to lazy 'L.ByteString', but offers
-- constant-time concatanation via '<>'.
--
-- Use 'toLazyByteString' to turn a 'Builder' into a 'L.ByteString'
newtype Builder = Builder
  { unBuilder :: DataSink -> BuilderState -> BuilderState
  }
  -- It takes and returns two pointers, "cur" and "end". "cur" points to
  -- the next location to put bytes to, and "end" points to the end of the
  -- buffer.

-- | The state of a builder. The components are:
--
-- * The "cur" pointer
-- * The "end" pointer
-- * The state token
type BuilderState = (# Addr#, Addr#, State# RealWorld #)

instance Sem.Semigroup Builder where
  (<>) = appendBuilder
  {-# INLINE (<>) #-}

appendBuilder :: Builder -> Builder -> Builder
appendBuilder (Builder a) (Builder b)
  = rebuild $ Builder $ \dex bs -> b dex (a dex bs)
{-# INLINE[1] appendBuilder #-}

{-# RULES "appendBuilder/assoc"
  forall x y z.
    appendBuilder (appendBuilder x y) z = appendBuilder x (appendBuilder y z)
  #-}

instance Monoid Builder where
  mempty = Builder $ \_ bs -> bs
  {-# INLINE mempty #-}
  mappend = (<>)
  {-# INLINE mappend #-}
  mconcat = foldr mappend mempty
  {-# INLINE mconcat #-}

-- | 'fromString' = 'stringUtf8'
instance IsString Builder where
  fromString = builderFromString
  {-# INLINE fromString #-}

-- | Specifies where bytes generated by a builder go.
data DataSink
  = DynamicSink !(IORef DynamicSink)
    -- ^ The destination of data changes while the builder is running.
  | GrowingBuffer !(IORef (ForeignPtr Word8))
    -- ^ Bytes are accumulated in a contiguous buffer.
  | HandleSink !IO.Handle !Int{-next buffer size-} !(IORef Queue)
    -- ^ Bytes are first accumulated in the 'Queue', then flushed to the
    -- 'IO.Handle'.

-- | Variable-destination cases.
data DynamicSink
  = ThreadedSink !(MVar Request) !(MVar Response)
      -- ^ Bytes are sent to another thread.
  | BoundedGrowingBuffer {-# UNPACK #-} !(ForeignPtr Word8) !Int{-bound-}
      -- ^ Bytes are accumulated in a contiguous buffer until the
      -- size limit is reached. After that, the destination switches
      -- to a 'ThreadedSink'.

-- | A mutable buffer.
data Queue = Queue
  { queueBuffer :: !(ForeignPtr Word8)
  , queueStart :: !Int
    -- ^ Starting position.
  , queueTotal :: !Int
    -- ^ Bytes written to the handle so far.
  }
  -- TODO: this is not really needed

-- | A request from the driver thread to the builder thread.
data Request
  = Request {-# UNPACK #-} !(Ptr Word8) {-# UNPACK #-} !(Ptr Word8)

-- | A response from the builder thread to the driver thread.
data Response
  = Error E.SomeException
      -- ^ A synchronous exception was thrown by the builder
  | Done !(Ptr Word8)
      -- ^ The builder thread has completed.
  | MoreBuffer !(Ptr Word8) !Int
      -- ^ The builder thread has finished generating one chunk,
      -- and waits for another request with the specified minimum size.
  | InsertByteString !(Ptr Word8) !S.ByteString
      -- ^ The builder thread has partially filled the current chunk,
      -- and wants to emit the bytestring to be included in the final
      -- output.
  deriving (Show)

----------------------------------------------------------------
-- Internally used exceptions

-- | Used in the implementation of 'toLazyByteString'. This is an exception
-- thrown by the consumer thread to itself when it has finished filling the
-- first chunk of the output. After this, a thread will be forked, and the
-- execution of the builder will be resumed in the new thread, using
-- 'ThreadedSink'.
data ChunkOverflowException
  = ChunkOverflowException
      !S.ByteString !(MVar Request) !(MVar Response) !Int

instance Show ChunkOverflowException where
  show (ChunkOverflowException buf _ _ req) =
    "ChunkOverflowException " ++ show buf ++ " _ _ " ++ show req

instance E.Exception ChunkOverflowException

-- | Used in the implementation of 'toLazyByteString'. This is a message sent
-- from the consumer thread to the builder thread, requesting the builder
-- thread to temporarily pause execution. Later, the consumer thread may
-- request resumption by filling the 'MVar'.
data SuspendBuilderException = SuspendBuilderException !(MVar ())

instance Show SuspendBuilderException where
  show _ = "SuspendBuilderException"

instance E.Exception SuspendBuilderException

----------------------------------------------------------------
-- Builder building blocks

-- | An internal type for making it easier to define builders. A value of
-- @'BuildM' a@ can do everything a 'Builder' can do, and in addition,
-- returns a value of type @a@ upon completion.
newtype BuildM a = BuildM { runBuildM :: (a -> Builder) -> Builder }
  deriving (Functor)

instance Applicative BuildM where
  pure x = BuildM $ \k -> k x
  {-# INLINE pure #-}
  (<*>) = ap

instance Monad BuildM where
  BuildM b >>= f = BuildM $ \k -> b $ \r -> runBuildM (f r) k
  {-# INLINE (>>=) #-}

-- | Create a builder from a BuildM.
mkBuilder :: BuildM () -> Builder
mkBuilder (BuildM bb) = bb $ \_ -> mempty
{-# INLINE mkBuilder #-}

-- | Embed a builder in the BuildM context.
useBuilder :: Builder -> BuildM ()
useBuilder b = BuildM $ \k -> b <> k ()
{-# INLINE useBuilder #-}

-- | Get the 'DataSink'.
getSink :: BuildM DataSink
getSink = BuildM $ \k -> Builder $ \dex (# cur, end, s #) ->
  unBuilder (k dex) dex (# cur, end, s #)

-- | Get the current pointer.
getCur :: BuildM (Ptr Word8)
getCur = BuildM $ \k -> Builder $ \dex (# cur, end, s #) ->
  unBuilder (k (Ptr cur)) dex (# cur, end, s #)

-- | Get the end-of-buffer pointer.
getEnd :: BuildM (Ptr Word8)
getEnd = BuildM $ \k -> Builder $ \dex (# cur, end, s #) ->
  unBuilder (k (Ptr end)) dex (# cur, end, s #)

-- | Set the current pointer.
setCur :: Ptr Word8 -> BuildM ()
setCur (Ptr p) = BuildM $ \k -> Builder $ \dex (# _, end, s #) ->
  unBuilder (k ()) dex (# p, end, s #)

-- | Set the end-of-buffer pointer.
setEnd :: Ptr Word8 -> BuildM ()
setEnd (Ptr p) = BuildM $ \k -> Builder $ \dex (# cur, _, s #) ->
  unBuilder (k ()) dex (# cur, p, s #)

-- | Perform IO.
io :: IO a -> BuildM a
io (IO x) = BuildM $ \k -> Builder $ \dex (# cur, end, s #) -> case x s of
  (# s', val #) -> unBuilder (k val) dex (# cur, end, s' #)

-- | Embed a 'BuilderState' transformer into `BuildM`.
updateState :: (BuilderState -> BuilderState) -> BuildM ()
updateState f = BuildM $ \k -> Builder $ \sink bs ->
  unBuilder (k ()) sink (f bs)

-- | A 'Write' is like a 'Builder', but an upper bound of its size is known
-- before it actually starts filling buffers. It means just one overflow check
-- is sufficient for each 'Write'.
data Write = Write !Int (BuilderState -> BuilderState)

instance Sem.Semigroup Write where
  Write s0 w0 <> Write s1 w1 = Write (s0 + s1) (\s -> w1 (w0 s))

instance Monoid Write where
  mempty = Write 0 (\s -> s)
  mappend = (<>)
  {-# INLINE mappend #-}

-- | Turn a 'PI.BoundedPrim' into a 'Write'.
writeBoundedPrim :: PI.BoundedPrim a -> a -> Write
writeBoundedPrim prim x =
  Write (PI.sizeBound prim) $ \(# cur, end, s #) ->
    case unIO (PI.runB prim x (Ptr cur)) s of
      (# s', Ptr cur' #) -> (# cur', end, s' #)

----------------------------------------------------------------
--
-- Running builders.

-- | Run a builder.
runBuilder :: Builder -> DataSink -> Ptr Word8 -> Ptr Word8 -> IO (Ptr Word8)
runBuilder (Builder f) sink (Ptr cur) (Ptr end) = IO $ \s ->
  case f sink (# cur, end, s #) of
    (# cur', _, s' #) -> (# s', Ptr cur' #)

-- | Turn a 'Builder' into a lazy 'L.ByteString'.
--
-- __Performance hint__: when the resulting 'L.ByteString' does not fit
-- in one chunk, this function forks a thread. Due to this, the performance
-- degrades sharply if you use this function from a bound thread. Note in
-- particular that the main thread is a bound thread when you use @ghc
-- -threaded@.
--
-- To avoid this problem, do one of these:
--
-- * Make sure the resulting 'L.ByteString' is consumed in an unbound
--    thread. Consider using 'runInUnboundThread' for this.
-- * Use other function to run the 'Builder' instead. Functions that don't
--    return a lazy 'L.ByteString' do not have this issue.
-- * Link your program without @-threaded@.
toLazyByteString :: Builder -> L.ByteString
toLazyByteString = toLazyByteStringWith 100 32768

-- | Like 'toLazyByteString', but allows the user to specify the initial
-- and the subsequent desired buffer sizes.
toLazyByteStringWith :: Int -> Int -> Builder -> L.ByteString

-- The implementation employs a two-phase strategy to minimize the overhead:
--
-- 0. Fill the first chunk in a single-threaded way. Start from 'initialSize'-
--    sized buffer and double the size whenever the buffer is full. This uses a
--    'BoundedGrowingBuffer' sink.
--
-- 1. If the first chunk is big enough and the builder still hasn't finished,
--    suspend the execution of the builder, fork a new thread and resume
--    execution of the builder in the new thread, using a 'ThreadedSink'.
toLazyByteStringWith !initialSize !maxSize builder = unsafePerformIO $ do
  fptr <- mallocForeignPtrBytes initialSize
  sink <- newIORef $ BoundedGrowingBuffer fptr maxSize
  let !base = unsafeForeignPtrToPtr fptr
  let
    finalPtr = unsafeDupablePerformIO $
      -- The use of unsafeDupablePerformIO is safe here, because at any given
      -- time, at most one thread can be attempting to evaluate this finalPtr
      -- thunk.
      runBuilder builder (DynamicSink sink) base (base `plusPtr` initialSize)
    {-# NOINLINE finalPtr #-}

    loop thunk = do
      -- Pass around `thunk` as an argument, otherwise GHC 7.10.1 inlines it
      -- despite the NOINLINE pragma.
      r <- E.try $ E.evaluate thunk
      case r of
        Right p -> do
          BoundedGrowingBuffer finalFptr _ <- readIORef sink
          let !finalBase = unsafeForeignPtrToPtr finalFptr
          return $! L.fromStrict $
            S.fromForeignPtr finalFptr 0 (p `minusPtr` finalBase)
        Left ex
          | Just (ChunkOverflowException chunk reqV respV minSize)
              <- E.fromException ex
            -> do
              let rest = continueBuilderThreaded reqV respV minSize maxSize thunk
              return $ L.fromChunks $
                if S.null chunk then rest else chunk : rest
          | otherwise -> do
              -- Here, there is no way to tell whether 'ex' is an asynchronous
              -- exception or not. We re-throw is as if it were async. This is
              -- a safe assumption, because if it is actually a synchronous
              -- exception, it will be re-thrown when we try to resume
              -- the evaluation of 'thunk'.
              myTid <- myThreadId
              E.throwTo myTid ex

              loop thunk

  loop finalPtr

-- | Continue a suspended builder using threads.
continueBuilderThreaded
  :: MVar Request -> MVar Response -> Int -> Int -> Ptr Word8
  -> [S.ByteString]
continueBuilderThreaded !reqV !respV !initialSize !maxSize thunk =
  makeChunks (max maxSize initialSize) maxSize $ toBufferWriter reqV respV thunk

-- | Run the given suspended builder using a new thread.
toBufferWriter :: MVar Request -> MVar Response -> Ptr Word8 -> X.BufferWriter
toBufferWriter !reqV !respV thunk buf0 sz0 = E.mask_ $
  writer Nothing buf0 sz0
  where
    writer !maybeBuilderTid !buf !sz = do
      putMVar reqV $ Request buf (buf `plusPtr` sz)
      -- Fork after putMVar, in order to minimize the chance that
      -- the new thread is scheduled on a different CPU.
      builderTid <- case maybeBuilderTid of
        Just t -> return t
        Nothing -> forkIOWithUnmask $ \u ->
          builderThreadWithUnmask u respV thunk
      resp <- wait builderTid
      let go cur next = return(written, next)
            where !written = cur `minusPtr` buf
      case resp of
        Error ex -> E.throwIO ex
        Done cur -> go cur X.Done
        MoreBuffer cur k -> go cur $ X.More k $ writer (Just builderTid)
        InsertByteString cur str -> go cur $ X.Chunk str $ writer (Just builderTid)

    wait !builderTid = do
      r <- E.try $ takeMVar respV
      case r of
        Right resp -> return resp
        Left exn -> do
          -- exn must be an async exception, because takeMVar throws no
          -- synchronous exceptions.
          resumeVar <- newEmptyMVar
          E.throwTo builderTid $ SuspendBuilderException resumeVar
          thisTid <- myThreadId
          E.throwTo thisTid (exn :: E.SomeException)

          -- A thunk containing this computation has been resumed.
          -- Resume the builder thread, and retry.
          putMVar resumeVar ()
          wait builderTid

-- | The body of the builder thread.
builderThreadWithUnmask
  :: (forall a. IO a -> IO a) -> MVar Response -> Ptr Word8
  -> IO ()
builderThreadWithUnmask unmask !respV thunk = loop
  where
    loop = do
      r <- E.try $ unmask $ E.evaluate thunk
      case r of
        Right p -> putMVar respV $ Done p
        Left ex
          | Just (SuspendBuilderException lock) <- E.fromException ex
            -> do takeMVar lock; loop
          | otherwise -> putMVar respV $ Error ex

-- | Run a 'X.BufferWriter'.
makeChunks :: Int -> Int -> X.BufferWriter -> [S.ByteString]
makeChunks !initialBufSize maxBufSize = go initialBufSize
  where
    go !bufSize w = unsafePerformIO $ do
      fptr <- S.mallocByteString bufSize
      (written, next) <- withForeignPtr fptr $ \buf -> w buf bufSize
      let rest = case next of
            X.Done -> []
            X.More reqSize w' -> go (max reqSize maxBufSize) w'
            X.Chunk chunk w' -> chunk : go maxBufSize w'
              -- TODO: don't throw away the remaining part of the buffer
      return $ if written == 0
        then rest
        else S.fromForeignPtr fptr 0 written : rest

-- | Turn a 'Builder' into a strict 'S.ByteString'.
toStrictByteString :: Builder -> S.ByteString
toStrictByteString builder = unsafePerformIO $ do
  let cap = 100
  fptr <- mallocForeignPtrBytes cap
  bufferRef <- newIORef fptr
  let !base = unsafeForeignPtrToPtr fptr
  cur <- runBuilder builder (GrowingBuffer bufferRef) base (base `plusPtr` cap)
  endFptr <- readIORef bufferRef
  let !written = cur `minusPtr` unsafeForeignPtrToPtr endFptr
  return $ S.fromForeignPtr endFptr 0 written

-- | Output a 'Builder' to a 'IO.Handle'.
hPutBuilder :: IO.Handle -> Builder -> IO ()
hPutBuilder !h builder = void $ hPutBuilderLen h builder
{-# INLINE hPutBuilder #-}

-- | Output a 'Builder' to a 'IO.Handle'. Returns the number of bytes written.
hPutBuilderLen :: IO.Handle -> Builder -> IO Int
hPutBuilderLen !h builder = hPutBuilderWith h 100 4096 builder

-- | Like 'hPutBuffer', but allows the user to specify the initial
-- and the subsequent desired buffer sizes. This function may be useful for
-- setting large buffer when high throughput I/O is needed.
hPutBuilderWith :: IO.Handle -> Int -> Int -> Builder -> IO Int
hPutBuilderWith !h !initialCap !nextCap builder = do
  fptr <- mallocForeignPtrBytes initialCap
  qRef <- newIORef $ Queue
    { queueBuffer = fptr
    , queueStart =  0
    , queueTotal = 0
    }
  let !base = unsafeForeignPtrToPtr fptr
  cur <- runBuilder builder (HandleSink h nextCap qRef)
    base (base `plusPtr` initialCap)
  flushQueue h qRef cur
  Queue{ queueTotal = len } <- readIORef qRef
  return len

----------------------------------------------------------------
-- builders

-- | Turn a 'String' into a 'Builder', using UTF-8,
builderFromString :: String -> Builder
builderFromString = primMapListBounded P.charUtf8
{-# NOINLINE[0] builderFromString #-}

{-# RULES "FastBuilder: builderFromString/unpackCString#"
  forall addr.
    builderFromString (unpackCString# addr) = unsafeCString (Ptr addr)
  #-}

-- | Turn a value of type @a@ into a 'Builder', using the given 'PI.BoundedPrim'.
primBounded :: PI.BoundedPrim a -> a -> Builder
primBounded prim = write . writeBoundedPrim prim
{-# INLINE primBounded #-}

-- | Turn a 'Write' into a 'Builder'.
write :: Write -> Builder
write (Write size w) = rebuild $ mkBuilder $ do
  useBuilder $ ensureBytes size
  updateState w
{-# INLINE[1] write #-}

{-# RULES "fast-builder: write/write"
  forall w0 w1.
    appendBuilder (write w0) (write w1) = write (w0 <> w1)
  #-}

{-# RULES "fast-builder: write/write/x"
  forall w0 w1 x.
    appendBuilder (write w0) (appendBuilder (write w1) x)
      = appendBuilder (write (w0 <> w1)) x
  #-}

-- | Turn a value of type @a@ into a 'Builder', using the given 'PI.FixedPrim'.
primFixed :: PI.FixedPrim a -> a -> Builder
primFixed prim = primBounded (PI.toB prim)
{-# INLINE primFixed #-}

-- | Turn a list of values of type @a@ into a 'Builder', using the given
-- 'PI.BoundedPrim'.
primMapListBounded :: PI.BoundedPrim a -> [a] -> Builder
primMapListBounded prim = mconcat . map (primBounded prim)
{-# INLINE primMapListBounded #-}

-- | Turn a list of values of type @a@ into a 'Builder', using the given
-- 'PI.FixedPrim'.
primMapListFixed :: PI.FixedPrim a -> [a] -> Builder
primMapListFixed prim = primMapListBounded (PI.toB prim)
{-# INLINE primMapListFixed #-}

-- | Turn a 'S.ByteString' to a 'Builder'.
byteString :: S.ByteString -> Builder
byteString = byteStringThreshold maximalCopySize
{-# INLINE byteString #-}

maximalCopySize :: Int
maximalCopySize = 2 * X.smallChunkSize

-- | Turn a 'S.ByteString' to a 'Builder'. If the size of the 'S.ByteString'
-- is larger than the given threshold, avoid copying it as much
-- as possible.
byteStringThreshold :: Int -> S.ByteString -> Builder
byteStringThreshold th bstr = rebuild $
  if S.length bstr >= th
    then byteStringInsert bstr
    else byteStringCopy bstr

-- | Turn a 'S.ByteString' to a 'Builder'. The 'S.ByteString' will be copied
-- to the buffer, regardless of the size.
byteStringCopy :: S.ByteString -> Builder
byteStringCopy !bstr =
  -- TODO: this is suboptimal; should keep using the same buffer size.
  ensureBytes (S.length bstr) <> byteStringCopyNoCheck bstr

-- | Like 'byteStringCopy', but assumes that the current buffer is large enough.
byteStringCopyNoCheck :: S.ByteString -> Builder
byteStringCopyNoCheck !bstr = mkBuilder $ do
  cur <- getCur
  io $ S.unsafeUseAsCString bstr $ \ptr ->
    copyBytes cur (castPtr ptr) len
  setCur $ cur `plusPtr` len
  where
    !len = S.length bstr

-- | Turn a 'S.ByteString' to a 'Builder'. When possible, the given
-- 'S.ByteString' will not be copied, and inserted directly into the output
-- instead.
byteStringInsert :: S.ByteString -> Builder
byteStringInsert !bstr = byteStringInsert_ bstr

-- | The body of the 'byteStringInsert', worker-wrappered manually.
byteStringInsert_ :: S.ByteString -> Builder
byteStringInsert_ bstr = mkBuilder $ do
  sink <- getSink
  case sink of
    DynamicSink dRef -> do
      dyn <- io $ readIORef dRef
      case dyn of
        ThreadedSink reqV respV -> do
          cur <- getCur
          io $ putMVar respV $ InsertByteString cur bstr
          handleRequest reqV
        BoundedGrowingBuffer fptr bound -> do
          r <- remainingBytes
          when (r < S.length bstr) $
            growBufferBounded dRef fptr bound (S.length bstr)
          -- TODO: insert rather than copy if the first chunk
          -- is full.
          useBuilder $ byteStringCopyNoCheck bstr
    GrowingBuffer bufRef -> do
      r <- remainingBytes
      when (r < S.length bstr) $
        growBuffer bufRef (S.length bstr)
      useBuilder $ byteStringCopyNoCheck bstr
    HandleSink h _nextCap queueRef -> do
      cur <- getCur
      io $ flushQueue h queueRef cur
      io $ S.hPut h bstr
      io $ modifyIORef' queueRef
        $ \q -> q { queueTotal = queueTotal q + S.length bstr }
{-# NOINLINE byteStringInsert_ #-}

-- | Turn a C String into a 'Builder'. The behavior is undefined if the given
-- 'CString' does not point to a constant null-terminated string.
unsafeCString :: CString -> Builder
unsafeCString cstr = rebuild $ let
    !len = fromIntegral $ c_pure_strlen cstr
  in unsafeCStringLen (cstr, len)

foreign import ccall unsafe "strlen" c_pure_strlen :: CString -> CSize

-- | Turn a 'CStringLen' into a 'Builder'. The behavior is undefined if the
-- given 'CStringLen' does not point to a constant memory block.
unsafeCStringLen :: CStringLen -> Builder
unsafeCStringLen (ptr, len) = mappend (ensureBytes len) $ mkBuilder $ do
  cur <- getCur
  io $ copyBytes cur (castPtr ptr) len
  setCur $ cur `plusPtr` len

-- | @'ensureBytes' n@ ensures that at least @n@ bytes of free space is
-- available in the current buffer, by allocating a new buffer when
-- necessary.
ensureBytes :: Int -> Builder
ensureBytes !n = mkBuilder $ do
  r <- remainingBytes
  when (r < n) $ useBuilder $ getBytes n
{-# INLINE ensureBytes #-}

-- | @'getBytes' n@ allocates a new buffer, containing at least @n@ bytes.
getBytes :: Int -> Builder
getBytes (I# n) = getBytes_ n

-- | The body of the 'getBytes' function, worker-wrappered manually.
getBytes_ :: Int# -> Builder
getBytes_ n = mkBuilder $ do
  sink <- getSink
  case sink of
    DynamicSink dRef -> do
      dyn <- io $ readIORef dRef
      case dyn of
        ThreadedSink reqV respV -> do
          cur <- getCur
          io $ putMVar respV $ MoreBuffer cur $ I# n
          handleRequest reqV
        BoundedGrowingBuffer fptr bound ->
          growBufferBounded dRef fptr bound (I# n)
    GrowingBuffer bufRef -> growBuffer bufRef (I# n)
    HandleSink h nextCap queueRef -> do
      cur <- getCur
      io $ flushQueue h queueRef cur
      switchQueue queueRef (max nextCap (I# n))
{-# NOINLINE getBytes_ #-}

-- | Return the remaining size of the current buffer, in bytes.
remainingBytes :: BuildM Int
remainingBytes = minusPtr <$> getEnd <*> getCur
{-# INLINE remainingBytes #-}

----------------------------------------------------------------
-- Performance tuning

-- | @'rebuild' b@ is equivalent to @b@, but it allows GHC to assume
-- that @b@ will be run at most once. This can enable various
-- optimizations that greately improve performance.
--
-- There are two types of typical situations where a use of 'rebuild'
-- is often a win:
--
-- * When constructing a builder using a recursive function. e.g.
--  @rebuild $ foldr ...@.
-- * When constructing a builder using a conditional expression. e.g.
--  @rebuild $ case x of ... @
rebuild :: Builder -> Builder
rebuild (Builder f) = Builder $ oneShot $ \dex -> oneShot $
  \(# cur, end, s #) -> f dex (# cur, end, s #)

----------------------------------------------------------------
-- ThreadedSink

-- | Wait for a request, and switch to a new buffer.
handleRequest :: MVar Request -> BuildM ()
handleRequest reqV = do
  Request newCur newEnd <- io $ takeMVar reqV
  setCur newCur
  setEnd newEnd

----------------------------------------------------------------
-- GrowingBuffer

-- | @growBuffer bufRef req@ reallocates the buffer, growing it
-- by at least @req@.
growBuffer :: IORef (ForeignPtr Word8) -> Int -> BuildM ()
growBuffer !bufRef !req = do
  cur <- getCur
  end <- getEnd
  fptr <- io $ readIORef bufRef
  let !base = unsafeForeignPtrToPtr fptr
  let !size = cur `minusPtr` base
  let !cap = end `minusPtr` base
  let !newCap = cap + max cap req
  newFptr <- io $ mallocForeignPtrBytes newCap
  let !newBase = unsafeForeignPtrToPtr newFptr
  setCur $ newBase `plusPtr` size
  setEnd $ newBase `plusPtr` newCap
  io $ do
    copyBytes newBase base size
    touchForeignPtr fptr
    touchForeignPtr newFptr
    writeIORef bufRef newFptr
{-# INLINE growBuffer #-}

----------------------------------------------------------------
-- HandleSink

-- | Put the content of the 'Queue' to the 'IO.Handle', and empty
-- the 'Queue'.
flushQueue :: IO.Handle -> IORef Queue -> Ptr Word8 -> IO ()
flushQueue !h !qRef !cur = do
  Queue{ queueBuffer = fptr, queueStart = start, queueTotal = total }
    <- readIORef qRef
  let !end = cur `minusPtr` unsafeForeignPtrToPtr fptr
  when (end > start) $ do
    S.hPut h $ S.fromForeignPtr fptr start (end - start)
    writeIORef qRef Queue
      { queueBuffer = fptr
      , queueStart = end
      , queueTotal = total + end - start
      }

-- | @switchQueue qRef minSize adv@ discards the old 'Queue' and sets up
-- a new empty 'Queue' of at least @minSize@ large. If the old 'Queue'
-- is large enough, it is re-used.
switchQueue :: IORef Queue -> Int -> BuildM ()
switchQueue !qRef !minSize = do
  end <- getCur
  Queue{ queueBuffer = fptr, queueTotal = total } <- io $ readIORef qRef
  let !base = unsafeForeignPtrToPtr fptr
  let !cap = end `minusPtr` base
  newFptr <- if minSize <= cap
    then return fptr
    else io $ mallocForeignPtrBytes minSize
  let !newBase = unsafeForeignPtrToPtr newFptr
  io $ writeIORef qRef Queue
    { queueBuffer = newFptr
    , queueStart = 0
    , queueTotal = total
    }
  setCur newBase
  setEnd $ newBase `plusPtr` max minSize cap

----------------------------------------------------------------
-- BoundedGrowingBuffer

-- | @growBufferBounded dRef fptr bound req@ reallocates the buffer, growing it
-- by at least @req@. If the buffer size would exceed @bound@, it instead
-- interrupts execution by throwing a 'ChunkOverflowException', and switches
-- to a 'ThreadedSink'.
growBufferBounded
  :: IORef DynamicSink -> ForeignPtr Word8 -> Int -> Int -> BuildM ()
growBufferBounded !dRef !fptr !bound !req = do
  cur <- getCur
  end <- getCur
  let !base = unsafeForeignPtrToPtr fptr
  let !size = cur `minusPtr` base
  let !cap = end `minusPtr` base
  let !newCap = cap + max cap req
  if bound < newCap
    then chunkOverflow dRef req $ S.fromForeignPtr fptr 0 size
    else do
      newFptr <- io $ mallocForeignPtrBytes newCap
      let !newBase = unsafeForeignPtrToPtr newFptr
      setCur $ newBase `plusPtr` size
      setEnd $ newBase `plusPtr` newCap
      io $ do
        copyBytes newBase base size
        touchForeignPtr fptr
        touchForeignPtr newFptr
        writeIORef dRef $ BoundedGrowingBuffer newFptr bound
{-# INLINE growBufferBounded #-}

-- | Throw a 'ChunkOverflowException' and switches to a 'ThreadedSink'.
chunkOverflow :: IORef DynamicSink -> Int -> S.ByteString -> BuildM ()
chunkOverflow !dRef !minSize !chunk = do
  myTid <- io myThreadId
  reqV <- io newEmptyMVar
  respV <- io newEmptyMVar
  io $ E.throwTo myTid $ ChunkOverflowException chunk reqV respV minSize
  io $ writeIORef dRef $ ThreadedSink reqV respV
  handleRequest reqV

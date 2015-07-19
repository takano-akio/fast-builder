{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE RankNTypes #-}

module Data.ByteString.FastBuilder.Base where

import Control.Concurrent (forkIOWithUnmask, myThreadId)
import Control.Concurrent.MVar
import qualified Control.Exception as E
import Control.Monad
import Data.Bits
import qualified Data.ByteString as S
import qualified Data.ByteString.Internal as S
import qualified Data.ByteString.Unsafe as S
import qualified Data.ByteString.Lazy as L
import Data.IORef
import Data.Monoid
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
import GHC.IO (IO(..))
import GHC.CString (unpackCString#)

import qualified Data.ByteString.Builder.Prim as P
import qualified Data.ByteString.Builder.Prim.Internal as PI
import qualified Data.ByteString.Builder.Extra as X

data DataSink
  = DynamicSink !(IORef DynamicSink)
  | GrowingBuffer !(IORef (ForeignPtr Word8))
  | HandleSink !IO.Handle !(IORef Queue)

data DynamicSink
  = ThreadedSink !(MVar Request) !(MVar Response)
  | BoundedGrowingBuffer !(ForeignPtr Word8) !Int{-bound-}

data Queue = Queue !(ForeignPtr Word8) !Int{-start-}

data Request
  = Request {-# UNPACK #-} !(Ptr Word8) {-# UNPACK #-} !(Ptr Word8)

data Response
  = Error E.SomeException
      -- ^ synchronous exception was thrown by the builder
  | Done !(Ptr Word8)
  | MoreBuffer !(Ptr Word8) !Int
  | InsertByteString !(Ptr Word8) !S.ByteString
  deriving (Show)

data BuilderArg = BuilderArg
  DataSink
  {-# UNPACK #-} !(Ptr Word8)
  {-# UNPACK #-} !(Ptr Word8)

newtype Builder = Builder { unBuilder :: BuilderArg -> State# RealWorld -> (# Addr#, Addr#, State# RealWorld #) }
type Builder_ = DataSink -> Addr# -> Addr# -> State# RealWorld -> (# Addr#, Addr#, State# RealWorld #)

toBuilder_ :: Builder -> Builder_
toBuilder_ (Builder f) dex cur end s = f (BuilderArg dex (Ptr cur) (Ptr end)) s

fromBuilder_ :: Builder_ -> Builder
fromBuilder_ f = Builder $ \(BuilderArg dex (Ptr cur) (Ptr end)) s -> f dex cur end s

newtype BuildM a = BuildM { runBuildM :: (a -> Builder) -> Builder }
  deriving (Functor)

instance Applicative BuildM where
  pure = return
  (<*>) = ap

instance Monad BuildM where
  return x = BuildM $ \k -> k x
  {-# INLINE return #-}
  BuildM b >>= f = BuildM $ \k -> b $ \r -> runBuildM (f r) k
  {-# INLINE (>>=) #-}

data SuspendBuilderException = SuspendBuilderException !(MVar ())

instance Show SuspendBuilderException where
  show _ = "SuspendBuilderException"

instance E.Exception SuspendBuilderException

runBuilderThreaded
  :: (forall a. IO a -> IO a) -> MVar Response -> Ptr Word8
  -> IO ()
runBuilderThreaded unmask !respV thunk = loop
  where
    loop = do
      r <- E.try $ unmask $ E.evaluate thunk
      case r of
        Right p -> putMVar respV $ Done p
        Left ex
          | Just (SuspendBuilderException lock) <- E.fromException ex
            -> do takeMVar lock; loop
          | otherwise -> putMVar respV $ Error ex

runBuilder :: Builder -> DataSink -> Ptr Word8 -> Ptr Word8 -> IO (Ptr Word8)
runBuilder (Builder f) sink !cur !end = IO $ \s ->
  case f (BuilderArg sink cur end) s of
    (# cur', _, s' #) -> (# s', Ptr cur' #)

mkBuilder :: BuildM () -> Builder
mkBuilder (BuildM bb) = bb $ \_ -> mempty
{-# INLINE mkBuilder #-}

useBuilder :: Builder -> BuildM ()
useBuilder b = BuildM $ \k -> b <> k ()
{-# INLINE useBuilder #-}

getSink :: BuildM DataSink
getSink = BuildM $ \k -> Builder $ \(BuilderArg dex cur end) s -> unBuilder (k dex) (BuilderArg dex cur end) s

getCur :: BuildM (Ptr Word8)
getCur = BuildM $ \k -> Builder $ \(BuilderArg dex cur end) s -> unBuilder (k cur) (BuilderArg dex cur end) s

getEnd :: BuildM (Ptr Word8)
getEnd = BuildM $ \k -> Builder $ \(BuilderArg dex cur end) s -> unBuilder (k end) (BuilderArg dex cur end) s

setCur :: Ptr Word8 -> BuildM ()
setCur p = BuildM $ \k -> Builder $ \(BuilderArg dex _ end) s -> unBuilder (k ()) (BuilderArg dex p end) s

setEnd :: Ptr Word8 -> BuildM ()
setEnd p = BuildM $ \k -> Builder $ \(BuilderArg dex cur _) s -> unBuilder (k ()) (BuilderArg dex cur p) s

io :: IO a -> BuildM a
io (IO x) = BuildM $ \k -> Builder $ \ba s -> case x s of
  (# s', val #) -> unBuilder (k val) ba s'

instance Monoid Builder where
  mempty = Builder $ \(BuilderArg _ (Ptr cur) (Ptr end)) s -> (# cur, end, s #)
  {-# INLINE mempty #-}
  mappend (Builder a) (Builder b) = Builder $ \(BuilderArg dex (Ptr cur) (Ptr end)) s ->
    case a (BuilderArg dex (Ptr cur) (Ptr end)) s of
      (# cur', end', s' #) -> b (BuilderArg dex (Ptr cur') (Ptr end')) s'
  {-# INLINE mappend #-}
  mconcat xs = foldr mappend mempty xs
  {-# INLINE mconcat #-}

instance IsString Builder where
  fromString = builderFromString
  {-# INLINE fromString #-}

builderFromString :: String -> Builder
builderFromString = primMapListBounded P.charUtf8
{-# NOINLINE[0] builderFromString #-}

{-# RULES "FastBuilder: builderFromString/unpackCString#"
  forall addr.
    builderFromString (unpackCString# addr) = unsafeCString (Ptr addr)
  #-}

rebuild :: Builder -> Builder
rebuild (Builder f) = Builder $ oneShot (\ !arg s -> f arg s)

toBufferWriter :: MVar Request -> MVar Response -> Ptr Word8 -> X.BufferWriter
toBufferWriter !reqV !respV thunk buf0 sz0 = E.mask_ $ do
  writer Nothing buf0 sz0
  where
    writer !maybeBuilderTid !buf !sz = do
      putMVar reqV $ Request buf (buf `plusPtr` sz)
      -- Fork after putMVar, in order to minimize the chance that
      -- the new thread is scheduled on a different CPU.
      builderTid <- case maybeBuilderTid of
        Just t -> return t
        Nothing -> forkIOWithUnmask $ \u ->
          runBuilderThreaded u respV thunk
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

toLazyByteString :: Builder -> L.ByteString
toLazyByteString = toLazyByteStringWith 1024 262114

-- TODO: why is this slower than toStrictByteString for small builders?
toLazyByteStringWith :: Int -> Int -> Builder -> L.ByteString
toLazyByteStringWith !initialSize !maxSize builder = unsafePerformIO $ do
  fptr <- mallocForeignPtrBytes initialSize
  sink <- newIORef $ BoundedGrowingBuffer fptr maxSize
  let !base = unsafeForeignPtrToPtr fptr
  let
    finalPtr = unsafePerformIO $
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

continueBuilderThreaded
  :: MVar Request -> MVar Response -> Int -> Int -> Ptr Word8
  -> [S.ByteString]
continueBuilderThreaded !reqV !respV !initialSize !maxSize thunk =
  makeChunks initialSize maxSize $ toBufferWriter reqV respV thunk

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

hPutBuilder :: IO.Handle -> Builder -> IO ()
hPutBuilder !h builder = do
  let cap = 100
  fptr <- mallocForeignPtrBytes cap
  qRef <- newIORef $ Queue fptr 0
  let !base = unsafeForeignPtrToPtr fptr
  cur <- runBuilder builder (HandleSink h qRef) base (base `plusPtr` cap)
  flushQueue h qRef cur

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

----------------------------------------------------------------
-- builders

primBounded :: PI.BoundedPrim a -> a -> Builder
primBounded prim !x = mappend (ensureBytes $ PI.sizeBound prim) $ mkBuilder $ do
  cur <- getCur
  cur' <- io $ PI.runB prim x cur
  setCur cur'
{-# INLINE primBounded #-}

primFixed :: PI.FixedPrim a -> a -> Builder
primFixed prim x = primBounded (PI.toB prim) x
{-# INLINE primFixed #-}

primMapListBounded :: PI.BoundedPrim a -> [a] -> Builder
primMapListBounded prim = \xs -> mconcat $ map (primBounded prim) xs
{-# INLINE primMapListBounded #-}

primMapListFixed :: PI.FixedPrim a -> [a] -> Builder
primMapListFixed prim = \xs -> primMapListBounded (PI.toB prim) xs
{-# INLINE primMapListFixed #-}

byteString :: S.ByteString -> Builder
byteString = byteStringThreshold maximalCopySize
{-# INLINE byteString #-}

maximalCopySize :: Int
maximalCopySize = 2 * X.smallChunkSize

byteStringThreshold :: Int -> S.ByteString -> Builder
byteStringThreshold th bstr = rebuild $
  if S.length bstr >= th
    then byteStringInsert bstr
    else byteStringCopy bstr

byteStringCopy :: S.ByteString -> Builder
byteStringCopy !bstr =
  -- TODO: this is suboptimal; should keep using the same buffer size.
  ensureBytes (S.length bstr) <> byteStringCopyNoCheck bstr

byteStringCopyNoCheck :: S.ByteString -> Builder
byteStringCopyNoCheck !bstr = mkBuilder $ do
  cur <- getCur
  io $ S.unsafeUseAsCString bstr $ \ptr ->
    copyBytes cur (castPtr ptr) len
  setCur $ cur `plusPtr` len
  where
    !len = S.length bstr

byteStringInsert :: S.ByteString -> Builder
byteStringInsert !bstr = fromBuilder_ $ byteStringInsert_ bstr

byteStringInsert_ :: S.ByteString -> Builder_
byteStringInsert_ bstr = toBuilder_ $ mkBuilder $ do
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
          -- TODO: don't grow if bstr fits in the current buffer
          growBufferBounded dRef fptr bound (S.length bstr)
          useBuilder $ byteStringCopyNoCheck bstr
    GrowingBuffer bufRef -> do
      -- TODO: don't grow if bstr fits in the current buffer
      growBuffer bufRef (S.length bstr +)
      useBuilder $ byteStringCopyNoCheck bstr
    HandleSink h queueRef -> do
      cur <- getCur
      io $ flushQueue h queueRef cur
      io $ S.hPut h bstr
{-# NOINLINE byteStringInsert_ #-}

unsafeCString :: CString -> Builder
unsafeCString cstr = rebuild $ let
    !len = fromIntegral $ c_pure_strlen cstr
  in
  mappend (ensureBytes len) $ mkBuilder $ do
  cur <- getCur
  io $ copyBytes cur (castPtr cstr) len
  setCur $ cur `plusPtr` len

foreign import ccall unsafe "strlen" c_pure_strlen :: CString -> CSize

getBytes :: Int -> Builder
getBytes (I# n) = fromBuilder_ (getBytes_ n)

getBytes_ :: Int# -> Builder_
getBytes_ n = toBuilder_ $ mkBuilder $ do
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
    GrowingBuffer bufRef -> growBuffer bufRef (`shiftL` 1) -- TODO: check n
    HandleSink h queueRef -> do
      cur <- getCur
      io $ flushQueue h queueRef cur
      emptyQueue queueRef $ max 4096 (I# n)
{-# NOINLINE getBytes_ #-}

ensureBytes :: Int -> Builder
ensureBytes !n = mkBuilder $ do
  cur <- getCur
  end <- getEnd
  when (cur `plusPtr` n >= end) $ useBuilder $ getBytes n
{-# INLINE ensureBytes #-}

----------------------------------------------------------------
-- ThreadedSink

handleRequest :: MVar Request -> BuildM ()
handleRequest reqV = do
  Request newCur newEnd <- io $ takeMVar reqV
  setCur newCur
  setEnd newEnd

----------------------------------------------------------------
-- GrowingBuffer

growBuffer :: IORef (ForeignPtr Word8) -> (Int -> Int) -> BuildM ()
growBuffer !bufRef capFn = do
  cur <- getCur
  end <- getCur
  fptr <- io $ readIORef bufRef
  let !base = unsafeForeignPtrToPtr fptr
  let !size = cur `minusPtr` base
  let !cap = end `minusPtr` base
  let !newCap = capFn cap
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

flushQueue :: IO.Handle -> IORef Queue -> Ptr Word8 -> IO ()
flushQueue !h !qRef !cur = do
  Queue fptr start <- readIORef qRef
  let !end = cur `minusPtr` unsafeForeignPtrToPtr fptr
  when (end > start) $ do
    S.hPut h $ S.fromForeignPtr fptr start (end - start)
    writeIORef qRef $ Queue fptr end

emptyQueue :: IORef Queue -> Int -> BuildM ()
emptyQueue !qRef !minSize = do
  end <- getCur
  Queue fptr _ <- io $ readIORef qRef
  let !base = unsafeForeignPtrToPtr fptr
  let !cap = end `minusPtr` base
  newFptr <- if minSize <= cap
    then return fptr
    else io $ mallocForeignPtrBytes minSize
  let !newBase = unsafeForeignPtrToPtr newFptr
  io $ writeIORef qRef $ Queue newFptr 0
  setCur newBase
  setEnd $ newBase `plusPtr` max minSize cap

----------------------------------------------------------------
-- BoundedGrowingBuffer

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

data ChunkOverflowException
  = ChunkOverflowException
      !S.ByteString !(MVar Request) !(MVar Response) !Int

instance Show ChunkOverflowException where
  show (ChunkOverflowException buf _ _ req) =
    "ChunkOverflowException " ++ show buf ++ " _ _ " ++ show req

instance E.Exception ChunkOverflowException

chunkOverflow :: IORef DynamicSink -> Int -> S.ByteString -> BuildM ()
chunkOverflow !dRef !minSize !chunk = do
  myTid <- io $ myThreadId
  reqV <- io $ newEmptyMVar
  respV <- io $ newEmptyMVar
  io $ E.throwTo myTid $ ChunkOverflowException chunk reqV respV minSize
  io $ writeIORef dRef $ ThreadedSink reqV respV
  handleRequest reqV

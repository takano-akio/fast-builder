{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveFunctor #-}

module Data.ByteString.FastBuilder.Base where

import Control.Concurrent (forkIO)
import Control.Concurrent.MVar
import qualified Control.Exception as E
import Control.Monad
import qualified Data.ByteString as S
import qualified Data.ByteString.Internal as S
import qualified Data.ByteString.Unsafe as S
import qualified Data.ByteString.Lazy as L
import Data.Monoid
import Data.String
import Data.Word
import Foreign.C.String
import Foreign.C.Types
import Foreign.ForeignPtr
import Foreign.Marshal.Utils
import Foreign.Ptr
import System.IO.Unsafe

import GHC.Exts (Addr#, State#, RealWorld, Ptr(..), Int(..), Int#)
import GHC.Magic (oneShot)
import GHC.IO (IO(..))
import GHC.CString (unpackCString#)

import qualified Data.ByteString.Builder.Prim as P
import qualified Data.ByteString.Builder.Prim.Internal as PI
import qualified Data.ByteString.Builder.Extra as X

data DataExchange = Dex
  {-# UNPACK #-} !(MVar Request)
  {-# UNPACK #-} !(MVar Response)

data Request
  = Request {-# UNPACK #-} !(Ptr Word8) {-# UNPACK #-} !(Ptr Word8)

data Response
  = Error E.SomeException
      -- ^ synchronous exception was thrown by the builder
  | Done !(Ptr Word8)
  | MoreBuffer !(Ptr Word8) !Int
  | InsertByteString !(Ptr Word8) !S.ByteString

data BuilderArg = BuilderArg
  DataExchange
  {-# UNPACK #-} !(Ptr Word8)
  {-# UNPACK #-} !(Ptr Word8)

newtype Builder = Builder { unBuilder :: BuilderArg -> State# RealWorld -> (# Addr#, Addr#, State# RealWorld #) }
type Builder_ = DataExchange -> Addr# -> Addr# -> State# RealWorld -> (# Addr#, Addr#, State# RealWorld #)

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

runBuilder :: DataExchange -> Builder -> IO ()
runBuilder dex@(Dex reqV respV) (Builder f) = do
  Request cur end <- takeMVar reqV
  do
    cur' <- IO $ \s -> case f (BuilderArg dex cur end) s of
      (# cur', _, s' #) -> (# s', Ptr cur' #)
    putMVar respV $ Done cur'
    `E.catch` \e -> putMVar respV $ Error e

mkBuilder :: BuildM () -> Builder
mkBuilder (BuildM bb) = bb $ \_ -> mempty
{-# INLINE mkBuilder #-}

useBuilder :: Builder -> BuildM ()
useBuilder b = BuildM $ \k -> b <> k ()
{-# INLINE useBuilder #-}

getDex :: BuildM DataExchange
getDex = BuildM $ \k -> Builder $ \(BuilderArg dex cur end) s -> unBuilder (k dex) (BuilderArg dex cur end) s

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

handleRequest :: DataExchange -> BuildM ()
handleRequest (Dex reqV _) = do
  Request newCur newEnd <- io $ takeMVar reqV
  setCur newCur
  setEnd newEnd

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

toBufferWriter :: Builder -> X.BufferWriter
toBufferWriter b buf0 sz0 = do
  dex <- newDex
  startBuilderThread dex b
  writer dex buf0 sz0
  where
    writer dex@(Dex reqV respV) buf sz = do
      putMVar reqV $ Request buf (buf `plusPtr` sz)
      -- TODO: handle async exceptions
      resp <- takeMVar respV
      let go cur next = return (written, next)
            where !written = cur `minusPtr` buf
      case resp of
        Error ex -> E.throwIO ex
        Done cur -> go cur X.Done
        MoreBuffer cur k -> go cur $ X.More k $ writer dex
        InsertByteString cur str -> go cur $ X.Chunk str $ writer dex

newDex :: IO DataExchange
newDex = Dex <$> newEmptyMVar <*> newEmptyMVar

startBuilderThread :: DataExchange -> Builder -> IO ()
startBuilderThread dex b = void $ forkIO $ runBuilder dex b

toLazyByteString :: Builder -> L.ByteString
toLazyByteString = toLazyByteStringWith 1024 (min 262114 . (*2))

toLazyByteStringWith :: Int -> (Int -> Int) -> Builder -> L.ByteString
toLazyByteStringWith !initialBufSize nextBufSize =
  L.fromChunks . makeChunks initialBufSize nextBufSize . toBufferWriter

makeChunks :: Int -> (Int -> Int) -> X.BufferWriter -> [S.ByteString]
makeChunks !initialBufSize nextBufSize = go initialBufSize
  where
    go !bufSize w = unsafePerformIO $ do
      fptr <- S.mallocByteString bufSize
      (written, next) <- withForeignPtr fptr $ \buf -> w buf bufSize
      let !nextSize = nextBufSize bufSize
      let rest = case next of
            X.Done -> []
            X.More reqSize w' -> go (max reqSize nextSize) w'
            X.Chunk chunk w' -> chunk : go nextSize w'
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
byteStringCopy bstr = mkBuilder $ do
  useBuilder $ ensureBytes (S.length bstr)
  cur <- getCur
  io $ S.unsafeUseAsCString bstr $ \ptr ->
    copyBytes cur (castPtr ptr) len
  setCur $ cur `plusPtr` len
  where
    !len = S.length bstr

byteStringInsert :: S.ByteString -> Builder
byteStringInsert bstr = mkBuilder $ do
  dex@(Dex _ respV) <- getDex
  cur <- getCur
  io $ putMVar respV $ InsertByteString cur bstr
  handleRequest dex

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
  dex@(Dex _ respV) <- getDex
  cur <- getCur
  io $ putMVar respV $ MoreBuffer cur $ I# n
  handleRequest dex
{-# NOINLINE getBytes_ #-}

ensureBytes :: Int -> Builder
ensureBytes !n = mkBuilder $ do
  cur <- getCur
  end <- getEnd
  when (cur `plusPtr` n >= end) $ useBuilder $ getBytes n
{-# INLINE ensureBytes #-}

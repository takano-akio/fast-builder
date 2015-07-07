{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveFunctor #-}

module Data.ByteString.FastBuilder.Base where

import Control.Applicative
import Control.Concurrent (forkIO)
import Control.Concurrent.MVar
import Control.Monad
import qualified Data.ByteString as S
import qualified Data.ByteString.Internal as S
import qualified Data.ByteString.Unsafe as S
import qualified Data.ByteString.Lazy as L
import Data.Monoid
import Data.String
import Data.Word
import Foreign.ForeignPtr
import Foreign.Marshal.Utils
import Foreign.Ptr
import System.IO.Unsafe

import GHC.Exts (Addr#, State#, RealWorld, Ptr(..), Int(..), Int#)
import GHC.Exts (realWorld#)
import GHC.IO (IO(..), unIO)

import qualified Data.ByteString.Builder.Prim as P
import qualified Data.ByteString.Builder.Prim.Internal as PI
import qualified Data.ByteString.Builder.Extra as X

data DataExchange = Dex
  {-# UNPACK #-} !(MVar Request)
  {-# UNPACK #-} !(MVar Response)

data Request
  = Request {-# UNPACK #-} !(Ptr Word8) {-# UNPACK #-} !(Ptr Word8)

data Response = Response {-# UNPACK #-} !(Ptr Word8) !More
data More
  = NoMore
  | MoreBuffer {-# UNPACK #-} !Int
  | InsertByteString !S.ByteString
data BuilderArg = BuilderArg
  !DataExchange
  {-# UNPACK #-} !(Ptr Word8)
  {-# UNPACK #-} !(Ptr Word8)

newtype Builder = Builder
  ( BuilderArg
  -> State# RealWorld
  -> (# Addr#, Addr#, State# RealWorld #)
  )

newtype Build a = Build (DataExchange -> Ptr Word8 -> Ptr Word8 -> IO (Ptr Word8, Ptr Word8, a))
  deriving (Functor)

instance Applicative Build where
  pure = return
  (<*>) = ap

instance Monad Build where
  return x = Build $ \_ cur end -> return (cur, end, x)
  {-# INLINE return #-}
  Build b >>= f = Build $ \dex cur end -> do
    (cur', end', x) <- b dex cur end
    case f x of
      Build c -> c dex cur' end'
  {-# INLINE (>>=) #-}

runBuild :: DataExchange -> Build a -> IO a
runBuild dex@(Dex reqV respV) (Build f) = do
  Request cur end <- takeMVar reqV
  (cur', _, x) <- f dex cur end
  putMVar respV $ Response cur' NoMore
  return x

mkBuilder :: Build () -> Builder
mkBuilder (Build bb) = Builder $ \(BuilderArg dex cur end) s ->
  let !(# s1, (Ptr cur', Ptr end', _) #) = unIO (bb dex cur end) s
  in (# cur', end', s1 #)
{-# INLINE mkBuilder #-}

runBuilder :: Builder -> Build ()
runBuilder (Builder b) = Build $ \dex cur end -> IO $ \s ->
  let !(# cur', end', s1 #) = b (BuilderArg dex cur end) s
  in (# s1, (Ptr cur', Ptr end', ()) #)
{-# INLINE runBuilder #-}

getDex :: Build DataExchange
getDex = Build $ \dex cur end -> return (cur, end, dex)

getCur :: Build (Ptr Word8)
getCur = Build $ \_ cur end -> return (cur, end, cur)

getEnd :: Build (Ptr Word8)
getEnd = Build $ \_ cur end -> return (cur, end, end)

setCur :: Ptr Word8 -> Build ()
setCur p = Build $ \_ _ end -> return (p, end, ())

setEnd :: Ptr Word8 -> Build ()
setEnd p = Build $ \_ cur _ -> return (cur, p, ())

io :: IO a -> Build a
io x = Build $ \_ cur end -> do
  v <- x
  return (cur, end, v)

handleRequest :: DataExchange -> Build ()
handleRequest (Dex reqV _) = do
  Request newCur newEnd <- io $ takeMVar reqV
  setCur newCur
  setEnd newEnd

instance Monoid Builder where
  mempty = mkBuilder $ return ()
  {-# INLINE mempty #-}
  mappend x y = mkBuilder $ runBuilder x >> runBuilder y
  {-# INLINE mappend #-}
  mconcat xs = foldr mappend mempty xs
  {-# INLINE mconcat #-}

instance IsString Builder where
  fromString = primMapListBounded P.charUtf8

rebuild :: (State# RealWorld -> Builder) -> Builder
rebuild f = Builder $ \arg s ->
  let Builder g = f realWorld#
  in g arg s

toBufferWriter :: Builder -> X.BufferWriter
toBufferWriter b buf0 sz0 = do
  dex <- newDex
  startBuilderThread dex b
  writer dex buf0 sz0
  where
    writer dex@(Dex reqV respV) buf sz = do
      putMVar reqV $ Request buf (buf `plusPtr` sz)
      -- TODO: handle async exceptions
      Response cur more <- takeMVar respV
      let !written = cur `minusPtr` buf
      return $ (,) written $ case more of
        NoMore -> X.Done
        MoreBuffer k -> X.More k $ writer dex
        InsertByteString str -> X.Chunk str $ writer dex

newDex :: IO DataExchange
newDex = Dex <$> newEmptyMVar <*> newEmptyMVar

startBuilderThread :: DataExchange -> Builder -> IO ()
startBuilderThread dex b = void $ forkIO $ runBuild dex $ runBuilder b

toLazyByteString :: Builder -> L.ByteString
toLazyByteString = L.fromChunks . makeChunks defaultBufferSize . toBufferWriter
  where
    defaultBufferSize = 4096
    makeChunks bufSize w = unsafePerformIO $ do
      fptr <- S.mallocByteString bufSize
      (written, next) <- withForeignPtr fptr $ \buf -> w buf bufSize
      let rest = case next of
            X.Done -> []
            X.More reqSize w' -> makeChunks (max reqSize defaultBufferSize) w'
            X.Chunk chunk w' -> chunk : makeChunks bufSize w'
              -- TODO: shouldn't be throwing away the unused part of the buffer
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
byteStringThreshold th bstr = rebuild $ \_ ->
  if S.length bstr >= th
    then byteStringInsert bstr
    else byteStringCopy bstr

byteStringCopy :: S.ByteString -> Builder
byteStringCopy bstr = mkBuilder $ do
  runBuilder $ ensureBytes (S.length bstr)
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
  io $ putMVar respV $ Response cur $ InsertByteString $ bstr
  handleRequest dex

getBytes :: Int# -> Builder
getBytes n = mkBuilder $ do
  dex@(Dex _ respV) <- getDex
  cur <- getCur
  io $ putMVar respV $ Response cur $ MoreBuffer $ I# n
  handleRequest dex
{-# NOINLINE getBytes #-}

ensureBytes :: Int -> Builder
ensureBytes n@(I# n#) = mkBuilder $ do
  cur <- getCur
  end <- getEnd
  when (cur `plusPtr` n >= end) $ runBuilder $ getBytes n#
{-# INLINE ensureBytes #-}

{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveFunctor #-}
module Data.ByteString.FastBuilder where

import Control.Concurrent.MVar
import qualified Data.ByteString as S
import Data.Monoid
import Data.Word
import Foreign.Ptr

import GHC.Exts (Addr#, State#, RealWorld, Ptr(..))
import GHC.IO (IO(..), unIO)

data DataExchange = Dex
  {-# UNPACK #-} !(MVar Request)
  {-# UNPACK #-} !(MVar Response)

data Request
  = Request {-# UNPACK #-} !(Ptr Word8) {-# UNPACK #-} !(Ptr Word8)

data Response = Response {-# UNPACK #-} !(Ptr Word8) !More
data More
  = NoDone
  | MoreBuffer {-# UNPACK #-} !Int
  | InsertByteString !S.ByteString

newtype Builder = Builder
  ( DataExchange
  -> Addr#
  -> Addr#
  -> State# RealWorld
  -> (# Addr#, Addr#, State# RealWorld #)
  )

newtype Build a = Build (DataExchange -> Ptr Word8 -> Ptr Word8 -> IO (Ptr Word8, Ptr Word8, a))
  deriving (Functor)

instance Monad Build where
  return x = Build $ \_ cur end -> return (cur, end, x)
  Build b >>= f = Build $ \dex cur end -> do
    (cur', end', x) <- b dex cur end
    case f x of
      Build c -> c dex cur' end'

mkBuilder :: Build () -> Builder
mkBuilder (Build bb) = Builder $ \dex cur end s -> 
  let !(# s1, (Ptr cur', Ptr end', _) #) = unIO (bb dex (Ptr cur) (Ptr end)) s
  in (# cur', end', s1 #)
{-# INLINE mkBuilder #-}

runBuilder :: Builder -> Build ()
runBuilder (Builder b) = Build $ \dex (Ptr cur) (Ptr end) -> IO $ \s ->
  let !(# cur', end', s1 #) = b dex cur end s
  in (# s1, (Ptr cur', Ptr end', ()) #)
{-# INLINE runBuilder #-}

getDex :: Build DataExchange
getDex = Build $ \dex cur end -> return (cur, end, dex)

getCur :: Build (Ptr Word8)
getCur = Build $ \_ cur end -> return (cur, end, cur)

setCur :: Ptr Word8 -> Build ()
setCur p = Build $ \_ _ end -> return (p, end, ())

setEnd :: Ptr Word8 -> Build ()
setEnd p = Build $ \_ cur _ -> return (cur, p, ())

io :: IO a -> Build a
io x = Build $ \_ cur end -> do
  v <- x
  return (cur, end, v)

getBytes :: Int -> Builder
getBytes !n = mkBuilder $ do
  dex@(Dex _ respV) <- getDex
  cur <- getCur
  io $ putMVar respV $ Response cur $ MoreBuffer n
  handleRequest dex

handleRequest :: DataExchange -> Build ()
handleRequest (Dex reqV _) = do
  Request newCur newEnd <- io $ takeMVar reqV
  setCur newCur
  setEnd newEnd

instance Monoid Builder where
  mempty = mkBuilder $ return ()
  mappend x y = mkBuilder $ runBuilder x >> runBuilder y

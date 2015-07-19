{-# LANGUAGE DeriveFunctor #-}
import Control.Concurrent
import Control.Monad
import Criterion.Main
import Data.Monoid
import qualified Data.Vector.Unboxed as U
import Data.Word
import qualified System.IO as IO

import qualified Data.ByteString.Builder as Bstr
import qualified Data.ByteString.FastBuilder as Fast

main :: IO ()
main = runInUnboundThread $ do
  h <- IO.openFile "/dev/null" IO.WriteMode
  let
    size sz vec = bgroup sz
      [ bench "lazy/fast" $ nf (Fast.toLazyByteString . fastVector) vec
      , bench "lazy/bstr" $ nf (Bstr.toLazyByteString . bstrVector) vec
      , bench "strict/fast" $ nf (Fast.toStrictByteString . fastVector) vec
      , bench "io/fast" $ whnfIO $ Fast.hPutBuilder h $ fastVector vec
      , bench "io/bstr" $ whnfIO $ Bstr.hPutBuilder h $ bstrVector vec
      ]
  vec10 `seq` vec100 `seq` vec1000 `seq` vec10000 `seq` defaultMain
    [ size "10" vec10
    , size "100" vec100
    , size "1000" vec1000
    , size "10000" vec10000
    ]

type Item = (Bool, Word32)

fastVector :: U.Vector Item -> Fast.Builder
fastVector = Fast.rebuild . my_foldr step mempty
  where
    step (b, w) rest = Fast.word8 (if b then 1 else 0) <> Fast.word32LE w <> rest

bstrVector :: U.Vector Item -> Bstr.Builder
bstrVector = my_foldr step mempty
  where
    step (b, w) rest = Bstr.word8 (if b then 1 else 0) <> Bstr.word32LE w <> rest

vec10 :: U.Vector Item
vec10 = makeItems 10

vec100 :: U.Vector Item
vec100 = makeItems 100

vec1000 :: U.Vector Item
vec1000 = makeItems 1000

vec10000 :: U.Vector Item
vec10000 = makeItems 10000

makeItems :: Int -> U.Vector Item
makeItems n = U.generate n $ \i ->
  (mod i 3 == 0, fromIntegral i + 100)

data Box a = Box a
  deriving Functor
instance Applicative Box where
  pure = return
  (<*>) = ap
instance Monad Box where
  return = Box
  Box a >>= f = f a

my_foldr :: (U.Unbox a) => (a -> r -> r) -> r -> U.Vector a -> r
my_foldr f z = go
  where
    go v
      | U.null v = z
      | otherwise = f (U.unsafeHead v) $ go (U.unsafeTail v)

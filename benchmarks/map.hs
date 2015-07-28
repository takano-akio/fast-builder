{-# LANGUAGE DeriveFunctor #-}
import Control.Concurrent
import Criterion.Main
import qualified Data.IntMap as IM
import Data.Monoid
import qualified System.IO as IO

import qualified Data.ByteString.Builder as Bstr
import qualified Data.ByteString.FastBuilder as Fast

main :: IO ()
main = runInUnboundThread $ do
  h <- IO.openFile "/dev/null" IO.WriteMode
  let
    size sz m = bgroup sz
      [ bench "lazy/fast" $ nf (Fast.toLazyByteString . fastMap) m
      , bench "lazy/bstr" $ nf (Bstr.toLazyByteString . bstrMap) m
      ]
  map10 `seq` map100 `seq` map1000 `seq` map10000 `seq` defaultMain
    [ size "10" map10
    , size "100" map100
    , size "1000" map1000
    , size "10000" map10000
    ]

type Map = IM.IntMap Int

fastMap :: Map -> Fast.Builder
fastMap = Fast.rebuild . IM.foldMapWithKey f
  where
    f key val =
      Fast.int32LE (fromIntegral key) <> Fast.int32LE (fromIntegral val)

bstrMap :: Map -> Bstr.Builder
bstrMap = IM.foldMapWithKey f
  where
    f key val =
      Bstr.int32LE (fromIntegral key) <> Bstr.int32LE (fromIntegral val)

map10 :: Map
map10 = makeItems 10

map100 :: Map
map100 = makeItems 100

map1000 :: Map
map1000 = makeItems 1000

map10000 :: Map
map10000 = makeItems 10000

makeItems :: Int -> Map
makeItems n = IM.fromList $ do
  i <- [0..n-1]
  return (i * 3, i)

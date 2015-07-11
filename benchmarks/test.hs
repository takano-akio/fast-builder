{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnboxedTuples #-}
module F where
import Data.ByteString.FastBuilder
import qualified Data.IntMap as IM
import qualified Data.Foldable as Fold
import Data.Monoid
import GHC.Exts

f0 :: IM.IntMap Int -> Builder
f0 m = inline Fold.foldMap intDec m

f1 :: Tree -> Builder
f1 m = foldMapTree intDec m

g :: Int -> Int -> Builder
g x y = intDec x <> intDec y

h1 :: ((# Int, Int #) -> Int) -> Int
h1 f = f (# 3, 4 #)

i1 :: String -> Builder
i1 x = stringUtf8 x

data Tree
  = Bin !Tree !Tree
  | Tip {-# UNPACK #-} !Int

foldMapTree :: (Monoid m) => (Int -> m) -> Tree -> m
foldMapTree f root = go root
  where
    go (Bin l r) = go l <> go r
    go (Tip x) = f x
{-# INLINE foldMapTree #-}

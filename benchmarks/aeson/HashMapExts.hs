{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
module HashMapExts where

import qualified Data.HashMap.Strict as H
import Data.Monoid
import GHC.Exts (Array#, sizeofArray#, indexArray#, Int(..))
import Unsafe.TrueName (quasiName)

import HashMapNames

foldMapWithKey :: (Monoid m) => (k -> a -> m) -> H.HashMap k a -> m
foldMapWithKey f = go
  where
    go hm = case hm of
      [quasiName|Empty ''H.HashMap|] -> mempty
      [quasiName|BitmapIndexed ''H.HashMap _ ary|] -> foldMapArray go ary
      [quasiName|Full ''H.HashMap ary|] -> foldMapArray go ary
      [quasiName|Collision ''H.HashMap _ ary|] -> foldMapArray leaf ary
      [quasiName|Leaf ''H.HashMap _ l|] -> leaf l

    leaf $(lPat "k" "v")  = f k v
{-# INLINE foldMapWithKey #-}

foldMapArray :: (Monoid m) => (a -> m) -> $(arrayType) a -> m
foldMapArray f $(arrayPat "arr") = foldMapArray' f arr

foldMapArray' :: (Monoid m) => (a -> m) -> Array# a -> m
foldMapArray' f arr = go 0
  where
    go k@(I# k#)
      | k >= I# (sizeofArray# arr) = mempty
      | otherwise = case indexArray# arr k# of
        (# v #) -> f v <> go (k + 1)
{-# INLINE foldMapArray' #-}

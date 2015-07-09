{-# LANGUAGE TemplateHaskell #-}
module HashMapNames where

import qualified Data.HashMap.Strict as H
import Language.Haskell.TH
import Unsafe.TrueName (trueName)

lPat :: String -> String -> Q Pat
lPat a b = do
  leafConName <- trueName "Leaf" ''H.HashMap
  leafTyName <- trueName "Leaf" leafConName
  lName <- trueName "L" leafTyName
  conP lName [varP (mkName a), varP (mkName b)]


arrayPat :: String -> Q Pat
arrayPat a = do
  fullConName <- trueName "Full" ''H.HashMap
  arrayTyName <- trueName "Array" fullConName
  arrayConName <- trueName "Array" arrayTyName
  conP arrayConName [varP (mkName a)]


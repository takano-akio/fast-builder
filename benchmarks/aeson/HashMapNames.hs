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

arrayTypeName :: Q Name
arrayTypeName = do
  fullConName <- trueName "Full" ''H.HashMap
  trueName "Array" fullConName

arrayType :: Q Type
arrayType = conT =<< arrayTypeName

arrayPat :: String -> Q Pat
arrayPat a = do
  arrayTyName <- arrayTypeName
  arrayConName <- trueName "Array" arrayTyName
  conP arrayConName [varP (mkName a)]

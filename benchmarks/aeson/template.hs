{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
module NAME
  ( fromValue
  , valueToLazyByteString
  ) where

#ifndef LIB
#define LIB Data.ByteString.FastBuilder
#endif

import Data.Aeson
import qualified Data.ByteString.Lazy as L
import Data.Monoid
import qualified Data.Scientific as Sci
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Vector as V

import LIB
import HashMapExts

valueToLazyByteString :: Value -> L.ByteString
valueToLazyByteString = toLazyByteString . fromValue

fromValue :: Value -> Builder
-- fromValue v = fromValue' v
fromValue v = rebuild $ fromValue' v

fromValue' :: Value -> Builder
fromValue' (Object o) = fromObject o
fromValue' (Array a) = fromArray a
fromValue' (String s) = fromString s
fromValue' (Number n) = fromNumber n
fromValue' (Bool False) = "false"
fromValue' (Bool True) = "true"
fromValue' Null = "null"
{-# INLINE fromValue' #-}

fromArray :: Array -> Builder
fromArray arr = V.foldr f (const $ char8 ']') arr True
  where
    f x r initial =
      (if initial then char8 '[' else char8 ',')
      <> fromValue x <> r False

fromString :: T.Text -> Builder
fromString str = byteString $ T.encodeUtf8 str

fromNumber :: Sci.Scientific -> Builder
fromNumber = either doubleDec integerDec . Sci.floatingOrInteger

fromObject :: Object -> Builder
fromObject obj = char8 '{' <> foldMapWithKey f obj <> char8 '}'
  where
      f k v =
        fromString k <> char8 ':' <> fromValue v
        <> char8 ','

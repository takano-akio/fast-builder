{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
module NAME
  ( fromValue
  , valueToLazyByteString
  --, fromObjT
  ) where

#ifndef LIB
#define LIB Data.ByteString.FastBuilder
#endif

import Data.Aeson
import qualified Data.ByteString.Lazy as L
import qualified Data.HashMap.Strict as H
--import qualified Data.HashMap.StrictT as HT
import Data.Monoid
import qualified Data.Scientific as Sci
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Vector as V
import GHC.Magic

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

{-
fromObjT :: HT.HashMap T.Text Value -> Builder
fromObjT obj = runM (HT.foldMapWithKeyT wrap unwrap f' obj) True <>
  char7 '}'
  where
    f' k v = M (f k v)
    f k v initial = rebuild $ \_ ->
      char7 (if initial then '{' else ',')
      <> fromString k <> char7 ':' <> fromValue v
-}

newtype G e = G { unG :: e -> Bool -> Builder }

wrap :: G e -> e -> M
wrap (G f) e = M (f e)

unwrap :: (e -> M) -> G e
unwrap f = G $ \e b -> rebuild $ runM (f e) b

fromObject :: Object -> Builder
fromObject obj = char8 '{' <> foldMapWithKey f obj <> char8 '}'
  where
      f k v =
        fromString k <> char8 ':' <> fromValue v
        <> char8 ','
{-
fromObject :: Object -> Builder
fromObject = fromObjectWith (const $ char8 '}')
-}

fromObjectWith :: (Bool -> Builder) -> Object -> Builder
fromObjectWith final = \obj -> H.foldrWithKey f final obj True
  where
    f k v r = oneShot $ \initial ->
      (char8 $ if initial then '{' else ',')
      <> fromString k <> char8 ':' <> fromValue v <> r False
{-# INLINE[0] fromObjectWith #-}

newtype M = M {runM :: Bool -> Builder }

instance Monoid M where
  mempty = M $ const mempty
  mappend (M f) (M g) = M $ \initial ->
    f initial <> g False

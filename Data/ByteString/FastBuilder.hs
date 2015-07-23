-- | An efficient implementation of ByteString builder.
--
-- In many cases, this module works as a drop-in replacement for
-- Data.ByteString.Builder, and should improve speed. However, one caveat
-- applies: when using 'toLazyByteString', if you consume the result
-- in a bound thread, performance degrades significantly. See the
-- documentation for 'toLazyByteString' for details.
--
-- Sometimes performance can be greatly improved by inserting calls to
-- 'rebuild' to your program. See the documentation for 'rebuild' for
-- details.
module Data.ByteString.FastBuilder
  (
  -- * The type

  Builder

  -- * Running a builder
  , toLazyByteString
  , toLazyByteStringWith
  , toStrictByteString
  , hPutBuilder

  -- * Performance tuning
  , rebuild

  -- * Basic builders
  , primBounded
  , primFixed
  , byteString
  , byteStringInsert
  , byteStringCopy
  , byteStringThreshold

  -- * Single byte
  , int8
  , word8

  -- * Little endian
  , int16LE
  , int32LE
  , int64LE

  , word16LE
  , word32LE
  , word64LE

  -- * Big endian
  , int16BE
  , int32BE
  , int64BE

  , word16BE
  , word32BE
  , word64BE

  -- * Decimal
  , intDec
  , int8Dec
  , int16Dec
  , int32Dec
  , int64Dec

  , wordDec
  , word8Dec
  , word16Dec
  , word32Dec
  , word64Dec

  , integerDec
  , doubleDec

  -- * UTF-8
  , charUtf8
  , stringUtf8

  -- * ASCII
  , char7
  , string7

  -- * ISO-8859-1
  , char8
  , string8
  ) where

import Data.ByteString.FastBuilder.Base
import Data.ByteString.FastBuilder.Prim

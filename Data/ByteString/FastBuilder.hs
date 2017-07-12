-- | An efficient implementation of ByteString builder.
--
-- In many cases, this module works as a drop-in replacement for
-- Data.ByteString.Builder, and should improve speed.
--
-- = Performance tips
--
-- fast-builder should be faster than the standard builder in most situations.
-- However, by following certain code patterns, you can often achive even
-- more efficient code, with almost no memory allocation aside from the
-- resulting 'ByteString' itself. The below are a list of hints for writing
-- efficient code for constructing builders.
--
-- == Return builders directly from your function
--
-- Once you construct a builder, it's usually a good idea to just return it from
-- your function. Avoid storing it in a data structure or passing it to another
-- function, unless they are going to be eliminated by the compiler.
-- Schematically, prefer this:
--
-- @
-- good :: YourDataStructure -> Builder
-- good d = serializeThis (this d) <> serializeThat (that d)
-- @
--
-- over:
--
-- @
-- bad0 :: YourDataStructure -> (Int, Builder)
-- bad0 d
--    = (compute d, serializeThis (this d) <> serializeThat (that d))
-- @
--
-- or:
--
-- @
-- bad1 :: YourDataStructure -> Builder
-- bad1 d = serializeMore d (serializeThis (this d))
-- @
--
-- An important special case of this general rule is to prefer foldr over
-- foldl' when serializing a list, and to prefer structural recursion over
-- tail recursion in general.
--
-- === Use 'rebuild'
--
-- When your function returns a different builder depending on the input,
-- it's usually a good idea to use 'rebuild' to wrap the whole body of your
-- function.  See the documentation for 'rebuild' for details.
--
-- === Background
--
-- Why is it good to return builders directly? It is because they are
-- implemented as functions. When storing a function in a data structure or
-- passing it around, you need to first allocate a closure for it. However,
-- if you are just returning it, the returned function can be merged with your
-- function, creating a function with a larger arity. For example, GHC can
-- compile the @good@ function above into a 5-ary function, which requires
-- no runtime allocation (the exact arity depends on the library version).
--
-- == Watch out for lazy ByteString generation
--
-- When using 'toLazyByteString', if you consume the result
-- in a bound thread, performance degrades significantly. See the
-- documentation for 'toLazyByteString' for details.
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

  , floatLE
  , doubleLE

  -- * Big endian
  , int16BE
  , int32BE
  , int64BE

  , word16BE
  , word32BE
  , word64BE

  , floatBE
  , doubleBE

  -- * Host-dependent size and byte order, non-portable
  , intHost
  , int16Host
  , int32Host
  , int64Host

  , wordHost
  , word16Host
  , word32Host
  , word64Host

  , floatHost
  , doubleHost

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
  , floatDec
  , doubleDec

  -- * Hexadecimal
  , wordHex
  , word8Hex
  , word16Hex
  , word32Hex
  , word64Hex

  -- * Fixed-width hexadecimal
  , int8HexFixed
  , int16HexFixed
  , int32HexFixed
  , int64HexFixed

  , word8HexFixed
  , word16HexFixed
  , word32HexFixed
  , word64HexFixed

  , floatHexFixed
  , doubleHexFixed

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

import Data.ByteString.FastBuilder.Internal
import Data.ByteString.FastBuilder.Internal.Prim

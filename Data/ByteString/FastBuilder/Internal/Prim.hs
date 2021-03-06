{-# LANGUAGE CPP #-}

-- | This is an internal module; its interface is unstable.
module Data.ByteString.FastBuilder.Internal.Prim where

import Data.ByteString.FastBuilder.Internal
import qualified Data.ByteString.Builder.Prim as P
import Data.Int
import Data.Word

# define WRAP_WITH(typ, name, def) name :: typ -> Builder; name = def; {-# INLINE name #-}
# define WRAP_F(type,name) WRAP_WITH(type,name,primFixed P.name)
# define WRAP_B(type,name) WRAP_WITH(type,name,primBounded P.name)
# define WRAP_SHOW7(type,name) WRAP_WITH(type,name,string7 . show)

WRAP_F(Int,intHost)
WRAP_F(Int8,int8)
WRAP_F(Int16,int16LE)
WRAP_F(Int16,int16BE)
WRAP_F(Int16,int16Host)
WRAP_F(Int32,int32LE)
WRAP_F(Int32,int32BE)
WRAP_F(Int32,int32Host)
WRAP_F(Int64,int64LE)
WRAP_F(Int64,int64BE)
WRAP_F(Int64,int64Host)

WRAP_F(Word,wordHost)
WRAP_F(Word8,word8)
WRAP_F(Word16,word16LE)
WRAP_F(Word16,word16BE)
WRAP_F(Word16,word16Host)
WRAP_F(Word32,word32LE)
WRAP_F(Word32,word32BE)
WRAP_F(Word32,word32Host)
WRAP_F(Word64,word64LE)
WRAP_F(Word64,word64BE)
WRAP_F(Word64,word64Host)

WRAP_F(Float,floatLE)
WRAP_F(Float,floatBE)
WRAP_F(Float,floatHost)
WRAP_F(Double,doubleLE)
WRAP_F(Double,doubleBE)
WRAP_F(Double,doubleHost)

WRAP_B(Int,intDec)
WRAP_B(Int8,int8Dec)
WRAP_B(Int16,int16Dec)
WRAP_B(Int32,int32Dec)
WRAP_B(Int64,int64Dec)

WRAP_B(Word,wordDec)
WRAP_B(Word8,word8Dec)
WRAP_B(Word16,word16Dec)
WRAP_B(Word32,word32Dec)
WRAP_B(Word64,word64Dec)

WRAP_B(Word,wordHex)
WRAP_B(Word8,word8Hex)
WRAP_B(Word16,word16Hex)
WRAP_B(Word32,word32Hex)
WRAP_B(Word64,word64Hex)

WRAP_F(Int8,int8HexFixed)
WRAP_F(Int16,int16HexFixed)
WRAP_F(Int32,int32HexFixed)
WRAP_F(Int64,int64HexFixed)

WRAP_F(Word8,word8HexFixed)
WRAP_F(Word16,word16HexFixed)
WRAP_F(Word32,word32HexFixed)
WRAP_F(Word64,word64HexFixed)

WRAP_F(Float,floatHexFixed)
WRAP_F(Double,doubleHexFixed)

WRAP_F(Char,char7)
WRAP_WITH(String,string7,primMapListFixed P.char7)

WRAP_F(Char,char8)
WRAP_WITH(String,string8,primMapListFixed P.char8)

WRAP_B(Char,charUtf8)
WRAP_WITH(String,stringUtf8,primMapListBounded P.charUtf8)

WRAP_SHOW7(Integer,integerDec)
WRAP_SHOW7(Double,floatDec)
WRAP_SHOW7(Double,doubleDec)

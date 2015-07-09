{-# LANGUAGE CPP #-}
#define LIB Data.ByteString.Builder
#define NAME Bstr
#include "template.hs"

rebuild :: Builder -> Builder
rebuild = id

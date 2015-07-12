{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

import qualified Control.Exception as E
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import Data.Monoid
import Data.Typeable
import Data.Word
import qualified Test.QuickCheck as QC

import Data.ByteString.FastBuilder

data BuilderTree
  = Leaf BuilderPrim
  | Mappend BuilderTree BuilderTree
  deriving (Show)

data BuilderPrim
  = BP_Word8 !Word8
  | BP_Word64le !Word64
  | BP_ByteString !BS.ByteString
  | BP_Empty
  deriving (Show)

instance QC.Arbitrary BuilderTree where
  arbitrary = QC.sized $ \size ->
    if size == 0
      then return (Leaf BP_Empty)
      else QC.oneof
        [ Leaf <$> QC.arbitrary
        , QC.scale (`div` 2) $ Mappend <$> QC.arbitrary <*> QC.arbitrary
        ]

instance QC.Arbitrary BuilderPrim where
  arbitrary = QC.oneof
    [ BP_Word8 <$> QC.arbitrary
    , BP_Word64le <$> QC.arbitrary
    , BP_ByteString . BS.pack <$> QC.arbitrary
    , pure BP_Empty
    ]

data Driver
  = ToLazyByteString
  | ToLazyByteStringWith_10
  deriving (Show, Enum, Bounded)

instance QC.Arbitrary Driver where
  arbitrary = QC.arbitraryBoundedEnum

newtype TestException = TextException Int
  deriving (Show, Eq, Typeable, QC.Arbitrary)

instance E.Exception TestException

runBuilder :: Driver -> Builder -> BS.ByteString
runBuilder ToLazyByteString = BSL.toStrict . toLazyByteString
runBuilder ToLazyByteStringWith_10 =
  BSL.toStrict . toLazyByteStringWith 10 (const 10)

mkBuilder :: BuilderTree -> Builder
mkBuilder = go
  where
    go (Leaf p) = prim p
    go (Mappend a b) = go a <> go b

    prim (BP_Word8 w) = word8 w
    prim (BP_Word64le w) = word64LE w
    prim (BP_ByteString bs) = byteString bs
    prim BP_Empty = mempty

buildWithBuilder :: Driver -> BuilderTree -> BS.ByteString
buildWithBuilder drv = runBuilder drv . mkBuilder

buildWithList :: BuilderTree -> BS.ByteString
buildWithList = BS.pack . ($[]) . go
  where
    go (Leaf p) = prim p
    go (Mappend a b) = go a . go b

    prim (BP_Word8 w) = (w:)
    prim (BP_Word64le w) = (list++)
      where
        list = take 8 $ map fromIntegral $ iterate (`div` 256) w
    prim (BP_ByteString bs) = (BS.unpack bs ++)
    prim BP_Empty = id

errorBuilder :: TestException -> Builder
errorBuilder ex = mempty <> E.throw ex
{-# NOINLINE errorBuilder #-}

-- | Builder's semantics matches the reference implementation.
prop_builderTree :: Driver -> BuilderTree -> Bool
prop_builderTree drv tree = buildWithBuilder drv tree == buildWithList tree

-- | When a Builder throws a synchronous exception, it should come out
-- unchanged from the driver.
prop_syncException
    :: TestException
    -> Driver
    -> BuilderTree
    -> BuilderTree
    -> QC.Property
prop_syncException ex drv before after = QC.ioProperty $ do
  r <- E.try $ E.evaluate bstr
  return $ r == Left ex
  where
    bstr = runBuilder drv $
      mkBuilder before <> errorBuilder ex <> mkBuilder after

return []

main :: IO ()
main = do
  True <- $(QC.quickCheckAll)
  return ()

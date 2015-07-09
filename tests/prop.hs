{-# LANGUAGE TemplateHaskell #-}

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import Data.Monoid
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
  arbitrary = QC.oneof
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

buildWithBuilder :: BuilderTree -> BS.ByteString
buildWithBuilder = BSL.toStrict . toLazyByteString . go
  where
    go (Leaf p) = prim p
    go (Mappend a b) = go a <> go b

    prim (BP_Word8 w) = word8 w
    prim (BP_Word64le w) = word64LE w
    prim (BP_ByteString bs) = byteString bs
    prim BP_Empty = mempty

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

prop_builderTree :: BuilderTree -> Bool
prop_builderTree tree = buildWithBuilder tree == buildWithList tree

return []

main :: IO ()
main = do
  True <- $(QC.quickCheckAll)
  return ()

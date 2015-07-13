{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Main where

import Control.Concurrent
import Control.Concurrent.STM
import qualified Control.Exception as E
import Control.Monad
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import Data.Monoid
import Data.Typeable
import Data.Word
import System.IO.Unsafe
import System.Timeout
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

asyncExnBuilder :: ThreadId -> TestException -> Builder
asyncExnBuilder tid ex = mempty <> unsafePerformIO act
  where
    act = do
      E.throwTo tid ex
      return mempty
{-# NOINLINE asyncExnBuilder #-}

nonterminatingBuilder :: TVar Bool -> Builder
nonterminatingBuilder inBlock = mempty <> unsafePerformIO act
  where
    act = do
      atomically $ writeTVar inBlock True
      loop 0
      `E.finally` atomically (writeTVar inBlock False)

    loop k
      | k < (0::Integer) = return mempty
      | otherwise = loop $ k + 1
{-# NOINLINE nonterminatingBuilder #-}

tryEvaluate :: a -> IO (Either TestException a)
tryEvaluate = E.try . E.evaluate

-- | Builder's semantics matches the reference implementation.
prop_builderTree :: Driver -> BuilderTree -> Bool
prop_builderTree drv tree = buildWithBuilder drv tree == buildWithList tree

-- | When a Builder throws a synchronous exception, the exception should come
-- out unchanged from the driver. Evaluating the result again should repeat
-- the same exception.
prop_syncException
    :: TestException
    -> Driver
    -> BuilderTree
    -> BuilderTree
    -> QC.Property
prop_syncException ex drv before after = QC.ioProperty $ do
  r <- tryEvaluate bstr
  r1 <- tryEvaluate bstr
  return $ (r, r1) == (Left ex, Left ex)
  where
    bstr = runBuilder drv $
      mkBuilder before <> errorBuilder ex <> mkBuilder after

-- | When a Builder is interrupted by an asynchronous exception, the exception
-- should come out unchanged from the driver. Evaluating the result again
-- should then succeed.
prop_asyncException
    :: TestException
    -> Driver
    -> BuilderTree
    -> BuilderTree
    -> QC.Property
prop_asyncException ex drv before after = QC.ioProperty $ do
  tid <- myThreadId
  let
    bstr = runBuilder drv $
      mkBuilder before <> asyncExnBuilder tid ex <> mkBuilder after
  r <- tryEvaluate bstr
  r1 <- tryEvaluate bstr
  return $ (r, r1) == (Left ex, Right $ buildWithList (Mappend before after))

-- | When a non-terminating Builder is interrupted by an asynchronous exception,
-- it should be killed, rather than keep running forever.
prop_asyncExceptionInterrupts
    :: TestException
    -> Driver
    -> BuilderTree
    -> BuilderTree
    -> QC.Property
prop_asyncExceptionInterrupts ex drv before after = QC.ioProperty $ do
  inNontermV <- newTVarIO False
  tid <- myThreadId
  let
    bstr = runBuilder drv $
      mkBuilder before <> nonterminatingBuilder inNontermV <> mkBuilder after
  _ <- forkIO $ do
    atomically $ guard =<< readTVar inNontermV
    E.throwTo tid ex
  r <- tryEvaluate bstr
  r1 <- timeout 100000 $ atomically $ guard . not =<< readTVar inNontermV
  return $ (r, r1) == (Left ex, Just ())

return []

main :: IO ()
main = do
  True <- $(QC.forAllProperties) $ \prop -> do
    r <- QC.quickCheckWithResult QC.stdArgs{ QC.chatty = False } prop
    print r
    return r
  return ()

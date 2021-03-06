import qualified Fast as F
import qualified Bstr as B
import Control.Concurrent
import Control.DeepSeq
import Criterion.Main
import Data.Aeson (decode', encode)
import qualified Data.ByteString.Lazy as L

main :: IO ()
main = runInUnboundThread $ do
  Just json <- decode' <$> L.readFile "benchmarks/aeson/twitter100-mangled.json"
  rnf json `seq` return ()
  defaultMain
    [ bench "fast" $ nf F.valueToLazyByteString json
    , bench "bstr" $ nf B.valueToLazyByteString json
    , bench "aeson" $ nf encode json
    ]

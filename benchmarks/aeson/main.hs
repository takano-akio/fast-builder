import qualified Fast as F
import qualified Bstr as B
import Control.DeepSeq
import Criterion.Main
import Data.Aeson (decode', encode)
import qualified Data.ByteString.Lazy as L

main :: IO ()
main = do
  Just json <- decode' <$> L.readFile "../aeson/benchmarks/json-data/twitter100.json"
  rnf json `seq` return ()
  defaultMain
    [ bench "fast" $ nf F.valueToLazyByteString json
    , bench "bstr" $ nf B.valueToLazyByteString json
    , bench "aeson" $ nf encode json
    ]

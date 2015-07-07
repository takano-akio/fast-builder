import qualified Aeson.Fast as F
import qualified Aeson.Bstr as B
import Control.Applicative
import Control.DeepSeq
import Criterion.Main
import Data.Aeson
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

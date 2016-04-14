module TestUtils (decode') where

import           Control.Monad.IO.Class (liftIO)
import           Data.Either.Unwrap     (fromRight)
import           Network.IPFS           (IPFS (..), Hash, decode)

decode' :: String -> Hash
decode' = fromRight . decode
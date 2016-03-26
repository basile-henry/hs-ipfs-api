module Network.IPFS.TestUtils (decode') where

import           Data.Either.Unwrap (fromRight)
import           Network.IPFS       (Hash, decode)

decode' :: String -> Hash
decode' = fromRight . decode

module IPNS(resolve, publish) where

import           Data.ByteString.Lazy (ByteString)
import           Network.IPFS.API     (Endpoint (..), call)

resolve :: Endpoint -> Hash -> IO ByteString
resolve endpoint hash =
    call endpoint ["name", "resolve"] [] [hash]

publish :: Endpoint -> Hash -> IO ByteString
publish endpoint hash =
    call endpoint ["name", "publish"] [] [hash]
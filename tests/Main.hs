module Main(main) where

import           Network.IPFS.API
import qualified Network.IPFS.API.Tests
import qualified Network.IPFS.Tests
import qualified Network.IPNS.Tests
import           Test.Tasty             (defaultMain, testGroup)

main :: IO ()
main = do
    endpoint <- localEndpoint
    defaultMain $ testGroup "Tests" [
            Network.IPFS.API.Tests.tests endpoint,
            Network.IPFS.Tests.tests     endpoint,
            Network.IPNS.Tests.tests     endpoint
        ]

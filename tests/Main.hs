module Main(main) where

import           Network.IPFS
import           Network.IPFS.API
import qualified Network.IPFS.API.Tests
import qualified Network.IPFS.Tests
import qualified Network.IPNS.Tests
import           Test.Tasty             (defaultMain, testGroup)

main :: IO ()
main = do
    endpoint <- localEndpoint
    defaultMain =<< runIPFS endpoint (testGroup "Tests" <$> sequence [
                Network.IPNS.Tests.tests,
                Network.IPFS.API.Tests.tests,
                Network.IPFS.Tests.tests
            ])

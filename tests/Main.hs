module Main(main) where

import           Test.Tasty             (defaultMain, testGroup)

import qualified Network.IPFS.API.Tests
import qualified Network.IPFS.Tests
import qualified Network.IPNS.Tests

main :: IO ()
main = defaultMain $ testGroup "Tests" [
        Network.IPFS.API.Tests.tests,
        Network.IPFS.Tests.tests,
        Network.IPNS.Tests.tests
    ]

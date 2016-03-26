module Network.IPNS.Tests(tests) where

import           Data.Maybe       (fromJust)
import           Network.IPNS
import           Network.IPFS.API
import           Test.HUnit       (Assertion, (@?=))
import           Test.Tasty       (TestTree, testGroup)
import           Test.Tasty.HUnit (testCase)

tests :: Endpoint -> TestTree
tests endpoint = testGroup "Network.IPNS" [
        testCase "testPublish"     $ testPublish     endpoint,
        testCase "testResolve"     $ testResolve     endpoint,
        testCase "testResolvePath" $ testResolvePath endpoint
    ]

-- This is node dependent (to be personalised for proper testing)
testPublish :: Endpoint -> Assertion
testPublish endpoint = do
    filePath <- fromJust <$> publish endpoint "QmeVPgSQ5Hpq9hiqw9X8CCQx421Ei9SkmtzXyhkSHdcB4A"
    filePath @?= "QmZzD8HfE5nK8ud3TVLujvKQNsAzQ8QPDMa1yUUFWhK4iN"

-- This is node dependent (to be personalised for proper testing)
testResolve :: Endpoint -> Assertion
testResolve endpoint = do
    filePath <- fromJust <$> resolve endpoint
    filePath @?= "/ipfs/QmeVPgSQ5Hpq9hiqw9X8CCQx421Ei9SkmtzXyhkSHdcB4A"

-- This is not immutable data (to be kept up to date for proper testing)
testResolvePath :: Endpoint -> Assertion
testResolvePath endpoint = do
    filePath <- fromJust <$> resolvePath endpoint "QmZzD8HfE5nK8ud3TVLujvKQNsAzQ8QPDMa1yUUFWhK4iN"
    filePath @?= "/ipfs/QmeVPgSQ5Hpq9hiqw9X8CCQx421Ei9SkmtzXyhkSHdcB4A"

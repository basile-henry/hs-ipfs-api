module Network.IPNS.Tests(tests) where

import           Data.Maybe       (fromJust)
import           Network.IPNS
import           Network.IPFS.API
import           Test.HUnit       (Assertion, (@=?))
import           Test.Tasty       (TestTree, testGroup)
import           Test.Tasty.HUnit (testCase)

tests :: TestTree
tests = testGroup "Network.IPNS" [
        testCase  "testPublish"     testPublish,
        testCase  "testResolve"     testResolve,
        testCase  "testResolvePath" testResolvePath
    ]

-- This is node dependent (to be personalised for proper testing)
testPublish :: Assertion
testPublish = do
    endpoint <- localEndpoint
    filePath <- fromJust <$> publish endpoint "QmeVPgSQ5Hpq9hiqw9X8CCQx421Ei9SkmtzXyhkSHdcB4A"
    filePath @=? "QmZzD8HfE5nK8ud3TVLujvKQNsAzQ8QPDMa1yUUFWhK4iN"

-- This is node dependent (to be personalised for proper testing)
testResolve :: Assertion
testResolve = do
    endpoint <- localEndpoint
    filePath <- fromJust <$> resolve endpoint
    filePath @=? "/ipfs/QmeVPgSQ5Hpq9hiqw9X8CCQx421Ei9SkmtzXyhkSHdcB4A"

-- This is not immutable data (to be kept up to date for proper testing)
testResolvePath :: Assertion
testResolvePath = do
    endpoint <- localEndpoint
    filePath <- fromJust <$> resolvePath endpoint "QmZzD8HfE5nK8ud3TVLujvKQNsAzQ8QPDMa1yUUFWhK4iN"
    filePath @=? "/ipfs/QmeVPgSQ5Hpq9hiqw9X8CCQx421Ei9SkmtzXyhkSHdcB4A"

module Network.IPNS.Tests(tests) where

import           Data.Maybe       (fromJust)
import           Network.IPFS     (IPFS (..))
import           Network.IPFS.API
import           Network.IPNS
import           Test.HUnit       (Assertion, (@?=))
import           Test.Tasty       (TestTree, testGroup)
import           Test.Tasty.HUnit (testCase)

tests :: IPFS TestTree
tests = testGroup "Network.IPNS" <$> sequence [
        testCase "testPublish"     <$> testPublish,
        testCase "testResolve"     <$> testResolve,
        testCase "testResolvePath" <$> testResolvePath
    ]

-- This is node dependent (to be personalised for proper testing)
testPublish :: IPFS Assertion
testPublish = do
    filePath <- publish "QmeVPgSQ5Hpq9hiqw9X8CCQx421Ei9SkmtzXyhkSHdcB4A"
    return $ filePath @?= "QmZzD8HfE5nK8ud3TVLujvKQNsAzQ8QPDMa1yUUFWhK4iN"

-- This is node dependent (to be personalised for proper testing)
testResolve :: IPFS Assertion
testResolve = do
    filePath <- resolve
    return $ filePath @?= "/ipfs/QmeVPgSQ5Hpq9hiqw9X8CCQx421Ei9SkmtzXyhkSHdcB4A"

-- This is not immutable data (to be kept up to date for proper testing)
testResolvePath :: IPFS Assertion
testResolvePath = do
    filePath <- resolvePath "QmZzD8HfE5nK8ud3TVLujvKQNsAzQ8QPDMa1yUUFWhK4iN"
    return $ filePath @?= "/ipfs/QmeVPgSQ5Hpq9hiqw9X8CCQx421Ei9SkmtzXyhkSHdcB4A"

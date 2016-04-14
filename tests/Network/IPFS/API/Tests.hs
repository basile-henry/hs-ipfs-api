module Network.IPFS.API.Tests(tests) where

import           Data.ByteString.Lazy.UTF8 (fromString)
import           Network.IPFS              (IPFS (..))
import           Network.IPFS.API
import           Test.HUnit                (Assertion, (@?=))
import           Test.Tasty                (TestTree, testGroup)
import           Test.Tasty.HUnit          (testCase)

tests :: IPFS TestTree
tests = testGroup "Network.IPFS.API" <$> sequence [
        testCase "testCall"            <$> testCall,
        testCase "testCallWithContent" <$> testCallWithContent
    ]

testCall :: IPFS Assertion
testCall = do
    configAddr <- call ["config"] [] ["Addresses.API"]
    return $ configAddr @?= fromString "{\"Key\":\"Addresses.API\",\"Value\":\"/ip4/127.0.0.1/tcp/5001\"}\n"

testCallWithContent :: IPFS Assertion
testCallWithContent = do
    fileHash <- callWithContent ["add"] [("q", "true")] [] (Raw $ fromString "Hello test!\n")
    return $ fileHash @?= fromString "{\"Name\":\"QmeVPgSQ5Hpq9hiqw9X8CCQx421Ei9SkmtzXyhkSHdcB4A\",\"Hash\":\"QmeVPgSQ5Hpq9hiqw9X8CCQx421Ei9SkmtzXyhkSHdcB4A\"}\n"

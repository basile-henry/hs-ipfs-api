module Network.IPFS.API.Tests(tests) where

import           Data.ByteString.Lazy.UTF8 (fromString)
import           Network.IPFS.API
import           Test.HUnit                (Assertion, (@?=))
import           Test.Tasty                (TestTree, testGroup)
import           Test.Tasty.HUnit          (testCase)

tests :: TestTree
tests = testGroup "Network.IPFS.API" [
        testCase  "testCall"            testCall,
        testCase  "testCallWithContent" testCallWithContent
    ]

testCall :: Assertion
testCall = do
    endpoint   <- localEndpoint
    configAddr <- call endpoint ["config"] [] ["Addresses.API"]
    configAddr @?= fromString "{\"Key\":\"Addresses.API\",\"Value\":\"/ip4/127.0.0.1/tcp/5001\"}\n"

testCallWithContent :: Assertion
testCallWithContent = do
    endpoint <- localEndpoint
    fileHash <- callWithContent endpoint ["add"] [("q", "true")] [] (Raw $ fromString "Hello test!\n")
    fileHash @?= fromString "{\"Name\":\"QmeVPgSQ5Hpq9hiqw9X8CCQx421Ei9SkmtzXyhkSHdcB4A\",\"Hash\":\"QmeVPgSQ5Hpq9hiqw9X8CCQx421Ei9SkmtzXyhkSHdcB4A\"}\n"

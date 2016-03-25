module Network.IPFS.Tests(tests) where

import qualified Data.ByteString           as B
import qualified Data.ByteString.Lazy.UTF8 as LU
import qualified Data.ByteString.UTF8      as U
import           Data.Maybe                (fromJust, isJust)
import           Network.IPFS
import           Network.IPFS.API
import           Test.HUnit                (Assertion, assertBool, (@=?))
import           Test.Tasty                (TestTree, testGroup)
import           Test.Tasty.HUnit          (testCase)

tests :: TestTree
tests = testGroup "Network.IPFS" [
        testCase  "testCat"        testCat,
        testCase  "testGetObject"  testGetObject,
        testCase  "testNewObject"  testNewObject,
        testCase  "testAddLink"    testAddLink,
        testCase  "testRemoveLink" testRemoveLink,
        testCase  "testSetData"    testSetData,
        testCase  "testAppendData" testAppendData,
        testCase  "testAdd"        testAdd,
        testCase  "testAddFile"    testAddFile,
        testCase  "testAddFiles"   testAddFiles,
        testCase  "testAddDir"     testAddDir,
        testCase  "testGetID"      testGetID
    ]

testCat :: Assertion
testCat = do
    endpoint   <- localEndpoint
    bytestring <- cat endpoint "QmeVPgSQ5Hpq9hiqw9X8CCQx421Ei9SkmtzXyhkSHdcB4A"
    bytestring @=? LU.fromString "Hello test!\n"

testGetObject :: Assertion
testGetObject = do
    endpoint <- localEndpoint
    object   <- getObject endpoint $ U.fromString "QmRKbLqN6DB9eZD5UVF43M5vBxWZSv9wL7Mvs2wa1v1oot"
    print object
    object   @=? Object
        (U.fromString "QmRKbLqN6DB9eZD5UVF43M5vBxWZSv9wL7Mvs2wa1v1oot")
        (B.pack [0x08, 0x01])
        [("file", Object
            (U.fromString "QmeVPgSQ5Hpq9hiqw9X8CCQx421Ei9SkmtzXyhkSHdcB4A")
            (B.pack [0x08, 0x02, 0x12, 0x0C, 0x48, 0x65, 0x6C, 0x6C, 0x6F, 0x20, 0x74, 0x65, 0x73, 0x74, 0x21, 0x0A, 0x18, 0x0C])
            [])
        ]

testNewObject :: Assertion
testNewObject = do
    endpoint <- localEndpoint
    hash     <- fromJust <$> newObject endpoint Unixfs
    hash     @=? U.fromString "QmUNLLsPACCz1vLxQVkXqqLX5R1X345qqfHbsf67hvA3Nn"

testAddLink :: Assertion
testAddLink = do
    endpoint <- localEndpoint
    hash     <- fromJust <$> addLink endpoint
        (U.fromString "QmUNLLsPACCz1vLxQVkXqqLX5R1X345qqfHbsf67hvA3Nn")
        (FileHash "file" $ U.fromString "QmeVPgSQ5Hpq9hiqw9X8CCQx421Ei9SkmtzXyhkSHdcB4A")
    hash     @=? U.fromString "QmRKbLqN6DB9eZD5UVF43M5vBxWZSv9wL7Mvs2wa1v1oot"

testRemoveLink :: Assertion
testRemoveLink = do
    endpoint <- localEndpoint
    hash     <- fromJust <$> removeLink endpoint
        (U.fromString "QmRKbLqN6DB9eZD5UVF43M5vBxWZSv9wL7Mvs2wa1v1oot")
        "file"
    hash     @=? U.fromString "QmUNLLsPACCz1vLxQVkXqqLX5R1X345qqfHbsf67hvA3Nn"

testSetData :: Assertion
testSetData = do
    endpoint <- localEndpoint
    hash     <- fromJust <$> setData endpoint
        (U.fromString "QmdfTbBqBPQ7VNxZEYEj14VmRuZBkqFbiwReogJgS1zR1n")
        (B.pack [0x08, 0x02, 0x12, 0x0C, 0x48, 0x65, 0x6C, 0x6C, 0x6F, 0x20, 0x74, 0x65, 0x73, 0x74, 0x21, 0x0A, 0x18, 0x0C])
    hash     @=? U.fromString "QmeVPgSQ5Hpq9hiqw9X8CCQx421Ei9SkmtzXyhkSHdcB4A"

testAppendData :: Assertion
testAppendData = do
    endpoint <- localEndpoint
    hash     <- fromJust <$> appendData endpoint
        (U.fromString "QmeVPgSQ5Hpq9hiqw9X8CCQx421Ei9SkmtzXyhkSHdcB4A")
        (U.fromString "more data")
    hash     @=? U.fromString "QmUqH1JgUxLnUhcjqmfWDLDAbr6zNnqT9gmXZRJAjQH4Ae"

testAdd :: Assertion
testAdd = do
    endpoint <- localEndpoint
    hash     <- fromJust <$> add endpoint (LU.fromString "Hello test!\n")
    hash     @=? FileHash
        "QmeVPgSQ5Hpq9hiqw9X8CCQx421Ei9SkmtzXyhkSHdcB4A"
        (U.fromString "QmeVPgSQ5Hpq9hiqw9X8CCQx421Ei9SkmtzXyhkSHdcB4A")

testAddFile :: Assertion
testAddFile = do
    endpoint <- localEndpoint
    hash     <- fromJust <$> addFile endpoint "tests/test_content/file1"
    hash     @=? FileHash
        "file1"
        (U.fromString "QmbFMke1KXqnYyBBWxB74N4c5SBnJMVAiMNRcGu6x1AwQH")

testAddFiles :: Assertion
testAddFiles = do
    endpoint <- localEndpoint
    hash     <- fromJust <$> addFiles endpoint ["tests/test_content/file1", "tests/test_content/file2", "tests/test_content/file3"]
    hash     @=? [
            FileHash "file1" $ U.fromString "QmbFMke1KXqnYyBBWxB74N4c5SBnJMVAiMNRcGu6x1AwQH",
            FileHash "file2" $ U.fromString "QmbFMke1KXqnYyBBWxB74N4c5SBnJMVAiMNRcGu6x1AwQH",
            FileHash "file3" $ U.fromString "QmbFMke1KXqnYyBBWxB74N4c5SBnJMVAiMNRcGu6x1AwQH"
        ]

testAddDir :: Assertion
testAddDir = do
    endpoint <- localEndpoint
    hash     <- fromJust <$> addDir endpoint "tests/test_content"
    hash     @=? FileHash
        "test_content"
        (U.fromString "QmbvqVQ6kgG8vwMamqhxgB7ET79iM9ToD7bdeRo9sWbgj5")

testGetID :: Assertion
testGetID = do
    maybeID <- getID =<< localEndpoint
    assertBool "Failed to get ID"
        $ isJust maybeID

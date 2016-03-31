module Network.IPFS.Tests(tests) where

import           Data.ByteString.Lazy
import           Data.ByteString.Lazy.UTF8
import           Data.Maybe                (fromJust, isJust)
import           Network.IPFS
import           Network.IPFS.API
import           Network.IPFS.TestUtils
import           Test.HUnit                (Assertion, assertBool, (@?=))
import           Test.Tasty                (TestTree, testGroup)
import           Test.Tasty.HUnit          (testCase)

tests :: Endpoint -> TestTree
tests endpoint = testGroup "Network.IPFS" [
        testCase  "testCat"        $ testCat        endpoint,
        testCase  "testGetID"      $ testGetID      endpoint,
        -- testCase  "testGetObject"  $ testGetObject  endpoint,
        testCase  "testNewObject"  $ testNewObject  endpoint,
        testCase  "testAddLink"    $ testAddLink    endpoint,
        testCase  "testRemoveLink" $ testRemoveLink endpoint,
        testCase  "testSetData"    $ testSetData    endpoint,
        testCase  "testAppendData" $ testAppendData endpoint,
        testCase  "testAdd"        $ testAdd        endpoint,
        testCase  "testAddFile"    $ testAddFile    endpoint,
        testCase  "testAddFiles"   $ testAddFiles   endpoint,
        testCase  "testAddDir"     $ testAddDir     endpoint,
        testCase  "testMount"      $ testMount      endpoint
        -- testCase  "testLs"         $ testLs         endpoint
    ]

testCat :: Endpoint -> Assertion
testCat endpoint = do
    bytestring <- cat endpoint "QmeVPgSQ5Hpq9hiqw9X8CCQx421Ei9SkmtzXyhkSHdcB4A"
    bytestring @?= fromString "Hello test!\n"

testGetID :: Endpoint -> Assertion
testGetID endpoint = do
    maybeID <- getID endpoint
    assertBool "Failed to get ID" $ isJust maybeID

testGetObject :: Endpoint -> Assertion
testGetObject endpoint = do
    object   <- getObject endpoint $ decode' "QmRKbLqN6DB9eZD5UVF43M5vBxWZSv9wL7Mvs2wa1v1oot"
    print object
    object   @?= Object
        (decode' "QmRKbLqN6DB9eZD5UVF43M5vBxWZSv9wL7Mvs2wa1v1oot")
        (pack [0x08, 0x01])
        [("file", Object
            (decode' "QmeVPgSQ5Hpq9hiqw9X8CCQx421Ei9SkmtzXyhkSHdcB4A")
            (pack [0x08, 0x02, 0x12, 0x0C, 0x48, 0x65, 0x6C, 0x6C, 0x6F, 0x20, 0x74, 0x65, 0x73, 0x74, 0x21, 0x0A, 0x18, 0x0C])
            [])
        ]

testNewObject :: Endpoint -> Assertion
testNewObject endpoint = do
    hash     <- fromJust <$> newObject endpoint Unixfs
    hash     @?= decode' "QmUNLLsPACCz1vLxQVkXqqLX5R1X345qqfHbsf67hvA3Nn"

testAddLink :: Endpoint -> Assertion
testAddLink endpoint = do
    hash     <- fromJust <$> addLink endpoint
        (decode' "QmUNLLsPACCz1vLxQVkXqqLX5R1X345qqfHbsf67hvA3Nn")
        (FileHash "file" $ decode' "QmeVPgSQ5Hpq9hiqw9X8CCQx421Ei9SkmtzXyhkSHdcB4A")
    hash     @?= decode' "QmRKbLqN6DB9eZD5UVF43M5vBxWZSv9wL7Mvs2wa1v1oot"

testRemoveLink :: Endpoint -> Assertion
testRemoveLink endpoint = do
    hash     <- fromJust <$> removeLink endpoint
        (decode' "QmRKbLqN6DB9eZD5UVF43M5vBxWZSv9wL7Mvs2wa1v1oot")
        "file"
    hash     @?= decode' "QmUNLLsPACCz1vLxQVkXqqLX5R1X345qqfHbsf67hvA3Nn"

testSetData :: Endpoint -> Assertion
testSetData endpoint = do
    hash     <- fromJust <$> setData endpoint
        (decode' "QmdfTbBqBPQ7VNxZEYEj14VmRuZBkqFbiwReogJgS1zR1n")
        (pack [0x08, 0x02, 0x12, 0x0C, 0x48, 0x65, 0x6C, 0x6C, 0x6F, 0x20, 0x74, 0x65, 0x73, 0x74, 0x21, 0x0A, 0x18, 0x0C])
    hash     @?= decode' "QmeVPgSQ5Hpq9hiqw9X8CCQx421Ei9SkmtzXyhkSHdcB4A"

testAppendData :: Endpoint -> Assertion
testAppendData endpoint = do
    hash     <- fromJust <$> appendData endpoint
        (decode' "QmeVPgSQ5Hpq9hiqw9X8CCQx421Ei9SkmtzXyhkSHdcB4A")
        (fromString "more data")
    hash     @?= decode' "QmUqH1JgUxLnUhcjqmfWDLDAbr6zNnqT9gmXZRJAjQH4Ae"

testAdd :: Endpoint -> Assertion
testAdd endpoint = do
    hash     <- fromJust <$> add endpoint (fromString "Hello test!\n")
    hash     @?= FileHash
        "QmeVPgSQ5Hpq9hiqw9X8CCQx421Ei9SkmtzXyhkSHdcB4A"
        (decode' "QmeVPgSQ5Hpq9hiqw9X8CCQx421Ei9SkmtzXyhkSHdcB4A")

testAddFile :: Endpoint -> Assertion
testAddFile endpoint = do
    hash     <- fromJust <$> addFile endpoint "tests/test_content/file1"
    hash     @?= FileHash
        "file1"
        (decode' "QmbFMke1KXqnYyBBWxB74N4c5SBnJMVAiMNRcGu6x1AwQH")

testAddFiles :: Endpoint -> Assertion
testAddFiles endpoint = do
    hash     <- fromJust <$> addFiles endpoint ["tests/test_content/file1", "tests/test_content/file2", "tests/test_content/file3"]
    hash     @?= [
            FileHash "file1" $ decode' "QmbFMke1KXqnYyBBWxB74N4c5SBnJMVAiMNRcGu6x1AwQH",
            FileHash "file2" $ decode' "QmbFMke1KXqnYyBBWxB74N4c5SBnJMVAiMNRcGu6x1AwQH",
            FileHash "file3" $ decode' "QmbFMke1KXqnYyBBWxB74N4c5SBnJMVAiMNRcGu6x1AwQH"
        ]

testAddDir :: Endpoint -> Assertion
testAddDir endpoint = do
    hash     <- fromJust <$> addDir endpoint "tests/test_content"
    hash     @?= FileHash
        "test_content"
        (decode' "QmbvqVQ6kgG8vwMamqhxgB7ET79iM9ToD7bdeRo9sWbgj5")

testLs :: Endpoint -> Assertion
testLs endpoint = do
    fileHashes <- fromJust <$> ls endpoint "QmbvqVQ6kgG8vwMamqhxgB7ET79iM9ToD7bdeRo9sWbgj5/dir1"
    fileHashes @?= [
            FileHash "dir2"  $ decode' "QmTb1wneppbtKYuR6EwJdnieGVyrtXz3VebYU2WG3GQ1Pz",
            FileHash "file1" $ decode' "QmbFMke1KXqnYyBBWxB74N4c5SBnJMVAiMNRcGu6x1AwQH",
            FileHash "file2" $ decode' "QmbFMke1KXqnYyBBWxB74N4c5SBnJMVAiMNRcGu6x1AwQH",
            FileHash "file3" $ decode' "QmbFMke1KXqnYyBBWxB74N4c5SBnJMVAiMNRcGu6x1AwQH"
        ]    

testMount :: Endpoint -> Assertion
testMount endpoint = do
    success <- mount endpoint Nothing Nothing
    assertBool "Failed to mount properly" success

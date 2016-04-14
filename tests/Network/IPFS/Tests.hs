module Network.IPFS.Tests(tests) where

import           Data.ByteString.Lazy
import           Data.ByteString.Lazy.UTF8 (fromString)
import           Network.IPFS
import           Test.HUnit                (Assertion, assertBool, (@?=))
import           Test.Tasty                (TestTree, testGroup)
import           Test.Tasty.HUnit          (testCase)

tests :: IPFS TestTree
tests = testGroup "Network.IPFS" <$> sequence [
        testCase  "testCat"        <$> testCat,
        -- testCase  "testGetID"      <$> testGetID,
        -- testCase  "testGetObject"  <$> testGetObject,
        testCase  "testNewObject"  <$> testNewObject,
        testCase  "testAddLink"    <$> testAddLink,
        testCase  "testRemoveLink" <$> testRemoveLink,
        testCase  "testSetData"    <$> testSetData,
        testCase  "testAppendData" <$> testAppendData,
        testCase  "testAdd"        <$> testAdd,
        testCase  "testAddFile"    <$> testAddFile,
        testCase  "testAddFiles"   <$> testAddFiles,
        testCase  "testAddDir"     <$> testAddDir,
        testCase  "testMount"      <$> testMount
        -- testCase  "testLs"         <$> testLs
    ]

testCat :: IPFS Assertion
testCat = do
    bytestring <- cat "QmeVPgSQ5Hpq9hiqw9X8CCQx421Ei9SkmtzXyhkSHdcB4A"
    return $ bytestring @?= fromString "Hello test!\n"

-- testGetID :: IPFS Assertion
-- testGetID = getID
    -- assertBool "Failed to get ID" $ isJust maybeID

testGetObject :: IPFS Assertion
testGetObject = do
    object <- getObject $ read "QmRKbLqN6DB9eZD5UVF43M5vBxWZSv9wL7Mvs2wa1v1oot"
    return $ object @?= Object
        (read "QmRKbLqN6DB9eZD5UVF43M5vBxWZSv9wL7Mvs2wa1v1oot")
        (pack [0x08, 0x01])
        [("file", Object
            (read "QmeVPgSQ5Hpq9hiqw9X8CCQx421Ei9SkmtzXyhkSHdcB4A")
            (pack [0x08, 0x02, 0x12, 0x0C, 0x48, 0x65, 0x6C, 0x6C, 0x6F, 0x20, 0x74, 0x65, 0x73, 0x74, 0x21, 0x0A, 0x18, 0x0C])
            [])
        ]

testNewObject :: IPFS Assertion
testNewObject = do
    hash <- newObject Unixfs
    return $ hash @?= read "QmUNLLsPACCz1vLxQVkXqqLX5R1X345qqfHbsf67hvA3Nn"

testAddLink :: IPFS Assertion
testAddLink = do
    hash <- addLink
        (read "QmUNLLsPACCz1vLxQVkXqqLX5R1X345qqfHbsf67hvA3Nn")
        (FileHash "file" $ read "QmeVPgSQ5Hpq9hiqw9X8CCQx421Ei9SkmtzXyhkSHdcB4A")
    return $ hash @?= read "QmRKbLqN6DB9eZD5UVF43M5vBxWZSv9wL7Mvs2wa1v1oot"

testRemoveLink :: IPFS Assertion
testRemoveLink = do
    hash <- removeLink
        (read "QmRKbLqN6DB9eZD5UVF43M5vBxWZSv9wL7Mvs2wa1v1oot")
        "file"
    return $ hash @?= read "QmUNLLsPACCz1vLxQVkXqqLX5R1X345qqfHbsf67hvA3Nn"

testSetData :: IPFS Assertion
testSetData = do
    hash <- setData
        (read "QmdfTbBqBPQ7VNxZEYEj14VmRuZBkqFbiwReogJgS1zR1n")
        (pack [0x08, 0x02, 0x12, 0x0C, 0x48, 0x65, 0x6C, 0x6C, 0x6F, 0x20, 0x74, 0x65, 0x73, 0x74, 0x21, 0x0A, 0x18, 0x0C])
    return $ hash @?= read "QmeVPgSQ5Hpq9hiqw9X8CCQx421Ei9SkmtzXyhkSHdcB4A"

testAppendData :: IPFS Assertion
testAppendData = do
    hash <- appendData
        (read "QmeVPgSQ5Hpq9hiqw9X8CCQx421Ei9SkmtzXyhkSHdcB4A")
        (fromString "more data")
    return $ hash @?= read "QmUqH1JgUxLnUhcjqmfWDLDAbr6zNnqT9gmXZRJAjQH4Ae"

testAdd :: IPFS Assertion
testAdd = do
    hash <- add (fromString "Hello test!\n")
    return $ hash @?= FileHash
        "QmeVPgSQ5Hpq9hiqw9X8CCQx421Ei9SkmtzXyhkSHdcB4A"
        (read "QmeVPgSQ5Hpq9hiqw9X8CCQx421Ei9SkmtzXyhkSHdcB4A")

testAddFile :: IPFS Assertion
testAddFile = do
    hash <- addFile "tests/test_content/file1"
    return $ hash @?= FileHash
        "file1"
        (read "QmbFMke1KXqnYyBBWxB74N4c5SBnJMVAiMNRcGu6x1AwQH")

testAddFiles :: IPFS Assertion
testAddFiles = do
    hash <- addFiles ["tests/test_content/file1", "tests/test_content/file2", "tests/test_content/file3"]
    return $ hash @?= [
            FileHash "file1" $ read "QmbFMke1KXqnYyBBWxB74N4c5SBnJMVAiMNRcGu6x1AwQH",
            FileHash "file2" $ read "QmbFMke1KXqnYyBBWxB74N4c5SBnJMVAiMNRcGu6x1AwQH",
            FileHash "file3" $ read "QmbFMke1KXqnYyBBWxB74N4c5SBnJMVAiMNRcGu6x1AwQH"
        ]

testAddDir :: IPFS Assertion
testAddDir = do
    hash <- addDir "tests/test_content"
    return $ hash @?= FileHash
        "test_content"
        (read "QmbvqVQ6kgG8vwMamqhxgB7ET79iM9ToD7bdeRo9sWbgj5")

testLs :: IPFS Assertion
testLs = do
    fileHashes <- ls "QmbvqVQ6kgG8vwMamqhxgB7ET79iM9ToD7bdeRo9sWbgj5/dir1"
    return $ fileHashes @?= [
            FileHash "dir2"  $ read "QmTb1wneppbtKYuR6EwJdnieGVyrtXz3VebYU2WG3GQ1Pz",
            FileHash "file1" $ read "QmbFMke1KXqnYyBBWxB74N4c5SBnJMVAiMNRcGu6x1AwQH",
            FileHash "file2" $ read "QmbFMke1KXqnYyBBWxB74N4c5SBnJMVAiMNRcGu6x1AwQH",
            FileHash "file3" $ read "QmbFMke1KXqnYyBBWxB74N4c5SBnJMVAiMNRcGu6x1AwQH"
        ]

testMount :: IPFS Assertion
testMount = do
    success <- mount Nothing Nothing
    return $ assertBool "Failed to mount properly" success

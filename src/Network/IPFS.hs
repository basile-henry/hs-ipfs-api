{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Network.IPFS (
    Object (..),
    Template (..),
    Hash,
    Data,
    cat,
    getNode,
    getObject,
    newObject,
    addLink,
    addFile,
    addDir,
    add
) where

import           Control.Monad                    (forM)
import           Data.Aeson                       (FromJSON (..), decode,
                                                   genericParseJSON, withObject,
                                                   (.:), (.:?))
import           Data.Aeson.Casing                (aesonPrefix)
import qualified Data.ByteString                  as B
import qualified Data.ByteString.Base58           as B58
import qualified Data.ByteString.Char8            as C
import qualified Data.ByteString.Lazy             as BL
import qualified Data.ByteString.Lazy.Char8       as LC
import qualified Data.ByteString.Lazy.UTF8        as LU
import qualified Data.ByteString.UTF8             as U
import           Data.Foldable                    (toList)
import           Data.Maybe                       (fromJust, maybeToList)
import           Data.Sequence                    (fromList)
import           GHC.Generics                     (Generic)
import           Network.IPFS.API                 (Content (..), Endpoint (..),
                                                   call, callWithContent)
import qualified Network.IPFS.MerkleDAG.Link      as PBL
import qualified Network.IPFS.MerkleDAG.Node      as PBN
import           System.Directory                 (doesDirectoryExist,
                                                   getDirectoryContents)
import           System.FilePath                  ((</>))
import           Text.ProtocolBuffers.Basic       (Utf8, uFromString, uToString)
import           Text.ProtocolBuffers.WireMessage (messageGet)

type Hash = B.ByteString -- TODO use multihash library

instance FromJSON Hash where
    parseJSON = (U.fromString <$>) . parseJSON

instance FromJSON LC.ByteString where
    parseJSON = (LU.fromString <$>) . parseJSON

instance FromJSON Utf8 where
    parseJSON = (uFromString <$>) . parseJSON

instance FromJSON PBN.Node where
    parseJSON = withObject "" $ \o -> PBN.Node
        <$> (fromList . maybeToList <$> o .:? "Links")
        <*> o .: "Data"

instance FromJSON PBL.Link where
    parseJSON = withObject "" $ \o -> PBL.Link
        <$> o .: "Hash"
        <*> o .: "Name"
        <*> o .: "Size"

type Data = B.ByteString

data Object = Object {
        objectHash    :: Hash,
        objectPayload :: Data,
        objectLinks   :: [(String, Object)]
    } deriving (Generic, Show)

getNode :: Endpoint -> Hash -> IO PBN.Node
getNode endpoint digest = do
    resp <- call endpoint
        ["object", "get"] [("encoding", "protobuf")]
        [C.unpack $ B58.encodeBase58 B58.bitcoinAlphabet digest]
    return $ case messageGet resp of
        Right (node, _) -> node
        Left err -> error err

getObject :: Endpoint -> Hash -> IO Object
getObject endpoint digest = do
    pbnode <- getNode endpoint digest
    let links' = toList $ PBN.links pbnode
        names = uToString . fromJust . PBL.name <$> links'
        data' = BL.toStrict . fromJust $ PBN.data' pbnode
    children <- mapM resolveLink links'
    return (Object digest data' $ zip names children)
    where resolveLink = getObject endpoint . BL.toStrict . fromJust . PBL.hash


cat :: Endpoint -> FilePath -> IO BL.ByteString
cat endpoint path = call endpoint ["cat"] [] [path]

data Template = Unixfs
              | None
            deriving Show

data GetHash = GetHash { getHash :: Hash } deriving (Generic, Show)

instance FromJSON GetHash where
    parseJSON = withObject "" $ \o -> GetHash <$> o .: "Hash"

newObject :: Endpoint -> Template -> IO (Maybe Hash)
newObject endpoint Unixfs = (getHash <$>)
    <$> decode
    <$> call endpoint ["object", "new", "unixfs-dir"] [] []
newObject endpoint None = (getHash <$>)
    <$> decode
    <$> call endpoint ["object", "new"] [] []

-- ipfs object patch
addLink :: Endpoint -> FileHash -> Hash -> IO (Maybe Hash)
addLink endpoint (FileHash name hash) root = (getHash <$>)
    <$> decode
    <$> call endpoint ["object", "patch", "add-link"] [] [U.toString root, name, U.toString hash]

data FileHash = FileHash {
        fileName :: FilePath,
        fileHash :: Hash
    } deriving (Generic, Show)

instance FromJSON FileHash where
   parseJSON = genericParseJSON $ aesonPrefix id

add :: Endpoint -> BL.ByteString -> IO (Maybe FileHash)
add endpoint raw = decode
    <$> callWithContent endpoint ["add"] [("q", "true")] [] (Raw raw)

addFile :: Endpoint -> FilePath -> IO (Maybe FileHash)
addFile endpoint path = decode
    <$> callWithContent endpoint ["add"] [("q", "true")] [] (File path)

getRecursiveContents :: FilePath -> IO [FilePath]
getRecursiveContents topdir = do
    names <- getDirectoryContents topdir
    let properNames = filter (`notElem` [".", ".."]) names
    paths <- forM properNames $ \name -> do
        let path = topdir </> name
        isDirectory <- doesDirectoryExist path
        if isDirectory
            then getRecursiveContents path
            else return [path]
    return (concat paths)

addDir :: Endpoint -> FilePath -> IO (Maybe [FileHash])
addDir endpoint path = do
    paths <- getRecursiveContents path
    bytestring <- callWithContent endpoint ["add"] [("r", "true"), ("q", "true")] [] (Dir paths)
    return $ mapM decode $ LC.lines bytestring

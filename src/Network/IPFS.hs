{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Network.IPFS (
    Hash,
    Data,
    Template (..),
    FileHash (..),
    Object (..),
    ID (..),
    encode,
    decode,
    cat,
    getID,
    ls,
    getObject,
    newObject,
    addLink,
    removeLink,
    setData,
    appendData,
    add,
    addFile,
    addFiles,
    addDir
) where

import           Control.Monad                    (foldM, forM)
import           Data.Aeson                       (FromJSON (..),
                                                   (.:), (.:?))
import qualified Data.Aeson                       as JSON
import           Data.Aeson.Types                 (Parser)
import           Data.ByteString.Lazy             (ByteString, fromStrict,
                                                   toStrict)
import           Data.ByteString.Lazy.UTF8        (fromString, lines, toString)
import qualified Data.ByteString.UTF8             as U
import           Data.Foldable                    (toList)
import           Data.Maybe                       (fromJust, maybeToList)
import qualified Data.Multihash.Base              as MB
import qualified Data.Multihash.Digest            as MD
import           Data.Sequence                    (fromList)
import           Data.Text                        (unpack)
import           GHC.Generics                     (Generic)
import           Network.IPFS.API                 (Content (..), Endpoint (..),
                                                   call, callWithContent)
import qualified Network.IPFS.MerkleDAG.PBLink    as PBL
import qualified Network.IPFS.MerkleDAG.PBNode    as PBN
import qualified Network.MultiAddr                as MA
import           Prelude                          hiding (lines)
import           System.Directory                 (doesDirectoryExist,
                                                   getDirectoryContents)
import           System.FilePath                  (splitPath, (</>))
import           Text.ProtocolBuffers.Basic       (Utf8, uFromString, uToString)
import           Text.ProtocolBuffers.WireMessage (messageGet)

type Hash     = MD.MultihashDigest

instance FromJSON Hash where
    parseJSON (JSON.String s) = either (fail "Expected a Hash String") return $ decode . unpack $ s
    parseJSON _               = fail "Expected a Hash String"

type Key      = ByteString -- Is it a multihash?
type Data     = ByteString
data Template = Unixfs | None deriving Show
data GetHash  = GetHash { getHash :: Hash } deriving (Generic, Show)

instance FromJSON GetHash where
    parseJSON (JSON.Object o) = GetHash <$> o .: "Hash"
    parseJSON _               = fail "Expected a GetHash"


data FileHash = FileHash {
        fileName :: FilePath,
        fileHash :: Hash
    } deriving (Generic, Show, Eq)

instance FromJSON FileHash where
    parseJSON (JSON.Object o) = FileHash
        <$> o .: "Name"
        <*> o .: "Hash"
    parseJSON _               = fail "Expected a FileHash"

data Link   = Link {
        hash :: Maybe Hash,
        name :: Maybe FilePath,
        size :: Maybe Int
    } deriving (Generic, Eq, Show)

instance FromJSON Link where
    parseJSON (JSON.Object o) = Link
        <$> o .:? "Hash"
        <*> o .:? "Name"
        <*> o .:? "Size"
    parseJSON _              = fail "Expected a Link"

data Node   = Node {
        links   :: [Link],
        payload :: Maybe Data
    } deriving (Generic, Show, Eq)

instance FromJSON Node where
    parseJSON (JSON.Object o) = Node
        <$> o .:  "Links"
        <*> o .:? "Data"
    parseJSON _              = fail "Expected a Node"

data Object = Object {
        objectHash    :: Hash,
        objectPayload :: Data,
        objectLinks   :: [(String, Object)]
    } deriving (Generic, Show, Eq)

data ID = ID {
        idHash          :: Hash,
        publicKey       :: Key,
        addresses       :: [MA.MultiAddr], -- TODO replace with multiaddresses ?
        agentVersion    :: String,
        protocolVersion :: String
    } deriving (Generic, Show, Eq)

instance FromJSON ID where
    parseJSON (JSON.Object o) = ID
        <$> o .: "ID"
        <*> o .: "PublicKey"
        <*> parseAddresses o
        <*> o .: "AgentVersion"
        <*> o .: "ProtocolVersion"
    parseJSON _               = fail "Expected an ID"

parseAddresses :: JSON.Object -> Parser [MA.MultiAddr]
parseAddresses o = (maybe
    (fail "Could parse MultiAddr")
    return
    =<< sequence . map MA.fromString) <$> (o .: "Addresses")

instance FromJSON ByteString where
    parseJSON = (fromString <$>) . parseJSON

encode :: Hash -> String
encode hash = toString . MB.encode MB.Base58 $ MD.encode (MD.algorithm hash) (MD.digest hash)

decode :: String -> Either String Hash
decode string = (MD.decode . toStrict) =<< (MB.decode MB.Base58 $ fromString string)

-- | = cat

cat :: Endpoint -> FilePath -> IO ByteString
cat endpoint path = call endpoint ["cat"] [] [path]

-- | = id

getID :: Endpoint -> IO (Maybe ID)
getID endpoint = JSON.decode <$> call endpoint ["id"] [] []

-- | = ls

ls :: Endpoint -> FilePath -> IO (Maybe [FileHash])
ls = undefined
-- ls endpoint path = do
--     list <- call endpoint ["ls"] [] [path]
--     print list
--     return $ JSON.decode list

-- | = object

getNode :: Endpoint -> Hash -> IO PBN.PBNode
getNode endpoint hash = do
    resp <- call endpoint
        ["object", "get"] [("encoding", "protobuf")]
        [encode hash]
    return $ case messageGet resp of
        Right (node, _) -> node
        Left err -> error err

getObject :: Endpoint -> Hash -> IO Object
getObject = undefined
-- getObject endpoint hash = do
--     pbnode <- getNode endpoint hash
--     let links' = toList $ PBN.links pbnode
--         names = uToString . fromJust . PBL.name <$> links'
--         data' = fromJust $ PBN.data' pbnode
--     children <- mapM resolveLink links'
--     return (Object hash data' $ zip names children)
--     where resolveLink = getObject endpoint . fromJust . PBL.hash

newObject :: Endpoint -> Template -> IO (Maybe Hash)
newObject endpoint Unixfs = (getHash <$>)
    <$> JSON.decode
    <$> call endpoint ["object", "new", "unixfs-dir"] [] []
newObject endpoint None = (getHash <$>)
    <$> JSON.decode
    <$> call endpoint ["object", "new"] [] []

-- Should PBL.PBLink be exported?
-- getLinks :: Endpoint -> Hash -> IO (Maybe [PBL.PBLink])

-- | == object patch

addLink :: Endpoint -> Hash -> FileHash -> IO (Maybe Hash)
addLink endpoint root (FileHash name hash) = (getHash <$>)
    <$> JSON.decode
    <$> call endpoint ["object", "patch", "add-link"] [] [encode root, name, encode hash]

removeLink :: Endpoint -> Hash -> FilePath -> IO (Maybe Hash)
removeLink endpoint root name = (getHash <$>)
    <$> JSON.decode
    <$> call endpoint ["object", "patch", "rm-link"] [] [encode root, name]

setData :: Endpoint -> Hash -> Data -> IO (Maybe Hash)
setData endpoint root data' = (getHash <$>)
    <$> JSON.decode
    <$> callWithContent endpoint ["object", "patch", "set-data"] [] [encode root] (Raw data')

appendData :: Endpoint -> Hash -> Data -> IO (Maybe Hash)
appendData endpoint root data' = (getHash <$>)
    <$> JSON.decode
    <$> callWithContent endpoint ["object", "patch", "append-data"] [] [encode root] (Raw data')

-- | = add

add :: Endpoint -> ByteString -> IO (Maybe FileHash)
add endpoint raw = JSON.decode
    <$> callWithContent endpoint ["add"] [("q", "true")] [] (Raw raw)

addFile :: Endpoint -> FilePath -> IO (Maybe FileHash)
addFile endpoint path = JSON.decode
    <$> callWithContent endpoint ["add"] [("q", "true")] [] (File path)

addFiles :: Endpoint -> [FilePath] -> IO (Maybe [FileHash])
addFiles endpoint paths = (mapM JSON.decode)
    <$> lines
    <$> callWithContent endpoint ["add"] [("q", "true")] [] (Files paths)

addDir :: Endpoint -> FilePath -> IO (Maybe FileHash)
addDir endpoint topdir = do
    doesDirectoryExist topdir >>= (\case
        False -> return Nothing
        True  -> do
            names <- getDirectoryContents topdir
            let properNames = filter (`notElem` [".", ".."]) names
            fileHashes <- forM properNames $ \name -> do
                let path = topdir </> name
                isDirectory <- doesDirectoryExist path
                if isDirectory
                    then addDir  endpoint path
                    else addFile endpoint path
            root <- newObject endpoint Unixfs
            hash <- foldM applyHash root fileHashes
            return $ FileHash (last . splitPath $ topdir) <$> hash)
    where
        applyHash :: Maybe Hash -> Maybe FileHash -> IO (Maybe Hash)
        applyHash Nothing _         = return Nothing
        applyHash _       Nothing   = return Nothing
        applyHash (Just r) (Just f) = addLink endpoint r f

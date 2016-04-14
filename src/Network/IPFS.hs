{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Network.IPFS (
    IPFS (..),
    Endpoint (..),
    Hash,
    Data,
    Template (..),
    FileHash (..),
    Object (..),
    ID (..),
    runIPFS,
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
    addDir,
    mount
) where

import           Control.Monad                    (foldM, forM, liftM2)
import           Control.Monad.IO.Class           (liftIO)
import           Control.Monad.Trans.Reader       (runReaderT)
import           Data.Aeson                       (FromJSON (..), (.:), (.:?))
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
import           Network.IPFS.API                 (Content (..), call,
                                                   callWithContent)
import qualified Network.IPFS.MerkleDAG.PBLink    as PBL
import qualified Network.IPFS.MerkleDAG.PBNode    as PBN
import           Network.IPFS.Types               (Endpoint (..), IPFS (..))
import           Network.Multiaddr                (Multiaddr)
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
        addresses       :: [Multiaddr], -- TODO replace with multiaddresses ?
        agentVersion    :: String,
        protocolVersion :: String
    } deriving (Generic, Show, Eq)

instance FromJSON ID where
    parseJSON (JSON.Object o) = ID
        <$> o .: "ID"
        <*> o .: "PublicKey"
        <*> (map read <$> o .: "Addresses")
        <*> o .: "AgentVersion"
        <*> o .: "ProtocolVersion"
    parseJSON _               = fail "Expected an ID"

instance FromJSON ByteString where
    parseJSON = (fromString <$>) . parseJSON

encode :: Hash -> String
encode hash = toString . MB.encode MB.Base58 $ MD.encode (MD.algorithm hash) (MD.digest hash)

decode :: String -> Either String Hash
decode string = (MD.decode . toStrict) =<< (MB.decode MB.Base58 $ fromString string)

-- | = cat

cat :: FilePath -> IPFS ByteString
cat path = call ["cat"] [] [path]

-- | = id

getID :: IPFS ID
getID = maybe (error "Failed to retrieve ID") id . JSON.decode
    <$> call ["id"] [] []

-- | = ls

ls :: FilePath -> IPFS [FileHash]
ls = undefined
-- ls path = do
--     list <- call ["ls"] [] [path]
--     print list
--     return $ JSON.decode list

-- | = object

getNode :: Hash -> IPFS PBN.PBNode
getNode hash = do
    resp <- call
        ["object", "get"] [("encoding", "protobuf")]
        [encode hash]
    return $ case messageGet resp of
        Right (node, _) -> node
        Left err -> error err

getObject :: Hash -> IPFS Object
getObject = undefined
-- getObject hash = do
--     pbnode <- getNode hash
--     let links' = toList $ PBN.links pbnode
--         names = uToString . fromJust . PBL.name <$> links'
--         data' = fromJust $ PBN.data' pbnode
--     children <- mapM resolveLink links'
--     return (Object hash data' $ zip names children)
--     where resolveLink = getObject fromJust . PBL.hash

newObject :: Template -> IPFS Hash
newObject t = getHash
    <$> maybe (error "Failed to create new object.") id . JSON.decode
    <$> call args [] []
    where
        args :: [String]
        args = ["object", "new"] ++ case t of
            Unixfs -> ["unixfs-dir"]
            None   -> []

-- Should PBL.PBLink be exported?
-- getLinks :: Endpoint -> Hash -> IO (Maybe [PBL.PBLink])

-- | == object patch

addLink :: Hash -> FileHash -> IPFS Hash
addLink root (FileHash name hash) = getHash
    <$> maybe (error "Failed to add link.") id . JSON.decode
    <$> call ["object", "patch", "add-link"] [] [encode root, name, encode hash]

removeLink :: Hash -> FilePath -> IPFS Hash
removeLink root name = getHash
    <$> maybe (error "Failed to remove link.") id . JSON.decode
    <$> call ["object", "patch", "rm-link"] [] [encode root, name]

setData :: Hash -> Data -> IPFS Hash
setData root data' = getHash
    <$> maybe (error "Failed to set data.") id . JSON.decode
    <$> callWithContent ["object", "patch", "set-data"] [] [encode root] (Raw data')

appendData :: Hash -> Data -> IPFS Hash
appendData root data' = getHash
    <$> maybe (error "Failed to append data.") id . JSON.decode
    <$> callWithContent ["object", "patch", "append-data"] [] [encode root] (Raw data')

-- | = add

add :: ByteString -> IPFS FileHash
add raw = maybe (error "Failed to add.") id . JSON.decode
    <$> callWithContent ["add"] [("q", "true")] [] (Raw raw)

addFile :: FilePath -> IPFS FileHash
addFile path = maybe (error "Failed to add file.") id . JSON.decode
    <$> callWithContent ["add"] [("q", "true")] [] (File path)

addFiles :: [FilePath] -> IPFS [FileHash]
addFiles paths = maybe (error "Failed to add files.") id . (mapM JSON.decode)
    <$> lines
    <$> callWithContent ["add"] [("q", "true")] [] (Files paths)

addDir :: FilePath -> IPFS FileHash
addDir topdir = do
    isDir <- liftIO $ doesDirectoryExist topdir
    if isDir
        then do
            names <- liftIO $ getDirectoryContents topdir
            let properNames = filter (`notElem` [".", ".."]) names
            fileHashes <- forM properNames $ \name -> do
                let path = topdir </> name
                isDirectory <- liftIO $ doesDirectoryExist path
                if isDirectory
                    then addDir  path
                    else addFile path
            root <- newObject Unixfs
            hash <- foldM addLink root fileHashes
            return $ FileHash (last . splitPath $ topdir) hash
        else error $ "Directory " ++ show topdir ++ " does not exist."

-- | = mount

data MountData = MountData {
        ipfsPath :: Maybe FilePath,
        ipnsPath :: Maybe FilePath,
        message  :: Maybe String
    } deriving (Generic, Show, Eq)

instance FromJSON MountData where
    parseJSON (JSON.Object o) = MountData
        <$> o .:? "IPFS"
        <*> o .:? "IPNS"
        <*> o .:? "Message"
    parseJSON _               = fail "Expected a MountData json"

mount :: Maybe FilePath -> Maybe FilePath -> IPFS Bool
mount ipfs ipns = checkMountData
    <$> JSON.decode
    <$> call ["mount"] options []
    where
        options :: [(String, String)]
        options = (maybeToList $ sequence ("f", ipfs))
            ++ (maybeToList $ sequence ("n", ipns))

        checkMountData :: Maybe MountData -> Bool
        checkMountData (Just (MountData (Just _) (Just _) Nothing)) = True
        checkMountData _                                            = False

runIPFS :: Endpoint -> IPFS a -> IO a
runIPFS endpoint (IPFS reader) = runReaderT reader endpoint

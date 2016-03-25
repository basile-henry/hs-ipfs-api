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
    cat,
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
import           Data.Aeson                       (FromJSON (..), decode,
                                                   withObject, (.:), (.:?))
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
import           System.FilePath                  (splitPath, (</>))
import           Text.ProtocolBuffers.Basic       (Utf8, uFromString, uToString)
import           Text.ProtocolBuffers.WireMessage (messageGet)

type Hash     = B.ByteString -- TODO use multihash library
type Data     = B.ByteString
data Template = Unixfs | None deriving Show
data GetHash  = GetHash { getHash :: Hash } deriving (Generic, Show)
data FileHash = FileHash {
        fileName :: FilePath,
        fileHash :: Hash
    } deriving (Generic, Show, Eq)

data Object = Object {
        objectHash    :: Hash,
        objectPayload :: Data,
        objectLinks   :: [(String, Object)]
    } deriving (Generic, Show, Eq)

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

instance FromJSON GetHash where
    parseJSON = withObject "" $ \o -> GetHash <$> o .: "Hash"

instance FromJSON FileHash where
    parseJSON = withObject "" $ \o -> FileHash
        <$> o .: "Name"
        <*> o .: "Hash"

cat :: Endpoint -> FilePath -> IO BL.ByteString
cat endpoint path = call endpoint ["cat"] [] [path]


-- == ipfs object

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



newObject :: Endpoint -> Template -> IO (Maybe Hash)
newObject endpoint Unixfs = (getHash <$>)
    <$> decode
    <$> call endpoint ["object", "new", "unixfs-dir"] [] []
newObject endpoint None = (getHash <$>)
    <$> decode
    <$> call endpoint ["object", "new"] [] []

-- === ipfs object patch
addLink :: Endpoint -> Hash -> FileHash -> IO (Maybe Hash)
addLink endpoint root (FileHash name hash) = (getHash <$>)
    <$> decode
    <$> call endpoint ["object", "patch", "add-link"] [] [U.toString root, name, U.toString hash]

removeLink :: Endpoint -> Hash -> FilePath -> IO (Maybe Hash)
removeLink endpoint root name = (getHash <$>)
    <$> decode
    <$> call endpoint ["object", "patch", "rm-link"] [] [U.toString root, name]

setData :: Endpoint -> Hash -> Data -> IO (Maybe Hash)
setData endpoint root data' = (getHash <$>)
    <$> decode
    <$> callWithContent endpoint ["object", "patch", "set-data"] [] [U.toString root] (Raw $ BL.fromStrict data')

appendData :: Endpoint -> Hash -> Data -> IO (Maybe Hash)
appendData endpoint root data' = (getHash <$>)
    <$> decode
    <$> callWithContent endpoint ["object", "patch", "append-data"] [] [U.toString root] (Raw $ BL.fromStrict data')


-- == ipfs add

add :: Endpoint -> BL.ByteString -> IO (Maybe FileHash)
add endpoint raw = decode
    <$> callWithContent endpoint ["add"] [("q", "true")] [] (Raw raw)

addFile :: Endpoint -> FilePath -> IO (Maybe FileHash)
addFile endpoint path = decode
    <$> callWithContent endpoint ["add"] [("q", "true")] [] (File path)

addFiles :: Endpoint -> [FilePath] -> IO (Maybe [FileHash])
addFiles endpoint paths = (mapM decode)
    <$> LC.lines
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

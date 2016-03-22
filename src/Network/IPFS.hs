module Network.IPFS where

import           Control.Monad                    (forM)
import qualified Data.ByteString                  as B
import qualified Data.ByteString.Base58           as B58
import qualified Data.ByteString.Char8            as C
import qualified Data.ByteString.Lazy             as BL
import           Data.Foldable                    (toList)
import           Data.Maybe                       (fromJust)
import qualified Network.IPFS.API                 as API
import qualified Network.IPFS.MerkleDAG.PBLink    as PBL
import qualified Network.IPFS.MerkleDAG.PBNode    as PBN
import           System.Directory                 (doesDirectoryExist,
                                                   getDirectoryContents)
import           System.FilePath                  ((</>))
import           Text.ProtocolBuffers.Basic       (uToString)
import           Text.ProtocolBuffers.WireMessage (messageGet)

type Hash = B.ByteString -- TODO use multihash library
type Data = B.ByteString

data Object = Object {
        hash    :: Hash,
        payload :: Data,
        links   :: [(String, Object)]
    } deriving (Show)

cat :: API.Endpoint -> FilePath -> IO BL.ByteString
cat endpoint path = API.call endpoint ["cat"] [] [path] API.Empty

getPBNode :: API.Endpoint -> Hash -> IO PBN.PBNode
getPBNode endpoint digest = do
    resp <- API.call endpoint
        ["object", "get"] [("encoding", "protobuf")]
        [C.unpack $ B58.encodeBase58 B58.bitcoinAlphabet digest]
        API.Empty
    return $ case messageGet resp of
        Right (node, _) -> node
        Left err -> error err

getObject :: API.Endpoint -> Hash -> IO Object
getObject endpoint digest = do
    pbnode <- getPBNode endpoint digest
    let links' = toList $ PBN.links pbnode
        names = uToString . fromJust . PBL.name <$> links'
        data' = BL.toStrict . fromJust $ PBN.data' pbnode
    children <- mapM resolveLink links'
    return (Object digest data' $ zip names children)
    where resolveLink = getObject endpoint . BL.toStrict . fromJust . PBL.hash

addFile :: API.Endpoint -> FilePath -> IO B.ByteString
addFile endpoint path = do
    resp <- API.call endpoint ["add"] [("q", "true")] [] (API.File path)
    return $ getLastHash resp


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

addDir :: API.Endpoint -> FilePath -> IO B.ByteString
addDir endpoint path = do
    paths <- getRecursiveContents path
    resp  <- API.call endpoint ["add"] [("r", "true"), ("q", "true")] [] (API.Dir paths)
    return $ getLastHash resp

add :: API.Endpoint -> BL.ByteString -> IO B.ByteString
add endpoint raw = do
    resp <- API.call endpoint ["add"] [("q", "true")] [] (API.Raw raw)
    return $ getLastHash resp

-- Ugly temporary method, a solution like using Aeson to parse the json output
-- might be more appropriate
getLastHash :: BL.ByteString -> B.ByteString
getLastHash = last . init . B.split (B.head $ C.singleton '"') . BL.toStrict
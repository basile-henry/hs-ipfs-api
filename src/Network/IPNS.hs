module Network.IPNS(resolve, publish) where

-- import qualified Data.ByteString      as B
import           Data.ByteString.Lazy (ByteString)
import           Network.IPFS.API     (Content (..), Endpoint (..), call)

-- type Hash = B.ByteString -- TODO use multihash library

resolve :: Endpoint -> FilePath -> IO ByteString
resolve endpoint path = call endpoint ["name", "resolve"] [("encoding", "json")] [path] Empty

publish :: Endpoint -> FilePath -> IO ByteString
publish endpoint path = call endpoint ["name", "publish"] [] [path] Empty

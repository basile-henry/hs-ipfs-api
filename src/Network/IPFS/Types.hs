{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Network.IPFS.Types (
    IPFS (..),
    Endpoint (..),
    Multihash (..),
    Key,
    Data,
    Template (..),
    FileHash (..),
    Link (..),
    Node (..),
    Object (..),
    ID (..),
    parseMultihash
) where

import           Control.Monad                (ap, liftM)
import           Control.Monad.Fix            (MonadFix, mfix)
import           Control.Monad.IO.Class       (MonadIO, liftIO)
import           Control.Monad.Trans.Reader   (ReaderT)
import           Data.Aeson                   (FromJSON (..), (.:), (.:?))
import qualified Data.Aeson                   as JSON
import           Data.ByteString.Lazy         (ByteString)
import           Data.ByteString.Lazy.UTF8    (fromString, toString)
import qualified Data.Multihash.Base          as MB
import           Data.Text                    (unpack)
import           GHC.Generics                 (Generic)
import qualified Network.HTTP.Conduit         as HTTP
import           Network.Multiaddr            (Multiaddr)
import           Text.ParserCombinators.ReadP (ReadP, readP_to_S, munch1)

-- | An 'Endpoint' is an IPFS node that will execute an API request
data Endpoint = Endpoint HTTP.Manager String

newtype IPFS a = IPFS { unIPFS :: ReaderT Endpoint IO a }

instance Monad IPFS where
    return = IPFS . return
    m >>= f = IPFS (unIPFS m >>= unIPFS . f)

instance MonadFix IPFS where
    mfix f = IPFS (mfix (unIPFS . f))

instance MonadIO IPFS where
    liftIO = IPFS . liftIO

instance Functor IPFS where
    fmap = liftM

instance Applicative IPFS where
    pure = return
    (<*>) = ap

newtype Multihash = Multihash { multihash :: ByteString } deriving (Generic, Eq)

instance Show Multihash where
    show = toString . MB.encode MB.Base58 . multihash

instance Read Multihash where
    readsPrec _ = readP_to_S parseMultihash

parseMultihash :: ReadP Multihash
parseMultihash = do
    base58 <- munch1 (`elem` ("123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz" :: String)) -- base58 bitcoin alphabet
    either fail (return . Multihash) . MB.decode MB.Base58 . fromString $ base58
            
instance FromJSON Multihash where
    parseJSON (JSON.String s) = return $ read . unpack $ s
    parseJSON _               = fail "Expected a Multihash String"

type Key      = ByteString
type Data     = ByteString
data Template = Unixfs | None deriving (Show, Eq)

data FileHash = FileHash {
        fileName :: FilePath,
        fileHash :: Multihash
    } deriving (Generic, Show, Eq)

instance FromJSON FileHash where
    parseJSON (JSON.Object o) = FileHash
        <$> o .: "Name"
        <*> o .: "Hash"
    parseJSON _               = fail "Expected a FileHash"

data Link   = Link {
        hash :: Maybe Multihash,
        name :: Maybe FilePath,
        size :: Maybe Int
    } deriving (Generic, Eq, Show)

instance FromJSON Link where
    parseJSON (JSON.Object o) = Link
        <$> o .:? "Hash"
        <*> o .:? "Name"
        <*> o .:? "Size"
    parseJSON _               = fail "Expected a Link"

data Node   = Node {
        links   :: [Link],
        payload :: Maybe Data
    } deriving (Generic, Show, Eq)

instance FromJSON Node where
    parseJSON (JSON.Object o) = Node
        <$> o .:  "Links"
        <*> o .:? "Data"
    parseJSON _               = fail "Expected a Node"

data Object = Object {
        objectHash    :: Multihash,
        objectPayload :: Data,
        objectLinks   :: [(String, Object)]
    } deriving (Generic, Show, Eq)

data ID = ID {
        idHash          :: Multihash,
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

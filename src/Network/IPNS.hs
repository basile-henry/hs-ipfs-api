{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : IPNS
Description : IPFS namespace (IPNS) library.
Copyright   : (c) Basile Henry, 2016
License     : MIT
Maintainer  : basile.henry@hotmail.com
Stability   : experimental

IPNS is a PKI namespace, where names are the hashes of public keys, and
the private key enables publishing new (signed) values. In both publish
and resolve, the default value of \<name\> is your own identity public key.

Examples:

Publish an \<ipfs-path\> to your identity name:

>>> publish "/ipfs/QmatmE9msSfkKxoffpHwNLNKgwZG8eT9Bud6YoPab52vpy"
> "QmbCMUZw6JFeZ7Wp9jkzbye3Fzp2GGcPgC3nmeUjfVF87n"

Resolve the value of your identity:

>>> resolve localEndpoint
> Just "/ipfs/QmatmE9msSfkKxoffpHwNLNKgwZG8eT9Bud6YoPab52vpy"

Resolve the value of another name:

>>> resolvePath localEndpoint "QmbCMUZw6JFeZ7Wp9jkzbye3Fzp2GGcPgC3nmeUjfVF87n"
> Just "/ipfs/QmatmE9msSfkKxoffpHwNLNKgwZG8eT9Bud6YoPab52vpy"
-}

module Network.IPNS (
    resolve,
    resolvePath,
    publish
) where

import           Data.Aeson         (FromJSON (..), decode, withObject, (.:))
import           GHC.Generics       (Generic)
import           Network.IPFS.API   (call)
import           Network.IPFS.Types (IPFS (..))

data Resolve = Resolve { path :: FilePath } deriving (Generic, Show)

instance FromJSON Resolve where
   parseJSON = withObject "" $ \o -> Resolve <$> o .: "Path"

data Publish = Publish { name :: FilePath } deriving (Generic, Show)

instance FromJSON Publish where
   parseJSON = withObject "" $ \o -> Publish <$> o .: "Name"

-- | Recursively resolve the IPNS of the current `Endpoint`
resolve :: IPFS FilePath
resolve = path
    <$> maybe (error "Could not resolve name.") id . decode
    <$> call ["name", "resolve"] [("r", "true")] []

-- | Recursively resolve an IPNS file path
resolvePath :: FilePath -> IPFS FilePath
resolvePath filePath = path
    <$> maybe (error "Could not resolve name.") id . decode
    <$> call ["name", "resolve"] [("r", "true")] [filePath]

-- | Publish an object to IPNS. It publishes the object given by 'Filepath' to the 'Endpoint' 's IPNS
publish :: FilePath -> IPFS FilePath
publish filePath = name
    <$> maybe (error "Could not resolve name.") id . decode
    <$> call ["name", "publish"] [] [filePath]

-- Publish an object to IPNS. It publishes object given by 'Filepath' to the second 'FilePath'
--
-- On success returns 'Just' 'FilePath'
--
-- On failure returns 'Nothing'
--
-- NB: not yet implemented in the go-ipfs daemon
-- publishTo :: Endpoint -> FilePath -> FilePath -> IO (Maybe FilePath)
-- publishTo endpoint filePath to = (name <$>)
--     <$> decode
--     <$> call endpoint ["name", "publish"] [] [filePath, to]

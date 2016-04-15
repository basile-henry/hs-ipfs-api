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

>>> resolve
> Just "/ipfs/QmatmE9msSfkKxoffpHwNLNKgwZG8eT9Bud6YoPab52vpy"

Resolve the value of another name:

>>> resolvePath "/ipns/QmbCMUZw6JFeZ7Wp9jkzbye3Fzp2GGcPgC3nmeUjfVF87n"
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
import           Network.IPFS.Path  (Path (..))

data Resolve = Resolve { path :: Path } deriving (Generic, Show)

instance FromJSON Resolve where
   parseJSON = withObject "" $ \o -> Resolve . read <$> o .: "Path"

data Publish = Publish { name :: Path } deriving (Generic, Show)

instance FromJSON Publish where
   parseJSON = withObject "" $ \o -> Publish . read <$> o .: "Name"

-- | Recursively resolve the IPNS of the current `Endpoint`
resolve :: IPFS Path
resolve = path
    <$> maybe (error "Could not resolve name.") id . decode
    <$> call ["name", "resolve"] [("r", "true")] []

-- | Recursively resolve an IPNS file path
resolvePath :: Path -> IPFS Path
resolvePath (PathIPFS _ _) = error "Can only resolve an IPNS path."
resolvePath pathIPNS       = path
    <$> maybe (error "Could not resolve name.") id . decode
    <$> call ["name", "resolve"] [("r", "true")] [show pathIPNS]

-- | Publish an object to IPNS. It publishes the object given by 'Filepath' to the 'Endpoint' 's IPNS
publish :: Path -> IPFS Path
publish pubPath = name
    <$> maybe (error "Could not resolve name.") id . decode
    <$> call ["name", "publish"] [] [show pubPath]

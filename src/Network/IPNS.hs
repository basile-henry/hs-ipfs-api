{-# LANGUAGE DeriveGeneric #-}

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

>>> publish localEndpoint "/ipfs/QmatmE9msSfkKxoffpHwNLNKgwZG8eT9Bud6YoPab52vpy"
> Just "QmbCMUZw6JFeZ7Wp9jkzbye3Fzp2GGcPgC3nmeUjfVF87n"

Publish an \<ipfs-path\> to another public key:

>>> publishTo localEndpoint "/ipfs/QmatmE9msSfkKxoffpHwNLNKgwZG8eT9Bud6YoPab52vpy" "QmbCMUZw6JFeZ7Wp9jkzbye3Fzp2GGcPgC3nmeUjfVF87n"
> Just "QmbCMUZw6JFeZ7Wp9jkzbye3Fzp2GGcPgC3nmeUjfVF87n"

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
    publish,
    publishTo
) where

import           Data.Aeson        (FromJSON (..), decode, genericParseJSON)
import           Data.Aeson.Casing (aesonDrop)
import           GHC.Generics      (Generic)
import           Network.IPFS.API  (Endpoint, call)

data Resolve = Resolve { _Path :: FilePath } deriving (Generic, Show)

instance FromJSON Resolve where
   parseJSON = genericParseJSON $ aesonDrop 1 id

-- | Recursively resolve the IPNS of the Endpoint
--
-- On success returns 'Just' 'FilePath'
--
-- On failure returns 'Nothing'
resolve :: Endpoint -> IO (Maybe FilePath)
resolve endpoint = (_Path <$>)
    <$> decode
    <$> call endpoint ["name", "resolve"] [("r", "true")] []

-- | Recursively resolve an IPNS file path
--
-- On success returns 'Just' 'FilePath'
--
-- On failure returns 'Nothing'
resolvePath :: Endpoint -> FilePath -> IO (Maybe FilePath)
resolvePath endpoint path = (_Path <$>)
    <$> decode
    <$> call endpoint ["name", "resolve"] [("r", "true")] [path]


-- TODO change the type of _Name to be a Hash
data Publish = Publish { _Name :: FilePath } deriving (Generic, Show)

instance FromJSON Publish where
   parseJSON = genericParseJSON $ aesonDrop 1 id

-- | Publish an object to IPNS. It publishes object given by 'Filepath' to the 'Endpoint' 's IPNS
--
-- On success returns 'Just' 'FilePath'
-- where the 'FilePath' is where the input 'FilePath' has been published to.
--
-- On failure returns 'Nothing'
publish :: Endpoint -> FilePath -> IO (Maybe FilePath)
publish endpoint path = (_Name <$>)
    <$> decode
    <$> call endpoint ["name", "publish"] [] [path]

-- | Publish an object to IPNS. It publishes object given by 'Filepath' to the second 'FilePath'
--
-- On success returns 'Just' 'FilePath'
--
-- On failure returns 'Nothing'
publishTo :: Endpoint -> FilePath -> FilePath -> IO (Maybe FilePath)
publishTo endpoint path to = (_Name <$>)
    <$> decode
    <$> call endpoint ["name", "publish"] [] [path, to]

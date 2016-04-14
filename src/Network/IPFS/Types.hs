module Network.IPFS.Types (
    IPFS (..),
    Endpoint (..)
) where

import           Control.Monad              (ap, liftM)
import           Control.Monad.Fix          (MonadFix, mfix)
import           Control.Monad.IO.Class     (MonadIO, liftIO)
import           Control.Monad.Trans.Reader (ReaderT)
import qualified Network.HTTP.Conduit       as HTTP

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

{-# LANGUAGE OverloadedStrings #-}

module Network.IPFS.API where

import           Blaze.ByteString.Builder               (Builder, toByteString)
import           Data.ByteString.Lazy                   (ByteString)
import qualified Data.ByteString.UTF8                   as U
import           Data.Maybe                             (fromJust)
import qualified Data.Text                              as T
import qualified Network.HTTP.Conduit                   as HTTP
import qualified Network.HTTP.Client.MultipartFormData as MFD
import qualified Network.HTTP.Types.URI                 as URI

-- | An 'Endpoint' is an IPFS node that will execute an API request
data Endpoint = Endpoint HTTP.Manager String

-- | To initialize the API at an IPFS node, the Endpoint
-- For example (in a do block):
-- > endpoint <- init "http://localhost:5001"
initEndpoint :: String -> IO Endpoint
initEndpoint host = do
    manager <- HTTP.newManager HTTP.tlsManagerSettings
    return $ Endpoint manager host

localEndpoint :: IO Endpoint
localEndpoint = initEndpoint "http://localhost:5001"


data Content = Empty
             | File FilePath
             | Dir  [FilePath]
             | Raw  ByteString

-- | The 'call' function makes an API call to the IPFS node given by the 'Endpoint'
call :: Endpoint            -- ^ IPFS node
     -> [String]            -- ^ command
     -> [(String, String)]  -- ^ options [(key, value)]
     -> [String]            -- ^ arguments (an IPFS path for example)
     -> Content             -- ^ content to send
     -> IO ByteString       -- ^ Return of a successful API request
call (Endpoint manager host) cmd opts args content = do
    req <- MFD.formDataBody (parts content) simpleReq
    HTTP.responseBody <$> HTTP.httpLbs req manager
    where
        path :: Builder
        path = URI.encodePathSegments . map T.pack $ ["api", "v0"] ++ cmd

        query ::URI.Query
        query = [(U.fromString k, Just $ U.fromString v) | (k,v) <- opts] ++
                [(U.fromString "arg", Just $ U.fromString arg) | arg <- args]

        simpleReq :: HTTP.Request
        simpleReq = (fromJust $ HTTP.parseUrl host) {
            HTTP.method      = "POST",
            HTTP.path        = toByteString path,
            HTTP.queryString = URI.renderQuery True query
        }

        parts :: Content -> [MFD.Part]
        parts  Empty            = []
        parts (File filePath)   = [MFD.partFileSourceChunked (T.pack filePath) filePath]
        parts (Dir  filePaths)  = map (\f -> MFD.partFileSourceChunked (T.pack f) f) filePaths
        parts (Raw  byteString) = [MFD.partLBS "raw" byteString]

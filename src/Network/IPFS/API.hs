{-# LANGUAGE OverloadedStrings #-}

module Network.IPFS.API (
    Content (..),
    Endpoint (..),
    initEndpoint,
    localEndpoint,
    call,
    callWithContent
) where

import           Blaze.ByteString.Builder              (Builder, toByteString)
import           Control.Monad.Trans.Reader            (ReaderT (..))
import           Data.ByteString.Lazy                  (ByteString)
import qualified Data.ByteString.UTF8                  as U
import           Data.Maybe                            (fromJust)
import qualified Data.Text                             as T
import qualified Network.HTTP.Client.MultipartFormData as MFD
import qualified Network.HTTP.Conduit                  as HTTP
import qualified Network.HTTP.Types.URI                as URI
import           Network.IPFS.Types                    (Endpoint (..),
                                                        IPFS (..))

withEndpoint :: (Endpoint -> IO a) -> IPFS a
withEndpoint io = IPFS (ReaderT (\endpoint -> io endpoint))

-- | To initialize the API at an IPFS node, the Endpoint
-- For example (in a do block):
--
-- > endpoint <- initEndpoint "http://localhost:5001"
initEndpoint :: String -> IO Endpoint
initEndpoint host = do
    manager <- HTTP.newManager HTTP.tlsManagerSettings
    return $ Endpoint manager host

localEndpoint :: IO Endpoint
localEndpoint = initEndpoint "http://localhost:5001"

data Content = Empty
             | File  FilePath
             | Files [FilePath]
             | Raw   ByteString

-- | The 'call' function makes an API call to the IPFS node given by the 'Endpoint'
call :: [String] -> [(String, String)] -> [String] -> IPFS ByteString
call cmd opts args = callWithContent cmd opts args Empty

-- | The 'callWithContent' function makes an API call to the IPFS node given by the 'Endpoint'
callWithContent :: [String]            -- ^ command
                -> [(String, String)]  -- ^ options [(key, value)]
                -> [String]            -- ^ arguments (an IPFS path for example)
                -> Content             -- ^ content to send
                -> IPFS ByteString       -- ^ Return of a successful API request
callWithContent cmd opts args content =
    withEndpoint $ \endpoint -> callWithContentIO endpoint cmd opts args content

callWithContentIO :: Endpoint -> [String] -> [(String, String)] -> [String] -> Content -> IO ByteString       -- ^ Return of a successful API request
callWithContentIO (Endpoint manager host) cmd opts args content = do
    req <- MFD.formDataBody partsWithHeaders simpleReq
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

        partsWithHeaders :: [MFD.Part]
        partsWithHeaders = map (`MFD.addPartHeaders` [("Content-Type", "application/octet-stream")]) $ parts content

        parts :: Content -> [MFD.Part]
        parts  Empty             = []
        parts (File  filePath)   = [MFD.partFileSourceChunked (T.pack filePath) filePath]
        parts (Files filePaths)  = map (\f -> MFD.partFileSourceChunked (T.pack f) f) filePaths
        parts (Raw   byteString) = [MFD.partLBS "raw" byteString]

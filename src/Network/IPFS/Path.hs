module Network.IPFS.Path (
    Path (..),
    fromString,
    toString
) where

import           Data.ByteString.Lazy      (toStrict)
import qualified Data.ByteString.Lazy.UTF8 as UTF8
import           Data.List                 (intercalate)
import           Data.List.Split           (splitOn)
import qualified Data.Multihash.Base       as MB
import qualified Data.Multihash.Digest     as MD

data PathType = IPFS
              | IPNS
              deriving (Eq, Show)

data Path     = Path {
        pathType :: PathType,
        hash     :: MD.MultihashDigest,
        filePath :: FilePath
    } deriving (Eq, Show)

fromString :: String -> Maybe Path
fromString string = Path <$> p <*> h <*> (Just f)
    where
        strings :: [String]
        strings = splitOn "/" string

        p          :: Maybe PathType
        hashString :: String
        f          :: FilePath
        (p, hashString, f) = case strings !! 0 of
            "" -> if length strings < 2
                then (Nothing, "", "")
                else case strings !! 1 of
                    "ipfs" -> (Just IPFS, strings !! 2, intercalate "/" $ drop 3 $ strings)
                    "ipns" -> (Just IPNS, strings !! 2, intercalate "/" $ drop 3 $ strings)
                    _      -> (Nothing, "", "")
            _  -> (Just IPFS, strings !! 0, intercalate "/" $ drop 1 $ strings)

        h :: Maybe MD.MultihashDigest
        h = either (\_ -> Nothing) Just
                $ MD.decode . toStrict =<< (MB.decode MB.Base58 $ UTF8.fromString hashString)


toString :: Path -> String
toString (Path IPFS h f) = "/ipfs/" ++ toString' h ++ "/" ++ f
toString (Path IPNS h f) = "/ipns/" ++ toString' h ++ "/" ++ f

toString' :: MD.MultihashDigest -> String
toString' h = UTF8.toString . MB.encode MB.Base58 $ MD.encode (MD.algorithm h) (MD.digest h)

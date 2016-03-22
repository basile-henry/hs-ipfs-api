module Network.IPFS.Types(Path(..), Hash, fromString) where

import           Control.Monad   (liftM2)
import           Data.ByteString (ByteString)
import           Data.List       (intercalate)
import           Data.List.Split (splitOn)

type Hash = ByteString -- TODO use multihash library

data Path = IPFS Hash FilePath
          | IPNS Hash FilePath

instance Show Path where
    show (IPFS hash filePath) = "/ipfs/" ++ show hash ++ "/" ++ show filePath
    show (IPNS hash filePath) = "/ipns/" ++ show hash ++ "/" ++ show filePath

fromString :: String -> Maybe Path
fromString str = case (splitOn "/" str) of
    ("":"ipfs":x:xs) -> liftM2 IPFS (readMaybe x) (Just $ intercalate "/" xs)
    ("":"ipns":x:xs) -> liftM2 IPNS (readMaybe x) (Just $ intercalate "/" xs)
    (x:xs)           -> liftM2 IPFS (readMaybe x) (Just $ intercalate "/" xs) -- defaulting to IPFS seems wrong
    [x]              -> liftM2 IPFS (readMaybe x) (Just "")
    _                -> Nothing

readMaybe :: Read a => String -> Maybe a
readMaybe s = case reads s of
    [(x, "")] -> Just x
    _         -> Nothing


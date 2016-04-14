{-# LANGUAGE DeriveGeneric #-}

module Network.IPFS.Path (
    Path (..)
) where

import           Control.Applicative          (many, some, (<|>))
import           Data.ByteString.Lazy         (toStrict)
import qualified Data.ByteString.Lazy.UTF8    as UTF8
import           Data.List                    (intercalate)
import           Data.List.Split              (splitOn)
import           GHC.Generics                 (Generic)
import           Network.IPFS.Types           (Multihash)
import           Text.ParserCombinators.ReadP (ReadP, char, readP_to_S,
                                               readS_to_P, satisfy, string)

data Path = PathIPFS Multihash FilePath
          | PathIPNS Multihash FilePath
          deriving (Eq, Generic)

instance Show Path where
    show (PathIPFS hash path) = "/ipfs/" ++ show hash ++ slash path
    show (PathIPNS hash path) = "/ipns/" ++ show hash ++ slash path

slash :: String -> String
slash s
    | length s == 0 = ""
    | head s == '/' = s
    | otherwise     = '/' : s

instance Read Path where
    readsPrec _ = readP_to_S $ parseIPFS <|> parseIPNS

parseIPFS :: ReadP Path
parseIPFS = do
    string "/ipfs/"
    hash <- readS_to_P read
    path <- parsePath
    return $ PathIPFS hash path

parseIPNS :: ReadP Path
parseIPNS = do
    string "/ipns/"
    hash <- readS_to_P read
    path <- parsePath
    return $ PathIPNS hash path

parsePath :: ReadP FilePath
parsePath = many $ satisfy (\_ -> True)

{-# LANGUAGE DeriveGeneric #-}

module Network.IPFS.Path (
    Path (..)
) where

import           Control.Applicative          (many, (<|>))
import           Data.String                  (IsString (..), fromString)
import           GHC.Generics                 (Generic)
import           Network.IPFS.Types           (Multihash, parseMultihash)
import           Text.ParserCombinators.ReadP (ReadP, readP_to_S, satisfy,
                                               string)

data Path = PathIPFS Multihash FilePath
          | PathIPNS Multihash FilePath
          deriving (Eq, Generic)

instance Show Path where
    show (PathIPFS hash path) = "/ipfs/" ++ show hash ++ slash path
    show (PathIPNS hash path) = "/ipns/" ++ show hash ++ slash path

instance IsString Path where
    fromString = read

slash :: String -> String
slash s
    | length s == 0 = ""
    | head s == '/' = s
    | otherwise     = '/' : s

instance Read Path where
    readsPrec _ = readP_to_S $ parseIPFS <|> parseIPNS <|> parseDefault

parseIPFS :: ReadP Path
parseIPFS = do
    string "/ipfs/"
    hash <- parseMultihash
    path <- parsePath
    return $ PathIPFS hash path

parseIPNS :: ReadP Path
parseIPNS = do
    string "/ipns/"
    hash <- parseMultihash
    path <- parsePath
    return $ PathIPNS hash path

-- When just a hash, the default is IPFS
parseDefault :: ReadP Path
parseDefault = do
    hash <- parseMultihash
    return $ PathIPFS hash ""

parsePath :: ReadP FilePath
parsePath = many $ satisfy (\_ -> True)

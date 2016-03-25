{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.ByteString.Base58    as B58
import qualified Data.ByteString.Char8     as C
import qualified Data.ByteString.Lazy.UTF8 as LU
import qualified Data.ByteString.UTF8      as U
import           Data.Maybe                (fromJust)
import           Network.IPFS
import           Network.IPNS
import           Network.IPFS.API

main :: IO ()
main = do
    ipfs <- localEndpoint
    -- print =<< ipfs `publish` "QmPio4aCNzuWvpp2P34ohhyvnWhZ3hsy16raeYTW3Cc2Ra"
    -- print =<< resolve ipfs
    -- let mhash = "QmPXME1oRtoT627YKaDPDQ3PwA8tdP9rWuAAweLzqSwAWT"
    -- let digest = fromJust . B58.decodeBase58 B58.bitcoinAlphabet $ C.pack mhash
    -- obj <- ipfs `getObject` digest
    -- print obj
    -- txt <- ipfs `cat` (mhash ++ "/readme")
    -- putStrLn $ LU.toString txt
    h <- fromJust <$> ipfs `addFile` "testsend.txt"
    print h
    base <- fromJust <$> newObject ipfs Unixfs
    print base
    print =<< (addLink ipfs h base)
    -- c <- ipfs `cat` (U.toString h)
    -- print c


-- {\"Name\":\"testsend.txt\",\"Hash\":\"QmPHYPjERVgjAaB9Bc1XgcE4SbksdE91ZsfcqHixdcKxve\"}\n
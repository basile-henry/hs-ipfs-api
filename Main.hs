{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.ByteString.Base58    as B58
import qualified Data.ByteString.Char8     as C
import qualified Data.ByteString.Lazy.UTF8 as U
import           Data.Maybe                (fromJust)
import           Network.IPFS
import qualified Network.IPFS.API          as API

main :: IO ()
main = do
    ipfs <- API.init "http://localhost:5001/"
    let mhash = "QmPXME1oRtoT627YKaDPDQ3PwA8tdP9rWuAAweLzqSwAWT"
    let digest = fromJust . B58.decodeBase58 B58.bitcoinAlphabet $ C.pack mhash
    obj <- ipfs `getObject` digest
    print obj
    txt <- ipfs `cat` (mhash ++ "/readme")
    putStrLn $ U.toString txt
    h <- ipfs `addDir` "testdir"
    print h
    -- c <- ipfs `cat` (take 46 $ drop 31 $ U.toString h)
    -- print c


-- {\"Name\":\"testsend.txt\",\"Hash\":\"QmPHYPjERVgjAaB9Bc1XgcE4SbksdE91ZsfcqHixdcKxve\"}\n
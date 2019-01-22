{-# LANGUAGE BangPatterns #-}
module Main where

import Data.Digest.Pure.MD5
import qualified Data.ByteString.Lazy as L
import System.Environment
import Control.Concurrent
import Control.Concurrent.Chan
import Control.Monad( forever , forM_ , replicateM_ )

nrWorker = 2 
main = do
    files <- getArgs
    str         <- newChan
    fileChan    <- newChan
    forM_ [1 .. nrWorker] (\_ -> forkIO $ worker str fileChan)
    forM_ files (writeChan fileChan)
    printNrResults ( length files) str
    putStrLn "START"

printNrResults i var = replicateM_ i ( readChan var >>= putStrLn)

worker :: Chan String -> Chan String -> IO()
worker str fileChan = forever ( readChan fileChan >>= hashAndPrint str)

hashAndPrint str f = do
    bs <- L.readFile f 
    let !h = show $ md5 bs
    writeChan str ( f ++ ":" ++ h)



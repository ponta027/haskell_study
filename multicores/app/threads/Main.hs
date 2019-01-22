module Main where


import Control.Concurrent(forkIO)
import Data.Digest.Pure.MD5 (md5)
import qualified Data.ByteString.Lazy as L
import System.Environment (getArgs)
 
main = do
    [fileA, fileB] <- getArgs
    forkIO $ hashAndPrint fileA
    hashAndPrint fileB
 
hashAndPrint f = L.readFile f >>= return . md5 >>= \h -> putStrLn (f ++ ": " ++ show h)


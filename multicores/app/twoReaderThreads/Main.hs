module Main where

import System.IO
import Control.Concurrent
import Control.Concurrent.Chan
import Text.Printf

type Pipe = Chan(Either String String)

main :: IO()
main = do
    chan    <- newChan ::IO Pipe
    s       <- getChanContents chan
    c1Thread    <- forkIO $ reader "c1" (catLeft s)
    c2Thread    <- forkIO $ reader "c2" (catRight s)
    hSetBuffering stdout NoBuffering
    writer chan  
    where
        catLeft ls = [x | Left x <- ls]
        catRight ls = [x | Right x <- ls]


writer :: Pipe -> IO()
writer chan = loop
        where 
            loop = getChar >>= command
            command '0' = print "done"
            command '1' = writeChan chan (Left "main:1" ) >> loop
            command '2' = writeChan chan (Right "main:2" ) >> loop
            command '\n' = loop
            command c = printf "illegal: %c\n" c >> loop


reader::String->[String] -> IO()
reader name xs = mapM_ ( printf "%s %s\n" name ) xs


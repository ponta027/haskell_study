module Main where

import Lib
import Network.Socket
import System.IO

main :: IO ()
main = do
    sock <- socket AF_INET Stream 0 
    setSocketOption sock ReuseAddr 1 
    bind sock (SockAddrInet 4242 iNADDR_ANY)
    listen sock 2
    mainLoop sock

mainLoop :: Socket -> IO ()
mainLoop sock = do
    conn <- accept sock     -- accept a connection and handle it
    runConn conn            -- run our server's logic
    mainLoop sock           -- repeat

{--
runConn :: (Socket, SockAddr) -> IO ()
runConn (sock, _) = do
    send sock "Hello!\n"
    close sock
--}

runConn::(Socket,SockAddr) -> IO()
runConn (sock,_) = do
    hdl <- socketToHandle sock ReadWriteMode
    hSetBuffering hdl NoBuffering
    hPutStrLn hdl "Hello"
    hClose hdl

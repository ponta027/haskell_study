module Main where

import Lib
import Network.Socket
import System.IO
import Control.Concurrent
import Control.Monad.Fix( fix )


type Msg = String


main :: IO ()
main = do
    sock <- socket AF_INET Stream 0 
    setSocketOption sock ReuseAddr 1 
    bind sock (SockAddrInet 4242 iNADDR_ANY)
    listen sock 2
    chan <- newChan
    mainLoop sock   chan

{--
mainLoop :: Socket -> IO ()
mainLoop sock = do
    conn <- accept sock     -- accept a connection and handle it
    runConn conn            -- run our server's logic
    mainLoop sock           -- repeat
--}
{--
runConn :: (Socket, SockAddr) -> IO ()
runConn (sock, _) = do
    send sock "Hello!\n"
    close sock
--}

runConn::(Socket,SockAddr) -> Chan Msg -> IO()
runConn (sock,_) chan = do
    let broadcast msg = writeChan chan msg  -- broadcast msg 
    hdl <- socketToHandle sock ReadWriteMode
    hSetBuffering hdl NoBuffering
    commLine <- dupChan chan                -- commLine Channel 
    --https://qiita.com/eielh/items/3b793dbedcc6c9dc125c
    forkIO $ fix $ \loop -> do              -- forkIO ::IO() -> IO ThreadId
        line <- readChan commLine           -- readChan commLine::Chan
        putStrLn $ "Msg:" ++ show line
        hPutStrLn hdl line                  -- response 
        loop                                -- call loop
    fix $ \loop -> do
        line <- fmap init (hGetLine hdl)    -- hGetLine::Handle->IO String
        broadcast line                      -- call broadcast msg
        loop                                    


mainLoop :: Socket -> Chan Msg -> IO ()
mainLoop sock chan = do
    conn <- accept sock
    forkIO (runConn conn chan)
    mainLoop sock chan


module Main where

import Lib
import Network.Socket
import System.IO
import Control.Concurrent
import Control.Monad( when )
import Control.Monad.Fix( fix )
import Control.Exception


type Msg = (Int,String)


main :: IO ()
main = do
    sock <- socket AF_INET Stream 0 
    setSocketOption sock ReuseAddr 1 
    bind sock (SockAddrInet 4242 iNADDR_ANY)
    listen sock 2
    chan <- newChan
    _ <- forkIO $ fix $ \loop -> do
        (_,_) <- readChan chan
        loop
    mainLoop sock   chan 0

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

runConn::(Socket,SockAddr) -> Chan Msg -> Int -> IO()
runConn (sock,_) chan msgNum = do
    let broadcast msg = writeChan chan (msgNum,msg)  -- broadcast msg 
    hdl <- socketToHandle sock ReadWriteMode
    hSetBuffering hdl NoBuffering
    hPutStrLn hdl "Hi what's your name?"
    name <- fmap init (hGetLine hdl)
    broadcast ("-->" ++ name ++ " entered chat.")
    hPutStrLn hdl ("Welcome," ++ name ++ "!")
    commLine <- dupChan chan                -- commLine Channel 
    --https://qiita.com/eielh/items/3b793dbedcc6c9dc125c
    reader <- forkIO $ fix $ \loop -> do              -- forkIO ::IO() -> IO ThreadId
            (nextNum,line) <- readChan commLine           -- readChan commLine::Chan            when (msgNum /=nextNum) $ hPutStrLn hdl line
            loop
    handle (\(SomeException _) -> return()) $ fix $ \loop -> do
        line <- fmap init (hGetLine hdl)    -- hGetLine::Handle->IO String
        case line of 
            "quit"  -> hPutStrLn hdl "Byte!"
            _       -> broadcast line  >>loop                    -- call broadcast msg
    killThread reader
    broadcast ("<--" ++ name ++ "left.")
    hClose hdl


-- | Socket: socket 
-- | Chan Msg: message
-- | Int:Msg Num
mainLoop :: Socket -> Chan Msg -> Int -> IO ()
mainLoop sock chan msgNum = do
    conn <- accept sock
    forkIO (runConn conn chan msgNum)
    mainLoop sock chan $! msgNum + 1


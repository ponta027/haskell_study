{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
module Lib
    ( someFunc
    , someFuncServer
    ) where

import Data.List(find)
import qualified Network.Socket  as S
import qualified Network.Socket.ByteString as SB
import qualified Data.ByteString.Char8 as C
import Data.Maybe
import Control.Monad (unless)
import GHC.Generics
import System.Daemon



host="localhost"
port="10000"
hint = S.defaultHints { S.addrSocketType = S.Datagram, S.addrProtocol = 17}

createSocket = do
    info <- S.getAddrInfo (Just hint) (Just host) (Just port)    
    let saddr =  find (\ i -> S.AF_INET == S.addrFamily i ) info
    sock <- S.socket ( S.addrFamily $ fromJust saddr ) S.Datagram S.defaultProtocol
    return ( sock, saddr )

someFunc :: String -> IO ()
someFunc msg = do
    (sock,saddr) <- createSocket 
    SB.sendTo sock
        (C.pack msg )
        (S.addrAddress  $ fromJust saddr)
    msg <- SB.recv sock 1024
    S.close sock
    putStrLn "someFunc"
    C.putStrLn msg

data Command = Run | Stop  deriving (Generic , Show)


instance Serialize Command

data Response = Failed String
              | Value ByteString
                deriving ( Generic, Show )
instance Serialize Response

handleCommand ::Command -> IO Response
handleCommand comm = case comm of 
                    Run     -> startSocket
                    Stop    ->

startSocket :: IO()
startSocket = do
    sock <- S.socket S.AF_INET S.Datagram 0 
    S.bind sock (S.SockAddrInet 10000 S.iNADDR_ANY)
    echo sock

       
-- server
someFuncServer :: String -> IO()
someFuncServer msg= do
    let options = def { daemonPort = 7856 }
    ensureDaemonRunning "addOne" options handleCommand 

-- echo 
echo ::S.Socket -> IO()
echo sock = do
    (msg , client) <- SB.recvFrom sock 1024
    case msg of 
        "quit"  -> S.close sock
        _       -> do 
                SB.sendTo sock ( "resp:" `C.append` msg ) client
                echo sock


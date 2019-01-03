{-# LANGUAGE CPP, OverloadedStrings, RecordWildCards, LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Lib
    ( someFunc
    , subscriber
    , publisher

    ) where

import Database.Redis
--import qualified Control.Concurrent.Async as Async
import Control.Exception
--import Control.Exception (try,SomeException)
import Control.Concurrent
import Control.Monad
import Control.Monad.Trans

import Data.Monoid ((<>))
import Data.Text
import Data.ByteString (ByteString)
import Data.Text.Encoding

import System.IO

someFunc :: IO ()
someFunc = do
    conn <- checkedConnect defaultConnectInfo
    tmp <- sample conn
    disconnect conn
    putStrLn "someFunc"

sample conn = 
    runRedis conn $ do
    set "hello" "hello"
    set "world" "world"
    hello <- get "hello"
    world <- get "world"
    liftIO $ print (hello,world)

msgHandler :: ByteString -> IO ()
msgHandler msg = hPutStrLn stderr $ "Saw msg: " ++ unpack (decodeUtf8 msg)


myhandler::ByteString -> IO()
-- unpack ::ByteString -> [Word8]
myhandler msg = hPutStrLn stderr $ "Saw msg: " ++ unpack (decodeUtf8 msg)
--myhandler msg = putStrLn $ unpack $ decodeUtf8 msg

onInitialComplete::IO()
onInitialComplete = putStrLn "Redis acknowledged that mychannel is now subscribed"

subscriber :: IO ()
subscriber = do
    conn <- checkedConnect defaultConnectInfo
    putStrLn "[START]"
    pubSubCtrl <- newPubSubController [("mychannel", myhandler)] []
    forkIO $ forever $
        pubSubForever conn pubSubCtrl onInitialComplete
            `catch` (\(e :: SomeException) -> do
                putStrLn $ "Got error: " ++ show e
                threadDelay $ 50*1000) -- TODO: use exponential backoff

    {- elsewhere in your program, use pubSubCtrl to change subscriptions -}    
    putStrLn "[END]subscribe"


publisher ::IO ()
publisher = do
    ctrl <- newPubSubController [("foo", msgHandler)] []
    putStrLn "publisher"

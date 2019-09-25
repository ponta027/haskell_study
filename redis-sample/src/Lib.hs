{-# LANGUAGE CPP, OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Lib
    ( someFunc
    , subscriber
    , publisher

    ) where

import Database.Redis
-- import qualified Control.Concurrent.Async as Async
import Control.Exception
-- import Control.Exception (try,SomeException)
import Control.Concurrent
import qualified Control.Concurrent.Async as A
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
    tmp <- kVSSample conn
    disconnect conn
    putStrLn "someFunc"

kVSSample conn = 
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

-- | publish messages every 2 seconds to several channels
publishThread :: Connection -> IO ()
publishThread c = runRedis c $ loop (0 :: Int)
  where
    loop i = do
      let msg = encodeUtf8 $ pack $ "Publish iteration " ++ show i
      void $ publish "foo" ("foo" <> msg)
      void $ publish "bar" ("bar" <> msg)
      void $ publish "baz:1" ("baz1" <> msg)
      void $ publish "baz:2" ("baz2" <> msg)
      liftIO $ threadDelay $ 2*1000*1000 -- 2 seconds
      loop (i+1)

handlerThread :: Connection -> PubSubController -> IO ()
handlerThread conn ctrl = forever $
       pubSubForever conn ctrl onInitialComplete
         `catch` (\(e :: SomeException) -> do
           hPutStrLn stderr $ "Got error: " ++ show e
           threadDelay $ 50*1000)



subscriber ::IO ()
subscriber =do
    ctrl <- newPubSubController [("foo", msgHandler)] []
    conn <- connect defaultConnectInfo
    tmp <- createSubThread conn ctrl
    putStrLn "subscriber"


createSubThread conn ctrl =
--    A.withAsync (subscribeThread conn) $ \_pubT -> 
    A.withAsync (handlerThread conn ctrl) $ \_handlerT -> do
    usub1 <- addChannelsAndWait ctrl [("foo2",msgHandler)] [("baz",pmsgHandler)]
    showChannels conn
    page2 <- A.wait _handlerT
--    page1 <- A.wait _pubT
    putStrLn "end"        
 




publisher ::IO ()
publisher = do
    ctrl <- newPubSubController [("foo", msgHandler)] []
    conn <- connect defaultConnectInfo
    tmp <- createThread conn ctrl
    putStrLn "publisher"

createThread conn ctrl =
    A.withAsync (publishThread conn) $ \_pubT -> 
    A.withAsync (handlerThread conn ctrl) $ \_handlerT -> do
--    usub1 <- addChannelsAndWait ctrl [("foo2",msgHandler)] [("baz",pmsgHandler)]
    showChannels conn
    page2 <- A.wait _handlerT
    page1 <- A.wait _pubT
    putStrLn "end"        
    


pmsgHandler :: RedisChannel -> ByteString -> IO ()
pmsgHandler channel msg = hPutStrLn stderr $ "Saw pmsg: " ++ unpack (decodeUtf8 channel) ++ unpack (decodeUtf8 msg)


showChannels :: Connection -> IO ()
showChannels c = do
  resp :: Either Reply [ByteString] <- runRedis c $ sendRequest ["PUBSUB", "CHANNELS"]
  liftIO $ hPutStrLn stderr $ "Current redis channels: " ++ show resp



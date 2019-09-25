{-# Language DataKinds, OverloadedStrings #-}
module Lib(someFunc
    ,mqttSample
) where
import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad (unless, forever)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)

import qualified Network.MQTT as MQTT

t1, t2 :: MQTT.Topic
t1 = "topic1"
t2 = "topic2/#"


handleMsg :: MQTT.Message MQTT.PUBLISH -> IO ()
handleMsg msg =
    -- sometimes it's useful to ignore retained messages
    unless (MQTT.retain $ MQTT.header msg) $ do
      let t = MQTT.topic $ MQTT.body msg
          p = MQTT.payload $ MQTT.body msg
      case MQTT.getLevels t of
        ["topic1"] -> putStr "topic1: " >> print p
        ["topic2", "foo"] -> putStr "foo: " >> print p
        "topic2" : bar   -> print bar
        _unexpected -> putStrLn $ "unexpected message on '" ++ show t ++ "': " ++ show p

-- | sample
someFunc :: IO ()
someFunc = do 
    mqttSample

-- | mqttSample sequence
--
--  1. create Command Channel ( Control.Currents.STM.TChan)
--
--  2. newTChan ( TChan ) : FIFO Channel
--
--  3. createConfig defaultConfg:: Comands -> TChan (Message PUBLISH) -> Config
--
--  4. forkIO . MQTT.subscribe:: Config -> [(Topic,QoS)] -> IO [Qos]
--
--  5. forever and atomically readTChan pubChan and handleMsg (readTChan pubChan)
--
--  forever :: Applicative f = f a -> f b

mqttSample :: IO ()
mqttSample = do
  print "START"
  cmds <- MQTT.mkCommands
  pubChan <- newTChanIO
  let conf = (MQTT.defaultConfig cmds pubChan)
              { 
                MQTT.cHost = "192.168.0.21"
              }

  _ <- forkIO $ do
    qosGranted <- MQTT.subscribe conf [(t1, MQTT.Handshake), (t2, MQTT.Handshake)]
    case qosGranted of
      [MQTT.Handshake, MQTT.Handshake] -> forever $ atomically (readTChan pubChan) >>= handleMsg
      _ -> do
        hPutStrLn stderr $ "Wanted QoS Handshake, got " ++ show qosGranted
        exitFailure

  terminated <- MQTT.run conf
  print terminated

{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Web.Spock
import Web.Spock.Config
import Web.Spock.Core                       (SpockCtxT)

import           Data.Aeson       hiding (json)
-- import           Data.Monoid      ((<>))
import           Data.Text        (Text, pack)

import Control.Monad.Trans
import Data.IORef
import Data.Monoid
--import qualified Data.Text as T
import           GHC.Generics

import           Network.Wai            (Middleware)
import           Network.Wai.Middleware.RequestLogger   (logStdoutDev)
import           Network.Wai.Middleware.Static
import           Network.Wai.Handler.WebSockets
import qualified Data.Text.Lazy as TL
import           Text.Mustache

import           Network.WebSockets
import           Control.Concurrent     (threadDelay)

data MySession = EmptySession
data MyAppState = DummyAppState (IORef Int)

data Person = Person
  { name :: Text
  , age  :: Int
  } deriving (Generic, Show)
instance ToJSON Person
instance FromJSON Person




type Api = SpockM () MySession MyAppState ()
-- type Api = SpockM () () () ()

type ApiAction a = SpockAction () MySession MyAppState a
-- type ApiAction a = SpockAction () () () a


appMiddlewares :: Web.Spock.Core.SpockCtxT () (WebStateM () MySession MyAppState) ()
appMiddlewares = do
    middleware logStdoutDev                                     -- add logging
    middleware (staticPolicy $ noDots >-> addBase "static")     -- add static files
    middleware wsMiddleware                                     -- add websocket

wsMiddleware :: Middleware
wsMiddleware =  websocketsOr defaultConnectionOptions wsApp
    where
        wsApp :: ServerApp
        wsApp pendingConn = do
            conn <- acceptRequest pendingConn
            forkPingThread conn 30  -- not needed here, but good to have in most other situations
            sendTextData conn ("..." :: Text)
            counter conn 1
        counter :: Connection -> Int -> IO ()
        counter conn i = do
            threadDelay 50000   -- 50ms
            sendTextData conn (pack $ show i)
            counter conn (i + 1)

main :: IO ()
main = do 
        ref <- newIORef 0
        template <- compileMustacheDir "index" "views/"
        let render pname = TL.toStrict . renderMustache (template {templateActual = pname})
 
        --spockCfg <- defaultSpockCfg () PCNoDatabase ()
        spockCfg <- defaultSpockCfg EmptySession PCNoDatabase (DummyAppState ref)
        runSpock 8080 ( spock spockCfg (appMiddlewares>> app render))
--        runSpock 8080 $fmap (logStdout.) $ spock spockCfg (app render)

app :: (PName->Value->Text) -> Api
app render= do
  get root $ html ( render "index" (object ["text" .= ("Spock"::Text)]))
  get "people" $ do
    json [Person { name = "Fry", age = 25 }, Person { name = "Bender", age = 4 }]
  post "people" $ do
    thePerson <- jsonBody' :: ApiAction Person
    text $ "Parsed: " <> pack (show thePerson)
  get ("hello" <//> var) $ \pname ->
           do (DummyAppState ref) <- getState
              visitorNumber <- liftIO $ atomicModifyIORef' ref $ \i -> (i+1, i+1)
              text ("Hello " <> pname <> ", you are visitor number " <> pack (show visitorNumber))


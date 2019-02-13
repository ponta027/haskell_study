{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Web.Spock
import Web.Spock.Config

import           Data.Aeson       hiding (json)
-- import           Data.Monoid      ((<>))
-- import           Data.Text        (Text, pack)

import Control.Monad.Trans
import Data.IORef
import Data.Monoid
import qualified Data.Text as T
import           GHC.Generics

import           Network.Wai.Middleware.RequestLogger   (logStdout)
import qualified Data.Text.Lazy as TL
import           Text.Mustache


data MySession = EmptySession
data MyAppState = DummyAppState (IORef Int)

data Person = Person
  { name :: T.Text
  , age  :: Int
  } deriving (Generic, Show)
instance ToJSON Person
instance FromJSON Person




type Api = SpockM () () () ()

type ApiAction a = SpockAction () () () a



main :: IO ()
main = do 
        ref <- newIORef 0
        template <- compileMustacheDir "index" "views/"
        let render pname = TL.toStrict . renderMustache (template {templateActual = pname})
 
        spockCfg <- defaultSpockCfg EmptySession PCNoDatabase (DummyAppState ref)
        runSpock 8080 $fmap (logStdout.) $ spock spockCfg (app render)


app ::(PName->Value->T.Text) -> SpockM () MySession MyAppState ()
app render =
    do 
       get root $ html ( render "index" (object ["text" .= ("Spock"::T.Text)]))
       get ("hello" <//> var) $ \aname ->
           do (DummyAppState ref) <- getState
              visitorNumber <- liftIO $ atomicModifyIORef' ref $ \i -> (i+1, i+1)
              text ("Hello " <> aname <> ", you are visitor number " <> T.pack (show visitorNumber))


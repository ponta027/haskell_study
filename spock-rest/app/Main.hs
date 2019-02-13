{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}


module Main where

import           Web.Spock
import           Web.Spock.Config

import           Data.Aeson       hiding (json)
import           Data.Monoid      ((<>))
import           Data.Text        (Text, pack)
import           GHC.Generics

import           Network.Wai.Middleware.RequestLogger   (logStdout)
import qualified Data.Text.Lazy as TL
import           Text.Mustache




data Person = Person
  { name :: Text
  , age  :: Int
  } deriving (Generic, Show)
instance ToJSON Person
instance FromJSON Person




type Api = SpockM () () () ()

type ApiAction a = SpockAction () () () a

main :: IO ()
main = do
  template <- compileMustacheDir "index" "views/"
  let render pname = TL.toStrict . renderMustache (template {templateActual = pname})
  spockCfg <- defaultSpockCfg () PCNoDatabase ()
  runSpock 8080 $ fmap (logStdout.) $ spock spockCfg (app render)

app :: (PName->Value->Text) -> Api
app render= do
  get root $ html ( render "index" (object ["text" .= ("Spock"::Text)]))
  get "people" $ do
    json [Person { name = "Fry", age = 25 }, Person { name = "Bender", age = 4 }]
  post "people" $ do
    thePerson <- jsonBody' :: ApiAction Person
    text $ "Parsed: " <> pack (show thePerson)


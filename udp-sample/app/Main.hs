{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}

module Main where

import Lib
import           System.Environment (getArgs)

import GHC.Generics
-- import Data.Default(def)
-- import Data.Serialize ( Serialize )
-- import Data.ByteString.Char8 ( ByteString )
-- import System.Daemon

{--
data Command = Run | Stop deriving (Generic ,Show)
instance Serialize Command
data Response = Failed String | Value ByteString deriving(Generic,Show)
instance Serialize Response

handleCommand :: Command->IO Response
handleCommand command = 
    case comm of 
        Run -> 
        Stop ->
        --}

main :: IO ()
main = do
--    let options = def {datamonPort=7856}
--    ensureDaemonRunning "addOne" options (handleCommand)
    args <- getArgs
    case head args of
        "client" -> someFunc (args !! 1)
        "server" -> someFuncServer (args !! 1)
        _        -> print "None"

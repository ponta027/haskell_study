module Main where

import Lib
import System.Environment(getArgs)

main :: IO ()
main = do
    args <- getArgs
    case args !! 0 of 
        "client" -> someFunc
        "subscribe" -> subscriber
        "publish"   -> publisher 
        _       -> undefined

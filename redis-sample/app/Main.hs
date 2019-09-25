module Main where

import Lib
import System.Environment(getArgs)

main :: IO ()
main = do
    args <- getArgs
    case head args  of 
        "client" -> someFunc
        "subscribe" -> subscriber
        "publish"   -> publisher 
        _       -> undefined

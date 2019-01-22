module Main where 
-- Example using STM and orElse to compose a solution
--import Control.Monad
import Control.Monad( forever , forM_ , replicateM_ , when )
--import Control.Monad.CatchIO
import Control.Concurrent
import Control.Exception
import Control.Concurrent.STM
import Data.IORef
import System.IO
import Network.Socket
--import Data.IORef

main :: IO()
main = do
    sock <- socket AF_INET Stream 0
    setSocketOption sock ReuseAddr 1
    bind sock (SockAddrInet 4242 iNADDR_ANY)
    listen sock 2
    runExampleFor sock 10
    putStrLn "END"

foreverX x = x >> forever x
 
runExampleFor :: Socket -> Int -> IO b
runExampleFor socket seconds = do
  tv <- newTVarIO False           -- Set to True to indicate graceful exit requested
  sInfo <- startServer socket tv
  threadDelay (1000*1000*seconds)
  shutdownServer tv sInfo
 
startServer :: Socket -> TVar Bool -> IO b
{--
startServer socket tv = do
        childrenList <- newMVar []
        tInfo <- fork (acceptUntil socket exampleReceiver childrenList ( retry'until'true tv))
        return(tInfo,childrenList)
--}
startServer socket tv = undefined 
--
fork todo = undefined
{--
fork todo = block $ do
    doneVar <- atomically (newEmptyTMVar)
    let putStarted = atomically (putTMVar doneVar False)
        putStopped = atomically (tryTakeTMVar doneVar >> putTMVar doneVar False)
    tid <- forkIO $ block $ (finally (putStarted >> unblock todo ) putStopped)
    yield 
    atomically $ do
        value <- takeTMVar doneVar 
        when value (putTMVar doneVar True) 
    return (doneVar,tid)
--}

acceptUntil socket reciever childrenList checker = undefined


cond true false test = if test then true else false 
retry'until'true tv = (readTVar tv >>= cond (return ()) retry)


exampleReceiver (handle,_,_) = do
    hPutStrLn handle "Hello."
    hPutStrLn handle "Goodby."

shutdownServer::  TVar Bool -> IO b -> IO c
shutdownServer tv sInfo = undefined


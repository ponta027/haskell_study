module Main where

import Control.Monad
import Control.Concurrent
import Control.Concurrent.STM

-- | newTVar:
main = do 
        shared <- atomically $ newTVar 0
        before <- atomRead shared 
        putStrLn $ "Before" ++ show before
        forkIO $ 25 `timesDo` (dispVar shared >> milliSleep 20) -- 20*25 = 500
        forkIO $ 10 `timesDo` (appV ((+) 2) shared >> milliSleep 50)    -- 500
        forkIO $ 20 `timesDo` (appV pred shared >> milliSleep 25)   -- pred:: a-> a :: x--
            -- 20*25 = 500 
        milliSleep 800
        after <- atomRead shared 
        putStrLn $ "After " ++ show after
    where 
        timesDo = replicateM_
        milliSleep = threadDelay . (*) 1000

atomRead = atomically . readTVar

dispVar x = (++) <$> (return "shared = ") <*> (show <$> atomRead x)  >>= print
{--
dispVar x = do 
            v <- atomRead x
            putStrLn $ "share=" ++ show v
--}

-- dispVar x = atomRead x)  >>= print   -- original code 

appV fn x= atomically $ readTVar x >>= writeTVar x . fn 



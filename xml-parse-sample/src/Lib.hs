module Lib
    ( someFunc
      ,parseXml
      ,getFood
      ,getPrice

    ) where
import           System.Environment (getArgs)
import Text.XML.HaXml
import Text.XML.HaXml.Posn
import Text.XML.HaXml.Util
import Text.XML.HaXml.Xtract.Parse


getPrice :: Content Posn -> [Content Posn]
getPrice = xtract id "//price" 

getFood:: Content Posn -> [Content Posn]
getFood =xtract id "//food"

parseXml :: String -> Content Posn
parseXml contents = 
        -- Text.XML.HaXml
            let Document _ _ root _ = xmlParse "" contents
            in
        --Text.XML.HaXml.Posn
            CElem root noPos

someFunc :: IO ()
someFunc = do
        args <- getArgs
        contents <- readFile $ head args
        let cont = parseXml contents
        let baseTypes = getFood cont
        let y = map getPrice baseTypes
        mapM_ (print . tagTextContent ) $ concat y
        --mapM_ (\x -> print $ tagTextContent x ) $ concat y
        print "END"

module Main where

import Data.List ( group )


rle :: String -> String
rle "" = ""
rle (h:t) = aux 1 h t where
    aux :: Int -> Char -> String -> String
    aux runLength prevChar "" = prevChar : show runLength
    aux runLength prevChar ( c: s ) 
        | c == prevChar = aux ( runLength + 1 ) prevChar s
        | otherwise = prevChar : shows runLength ( aux 1 c s )
rle' :: String -> String
rle' = fromCharAndRunLength . toCharAndRunLength

fromCharAndRunLength :: [(Char,Int)]->String
fromCharAndRunLength = concatMap rl2str

rl2str :: (Char,Int) -> String
rl2str (c,n)= c : show n




toCharAndRunLength :: String -> [(Char,Int)]
toCharAndRunLength = map toPairs . group


toPairs :: String ->  (Char,Int)
toPairs str = (head str, length str)


main = do
    print "BEGIN"
    print ( rle "AA")
    print ( rle' "AA")

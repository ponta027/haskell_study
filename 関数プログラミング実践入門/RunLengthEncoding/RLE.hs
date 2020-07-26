module RLE where


import Data.List (group)

-- | ランレングス圧縮
-- >>> rle ""
-- >>> rle "A"
-- "A1"
-- >>> rle "AAABBCCCCCAAA"
-- A3B2C4A3
rle::String -> String
rle "" = ""
rle (h:t) = aux 1 h t where
  aux :: Int -> Char -> String -> String
  aux runLength prevChar "" = prevChar : show runLength
  aux runLength prevChar (c:s) 
      | c == prevChar = aux ( runLength + 1 ) prevChar s
      | otherwise = prevChar : shows runLength ( aux 1 c s)

rle2 :: String -> String
rle2 =concatMap (\s -> head s :show (length s)) . group







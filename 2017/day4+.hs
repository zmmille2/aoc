import Data.Char
import Data.List
import Data.List.Split

--validPhrase :: IO [[Int]]
validPhrase = do
    all <- readFile "inputs/day4.txt"
    let rows = splitOn "\n" all
    let info = map (splitOn " ") rows
    return (validPhraseHelper info 0)
    --return validPhraseHelper nums

-- validPhraseHelper :: IO Int
validPhraseHelper [] n = n
validPhraseHelper (x:xs) n
    | isValid y [] = validPhraseHelper xs n+1
    | otherwise    = validPhraseHelper xs n
    where y = map sort x

isValid [] _  = True
isValid (x:xs) seen
    | elem x seen = False
    | otherwise   = isValid xs (x:seen)
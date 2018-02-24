import Data.Char
import Data.List.Split
import Data.Bool

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
    | isValid x [] = validPhraseHelper xs n+1
    | otherwise    = validPhraseHelper xs n

isValid [] _  = True
isValid (x:xs) seen
    | elem x seen = False
    | otherwise   = isValid xs (x:seen)

--456 too high, new line threw things off lmao
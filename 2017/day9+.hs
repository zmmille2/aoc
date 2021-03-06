import Data.Char
import Data.List
import Data.List.Split
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Debug.Trace

main = do
    all <- readFile "inputs/day9.txt"
    return $ processStream all 0 False 0

processStream :: String -> Int -> Bool -> Int -> Int
processStream [] _ _ score = score
processStream ('!':_:rest) level isGarbage score = processStream rest level isGarbage score
processStream ('<':rest) level isGarbage score
    | isGarbage = processStream rest level isGarbage (score + 1)
    | otherwise = processStream rest level True score
processStream ('>':rest) level isGarbage score = processStream rest level False score
processStream ('{':rest) level isGarbage score
    | isGarbage = processStream rest level isGarbage (score + 1)
    | otherwise = processStream rest (level + 1) isGarbage score
processStream ('}':rest) level isGarbage score
    | isGarbage = processStream rest level isGarbage (score + 1)
    | otherwise = processStream rest (level - 1) isGarbage score
processStream (_:rest) level isGarbage score
    | isGarbage = processStream rest level isGarbage (score + 1)
    | otherwise = processStream rest level isGarbage (score)

    -- 3910 too low, forgot to consider when special characters are garbage
    -- 5136 also too low, forgot about <
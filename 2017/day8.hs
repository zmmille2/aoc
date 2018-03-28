import Data.Char
import Data.List
import Data.List.Split
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Debug.Trace

main = do
    all <- readFile "inputs/day8.txt"
    let commands   = splitOn "\n" all
    let registers  = map words items
    return $ runCommand commands registers

runCommand :: [String] -> Map String Int -> Map String Int
runCommand [] world                      = world
runCommand command world
    | conRegister conOperator conOperand = world'
    | otherwise                          = world
    where
        world'                                                              = Map.insertWith operator register operand world 
        (register, operator, operand, conRegister, conOperator, conOperand) = parseCommand command

parseCommand :: String -> (String, Function, Int, String, Function, Int)
parseCommand command = (register, operator, operand, conRegister, conOperator, conOperand)
    where
        c0 = words command
        (register, c1)    = (head c0, tail c0)
        (operator, c2)    = (head c1, tail c1)
        (operand, c3)     = (head c2, tail c2)
        (_, c4)           = (head c3, tail c3)
        (conRegister, c5) = (head c4, tail c4)
        (conOperator, c6) = (head c5, tail c5)
        conOperand        = head c6
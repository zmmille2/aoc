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
    let registers  = Map.empty
    return $ runCommands commands registers 0

runCommands :: [String] -> Map String Int -> Int -> Map String Int
runCommands [] world biggest = traceShow biggest world
runCommands (command:xs) world biggest
    | condition = runCommands xs world' (max biggest value)
    | otherwise = runCommands xs world biggest
    where
        value                                                               = Map.findWithDefault 0 register world
        world'                                                              = Map.insert register (operator value operand) world
        condition                                                           = (conOperator (Map.findWithDefault 0 conRegister world) conOperand)
        (register, operator, operand, conRegister, conOperator, conOperand) = parseCommand command

parseCommand :: String -> (String, (Int -> Int -> Int), Int, String, (Int -> Int -> Bool), Int)
parseCommand command = (register, operator, operand, conRegister, conOperator, conOperand)
    where
        c0 = words command
        (register, c1)    = (head c0, tail c0)
        (operator, c2)    = (parseIncDec (head c1), tail c1)
        (operand, c3)     = (read (head c2) :: Int, tail c2)
        (_, c4)           = (head c3, tail c3)
        (conRegister, c5) = (head c4, tail c4)
        (conOperator, c6) = (parseEq (head c5), tail c5)
        conOperand        = read (head c6) :: Int

parseIncDec :: String -> (Int -> Int -> Int)
parseIncDec str
    | str == "inc" = (+)
    | str == "dec" = (-)
    | otherwise    = traceShow str (\x y->0)

parseEq :: String -> (Int -> Int -> Bool)
parseEq str
    | str == "<"  = (<)
    | str == ">"  = (>)
    | str == "<=" = (<=)
    | str == ">=" = (>=)
    | str == "==" = (==)
    | str == "!=" = (/=)
    | otherwise   = traceShow str (\x y->False)


    -- 5502 too high, 3135 too low, 4448 was right! InsertWith didn't do what I thought it did.
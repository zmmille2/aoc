import Data.Char

foo :: Int -> Int -> Int
foo x y
    | x == y    = x
    | otherwise = 0

repeatSum :: String -> Int
repeatSum [] = 0
repeatSum xs = sum (zipWith foo (map digitToInt xs) (map digitToInt across))
    where split = splitAt (quot (length xs) 2) xs
          across = (snd split) ++ (fst split)

input = 361527

findRadius :: Int -> Int
findRadius n = quot (findRadiusHelper n 1) 2

findRadiusHelper :: Int -> Int -> Int
findRadiusHelper n s
    | n <= (s * s) = s
    | otherwise    = findRadiusHelper n (s + 2)

findPosition :: Int -> Int
findPosition 1 = 0
findPosition n = n - (((2 * radius) + 1) ^ 2) + ((radius * 8) - 1)
    where
        radius = findRadius n
        nextRadius = radius + 1
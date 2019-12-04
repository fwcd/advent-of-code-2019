-- Part 1.

isPossiblyPassword :: Int -> Bool
isPossiblyPassword i = adj i 10 False
    where adj :: Int -> Int -> Bool -> Bool
          adj i d fnd | i == 0 = fnd || d == d'
                      | otherwise = d >= d' && adj i' d' (fnd || d == d')
            where i' = i `div` 10
                  d' = i `mod` 10

solution :: [Int] -> Int
solution = length . filter isPossiblyPassword

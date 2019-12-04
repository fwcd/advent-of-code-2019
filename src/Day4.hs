-- Part 1.

isPossiblyPassword :: Int -> Bool
isPossiblyPassword i = adj i 10 False
    where adj :: Int -> Int -> Bool -> Bool
          adj i d fnd | i == 0 = fnd'
                      | otherwise = d >= d' && adj i' d' fnd'
            where i' = i `div` 10
                  d' = i `mod` 10
                  fnd' = fnd || d == d'

solution :: [Int] -> Int
solution = length . filter isPossiblyPassword

-- Part 2.

isPossiblyPassword2 :: Int -> Bool
isPossiblyPassword2 i = adj i 10 11 12 False
    where adj :: Int -> Int -> Int -> Int -> Bool -> Bool
          adj i d dd ddd fnd | i == 0 = fnd'
                             | otherwise = d >= d' && adj i' d' d dd fnd'
            where i' = i `div` 10
                  d' = i `mod` 10
                  fnd' = fnd || ((d' /= d) && (d == dd) && (dd /= ddd))

solution2 :: [Int] -> Int
solution2 = length . filter isPossiblyPassword2


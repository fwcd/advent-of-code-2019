fuel :: Int -> Int
fuel m = (m `div` 3) - 2

totalFuel :: [Int] -> Int
totalFuel = foldr (+) 0 . map fuel

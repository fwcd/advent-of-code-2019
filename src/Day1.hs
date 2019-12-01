-- Part 1

fuel :: Int -> Int
fuel m = (m `div` 3) - 2

totalFuel :: [Int] -> Int
totalFuel = foldr (+) 0 . map fuel

-- Part 2
fuelCumulative :: Int -> Int
fuelCumulative m | m > 0 = max 0 $ f + fuelCumulative f
                 | otherwise = 0
    where f = fuel m

totalFuelCumulative :: [Int] -> Int
totalFuelCumulative = foldr (+) 0 . map fuelCumulative

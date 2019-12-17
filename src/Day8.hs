import Data.List

-- Day 8: Space Image Format

-- Part 1.

chunks :: Int -> [a] -> [[a]]
chunks n xs | null zs = [ys]
            | otherwise = ys : chunks n zs
    where ys = take n xs
          zs = drop n xs

count :: Eq a => a -> [a] -> Int
count x = length . filter (== x)

layers :: Int -> Int -> [a] -> [[a]]
layers w h = chunks $ w * h

minimumComparing :: (a -> Int) -> [a] -> a
minimumComparing f = minimumBy (\x y -> compare (f x) (f y))

solution1 :: Int -> Int -> [Int] -> Int
solution1 w h = (\l -> count 1 l * count 2 l) . minimumComparing (count 0) . layers w h

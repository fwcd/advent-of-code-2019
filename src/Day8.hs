import Control.Monad
import Data.List

-- Day 8: Space Image Format

data Image = Image Int Int [Int]

-- Part 1.

chunks :: Int -> [a] -> [[a]]
chunks n xs | null zs = [ys]
            | otherwise = ys : chunks n zs
    where ys = take n xs
          zs = drop n xs

count :: Eq a => a -> [a] -> Int
count x = length . filter (== x)

layers :: Image -> [[Int]]
layers (Image w h px) = chunks (w * h) px

minimumComparing :: (a -> Int) -> [a] -> a
minimumComparing f = minimumBy $ \x y -> compare (f x) (f y)

solution1 :: Image -> Int
solution1 = (\l -> count 1 l * count 2 l) . minimumComparing (count 0) . layers

-- Part 2.

composeLayers :: [Int] -> [Int] -> [Int]
composeLayers xs [] = xs
composeLayers [] ys = ys
composeLayers xs ys = zipWith composer xs ys
    where composer x y = if x == 2 then y
                                   else x

composeAllLayers :: Image -> [Int]
composeAllLayers = foldl composeLayers [] . layers

replace :: Eq a => a -> a -> [a] -> [a]
replace o r = map $ \x -> if x == o then r else x

printImage :: Image -> IO ()
printImage img@(Image w _ _) = putStrLn $ unlines $ join <$> (chunks w $ replace "0" " " $ show <$> composeAllLayers img)

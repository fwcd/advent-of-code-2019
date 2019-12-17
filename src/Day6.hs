import Data.Functor
import Data.Maybe
import qualified Data.Map as M
import qualified Data.List as L

-- Day 6: Universal Orbit Map

-- Part 1.

data Orbit = Orbit String String deriving Show
type OrbitMap = M.Map String [String]
type OrbitCountMap = M.Map String Int

-- Creates an in-memory graph structure from a list of orbits.
buildOrbitMap :: [Orbit] -> OrbitMap
buildOrbitMap = foldr insertOrbit M.empty
    where insertOrbit :: Orbit -> OrbitMap -> OrbitMap
          insertOrbit (Orbit x y) = M.insertWith (++) x [y]

-- The number of transitive orbits.
indirectOrbitCount :: OrbitMap -> Int
indirectOrbitCount = indirectOrbitCount' (-1) "COM"
    where indirectOrbitCount' :: Int -> String -> OrbitMap -> Int
          indirectOrbitCount' c x os = foldr (+) cc $ (flip (indirectOrbitCount' $ 1 + c) $ os) <$> ys
            where cc = max c 0
                  ys = M.findWithDefault [] x os

-- The number of total (direct and indirect) orbits.
orbitCount :: [Orbit] -> Int
orbitCount os = (indirectOrbitCount . buildOrbitMap) os + length os

-- Splits a string around a character.
splitOn :: Char -> String -> [String]
splitOn _ "" = [""]
splitOn sep (c:cs) = if c == sep then ("":(r:rs))
                                 else ((c:r):rs)
    where (r:rs) = splitOn sep cs

-- Parses an orbit.
parseOrbit :: String -> Orbit
parseOrbit s = Orbit sx sy
    where (sx:sy:_) = splitOn ')' s

parseOrbits :: [String] -> [Orbit]
parseOrbits = map parseOrbit

-- Part 2.

-- partitionUnique :: Eq a => [a] -> ([a], [a])
-- partitionUnique = partitionUnique' [] []
--     where partitionUnique' :: Eq a => [a] -> [a] -> [a] -> ([a], [a])
--           partitionUnique' us ds [] = (us, ds)
--           partitionUnique' us ds (x:xs) = if elem x us then partitionUnique' (L.delete x us) (x:ds) xs
--                                                        else partitionUnique' (x:us) ds xs
-- 
-- safeMinimum :: Ord a => [a] -> Maybe a
-- safeMinimum [] = Nothing
-- safeMinimum xs = Just $ minimum xs

expectJust :: String -> Maybe a -> a
expectJust _ (Just x) = x
expectJust msg Nothing = error msg

stepMaybe :: String -> String -> OrbitMap -> Maybe (String, Int)
stepMaybe x y om | x == y = Just ("", -1)
                 | otherwise = listToMaybe $ (>>= \x' -> maybeToList $ (\(_, d) -> (x', d + 1)) <$> stepMaybe x' y om) $ M.findWithDefault [] x om

step :: String -> String -> OrbitMap -> (String, Int)
step x y = expectJust ("No path from " <> x <> " to " <> y) . stepMaybe x y

orbitalTransfers :: String -> String -> OrbitMap -> Int
orbitalTransfers = orbitalTransfers' "COM"
    where orbitalTransfers' :: String -> String -> String -> OrbitMap -> Int
          orbitalTransfers' z x y om | z /= x && z /= y && xp == yp = orbitalTransfers' xp x y om
                                     | otherwise = xd + yd
            where (xp, xd) = step z x om
                  (yp, yd) = step z y om

import qualified Data.Map as M

-- Day 6: Universal Orbit Map

-- Part 1.

data Orbit = Orbit String String deriving Show
type OrbitCountMap = M.Map String Int

-- Finds the number of direct and indirect orbits for each planet.
orbitCounts :: [Orbit] -> (OrbitCountMap, OrbitCountMap)
orbitCounts os = let (ds, as) = orbitCounts' M.empty M.empty os
                  in (ds, M.mapWithKey (\x y -> y - M.findWithDefault 0 x ds) as)
    where orbitCounts' :: OrbitCountMap -> OrbitCountMap -> [Orbit] -> (OrbitCountMap, OrbitCountMap)
          orbitCounts' ds as [] = (ds, as)
          orbitCounts' ds as (Orbit x y:os) = orbitCounts' ds' as' os
            where ds' = M.insertWith (+) y 1 ds
                  as' = M.insertWith (+) y (1 + M.findWithDefault 0 x as) as

-- Computes the sum of direct and indirect orbit counts.
orbitCountSum :: [Orbit] -> Int
orbitCountSum os = mapSum ds + mapSum is
    where mapSum = M.foldr (+) 0
          (ds, is) = orbitCounts os

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

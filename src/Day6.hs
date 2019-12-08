import qualified Data.Map as M

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

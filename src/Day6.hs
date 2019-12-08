import qualified Data.Map as M

-- Day 6: Universal Orbit Map

-- Part 1.

data Orbit = Orbit String String
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

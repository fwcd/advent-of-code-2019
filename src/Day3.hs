-- Part 1.

data Vec2 = Vec2 Int Int deriving (Show, Eq)

instance Num Vec2 where
    (Vec2 x y) + (Vec2 x' y') = Vec2 (x + x') (y + y')
    (Vec2 x y) - (Vec2 x' y') = Vec2 (x - x') (y - y')
    (Vec2 x y) * (Vec2 x' y') = Vec2 (x * x') (y * y')
    abs (Vec2 x y) = Vec2 (abs x) (abs y)
    signum (Vec2 x y) = Vec2 (signum x) (signum y)
    fromInteger i = Vec2 n n
        where n = (fromInteger i) :: Int

instance Ord Vec2 where
    compare a b = compare (manhattanDistance a) (manhattanDistance b)

-- Measures the manhattan distance to the origin of a given point.
manhattanDistance :: Vec2 -> Int
manhattanDistance (Vec2 x y) = (abs x) + (abs y)

-- Finds all points on the given direction vector.
pointsOn :: Vec2 -> [Vec2]
pointsOn (Vec2 dx dy) = [Vec2 x y |
    x <- if dx == 0 then [0] else [dx,(dx - signum dx)..(signum dx)],
    y <- if dy == 0 then [0] else [dy,(dy - signum dy)..(signum dy)]]

-- Turns a list of direction vectors into a path.
pathPoints :: [Vec2] -> [Vec2]
pathPoints = foldl (\l v -> (map ((head l) +) $ pointsOn $ v) ++ l) [Vec2 0 0]

-- Computes the intersection of two lists.
intersect :: Eq a => [a] -> [a] -> [a]
intersect xs [] = []
intersect xs (y:ys) = if elem y xs then y : intersect xs ys
                                   else intersect xs ys

-- Splits a string.
split :: Char -> String -> [String]
split _ "" = [""]
split d (c:s) = if c == d then "" : rs : rss
                          else (c : rs) : rss
    where (rs:rss) = split d s

parseSegment :: String -> Vec2
parseSegment (dd:nn) = (Vec2 n n) * d
    where n = (read nn) :: Int
          d = case dd of
                   'L' -> Vec2 (-1) 0
                   'U' -> Vec2 0 (-1)
                   'R' -> Vec2 1 0
                   'D' -> Vec2 0 1

parsePath :: String -> [Vec2]
parsePath = map parseSegment . split ','

solution :: String -> String -> Int
solution p1 p2 = manhattanDistance $ minimum $ filter ((> 0) . manhattanDistance) $ intersect (pf p1) (pf p2)
    where pf = pathPoints . parsePath

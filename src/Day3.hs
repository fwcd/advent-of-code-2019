-- Part 1.

data Vec = Vec Int Int deriving (Show, Eq)
data LineSeg = LineSeg Vec Vec deriving (Show, Eq)

instance Num Vec where
    (Vec x y) + (Vec x' y') = Vec (x + x') (y + y')
    (Vec x y) - (Vec x' y') = Vec (x - x') (y - y')
    (Vec x y) * (Vec x' y') = Vec (x * x') (y * y')
    abs (Vec x y) = Vec (abs x) (abs y)
    signum (Vec x y) = Vec (signum x) (signum y)
    fromInteger i = Vec n n
        where n = (fromInteger i) :: Int

instance Ord Vec where
    compare a b = compare (manhattanDistance a) (manhattanDistance b)

-- Measures the manhattan distance to the origin of a given point.
manhattanDistance :: Vec -> Int
manhattanDistance (Vec x y) = (abs x) + (abs y)

-- Computes the manhattan distance length of a line.
manhattanLength :: LineSeg -> Int
manhattanLength (LineSeg s e) = manhattanDistance (e - s)

-- Turns a list of direction vectors into a path.
lineSegsOf :: [Vec] -> [LineSeg]
lineSegsOf = tail . reverse . foldl (\l d -> let (LineSeg _ v) = head l in (LineSeg v $ v + d) : l) [LineSeg (Vec 0 0) (Vec 0 0)]

-- Tests whether a value is between the given bounds.
isBetween :: Ord a => a -> a -> a -> Bool
isBetween s l x = (x >= s && x <= l) || (x >= l && x <= s)

orthoIntersection1 :: LineSeg -> LineSeg -> Maybe Vec
orthoIntersection1 l1 l2 = if (isBetween x1 x1' x2) && (isBetween y2 y2' y1) then Just $ Vec x2 y1
                                                                             else Nothing
    where (LineSeg (Vec x1 y1) (Vec x1' y1')) = l1
          (LineSeg (Vec x2 y2) (Vec x2' y2')) = l2

orthoIntersection2 :: LineSeg -> LineSeg -> Maybe Vec
orthoIntersection2 l1 l2 = if (isBetween y1 y1' y2) && (isBetween x2 x2' x1) then Just $ Vec x1 y2
                                                                             else Nothing
    where (LineSeg (Vec x1 y1) (Vec x1' y1')) = l1
          (LineSeg (Vec x2 y2) (Vec x2' y2')) = l2

-- Tests whether two axis-aligned and orthogonal line segments intersect.
intersection :: LineSeg -> LineSeg -> Maybe Vec
intersection l1 l2 | x1 == x1' && x2 /= x2' = orthoIntersection2 l1 l2
                   | x2 == x2' && x1 /= x1' = orthoIntersection2 l2 l1
                   | y1 == y1' && y2 /= y2' = orthoIntersection1 l1 l2
                   | y2 == y2' && y1 /= y1' = orthoIntersection1 l2 l1
                   | otherwise = error $ "Non-orthogonal lines: " <> show l1 <> " and " <> show l2
    where (LineSeg (Vec x1 y1) (Vec x1' y1')) = l1
          (LineSeg (Vec x2 y2) (Vec x2' y2')) = l2

-- Tests whether two axis-aligned line segments are orthogonal to each other.
areOrthogonal :: LineSeg -> LineSeg -> Bool
areOrthogonal l1 l2 = (x1 == x1' && x2 /= x2')
                   || (x2 == x2' && x1 /= x1')
                   || (y1 == y1' && y2 /= y2')
                   || (y2 == y2' && y1 /= y1')
    where (LineSeg (Vec x1 y1) (Vec x1' y1')) = l1
          (LineSeg (Vec x2 y2) (Vec x2' y2')) = l2


-- Converts an optional to a list.
maybeToList :: Maybe a -> [a]
maybeToList Nothing = []
maybeToList (Just x) = [x]

-- Finds all pairs where the components are not equal.
nonEqualPairs :: Eq a => [a] -> [a] -> [(a, a)]
nonEqualPairs xs ys = filter (uncurry (/=)) [(x, y) | x <- xs, y <- ys]

-- Finds all intersections of two lines.
intersections :: [LineSeg] -> [LineSeg] -> [Vec]
intersections p1 p2 = do
    (l1, l2) <- filterOrtho $ nonEqualPairs p1 p2
    maybeToList $ intersection l1 l2
    where filterOrtho = filter (uncurry areOrthogonal)

-- Splits a string.
split :: Char -> String -> [String]
split _ "" = [""]
split d (c:s) = if c == d then "" : rs : rss
                          else (c : rs) : rss
    where (rs:rss) = split d s

-- Parses a formatted direction.
parseSegment :: String -> Vec
parseSegment (dd:nn) = (Vec n n) * d
    where n = (read nn) :: Int
          d = case dd of
                   'L' -> Vec (-1) 0
                   'U' -> Vec 0 (-1)
                   'R' -> Vec 1 0
                   'D' -> Vec 0 1

-- Parses a comma-separated list of formatted directions
parsePath :: String -> [Vec]
parsePath = map parseSegment . split ','

-- Computes the solution to the problem (part 1).
solution1 :: String -> String -> Int
solution1 rp1 rp2 = minimum $ filter (> 0) $ manhattanDistance <$> intersections (pf rp1) (pf rp2)
    where pf = lineSegsOf . parsePath

-- Part 2.

-- Computes the number of steps needed to reach the given position.
wireStepsTo :: Vec -> [LineSeg] -> Maybe Int
wireStepsTo _ [] = Nothing
wireStepsTo (Vec x y) p | sx == ex && ex == x && isBetween sy ey y = Just $ abs $ y - sy
                        | sy == ey && ey == y && isBetween sx ex x = Just $ abs $ x - sx
                        | otherwise = (manhattanLength l +) <$> wireStepsTo (Vec x y) ls
    where (l@(LineSeg (Vec sx sy) (Vec ex ey)):ls) = p

-- Computes the solution to the problem (part 2).
solution2 :: String -> String -> Int
solution2 rp1 rp2 = minimum $ filter (> 0) $ do
    i <- intersections p1 p2
    w1 <- maybeToList $ wireStepsTo i p1
    w2 <- maybeToList $ wireStepsTo i p2
    [w1 + w2]
    where pf = lineSegsOf . parsePath
          p1 = pf rp1
          p2 = pf rp2

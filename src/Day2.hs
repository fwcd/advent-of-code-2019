-- Part 1

replaceNth :: Int -> a -> [a] -> [a]
replaceNth n z (x:xs) | n == 0 = z : xs
                      | otherwise = x : replaceNth (n - 1) z xs

-- Performs a single operation in Intcode.
performOp :: Int -> Int -> Int -> Int -> [Int] -> [Int]
performOp op x y z m = case op of
                          1 -> replaceNth z ((m !! x) + (m !! y)) m
                          2 -> replaceNth z ((m !! x) * (m !! y)) m
                          n -> error $ "Invalid op " <> show n

-- Interprets a program written in Intcode.
interpret :: [Int] -> [Int]
interpret m = interpret' m m
    where interpret' :: [Int] -> [Int] -> [Int]
          interpret' [] = id
          interpret' (99:_) = id
          interpret' (op:x:y:z:p) = interpret' p . performOp op x y z
          interpret' p = const $ error $ "Parse error on remaining program: " <> show p

-- Interprets a program written in Intcode with inputs and returns the value in the first cell.
interpretWithInputs :: Int -> Int -> [Int] -> Int
interpretWithInputs x y (op:_:_:p) = head $ interpret (op:x:y:p)

-- Part 2.

interpretingYields :: Int -> [Int] -> Int -> Int -> Bool
interpretingYields o p x y = o == interpretWithInputs x y p

findInputs :: Int -> [Int] -> (Int, Int)
findInputs o p = head $ filter (uncurry (interpretingYields o p)) [(x, y) | x <- [0..99], y <- [0..99]]

solution :: Int -> [Int] -> Int
solution o p = 100 * n + v
    where (n, v) = findInputs o p

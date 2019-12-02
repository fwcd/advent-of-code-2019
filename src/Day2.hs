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

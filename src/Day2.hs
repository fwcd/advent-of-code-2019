-- Part 1

replaceNth :: Int -> a -> [a] -> [a]
replaceNth n z (x:xs) | n == 0 = z : xs
                      | otherwise = replaceNth (n - 1) z xs

-- Performs a single operation in Intcode.
performOp :: [Int] -> [Int] -> ([Int], [Int])
performOp (op:x:y:z:p) mem = (p, f mem)
    where f = case op of
                   0 -> replaceNth z (x + y)
                   1 -> replaceNth z (x * y)

-- Interprets a program written in Intcode.
interpret :: [Int] -> [Int] -> [Int]
interpret [] mem = mem
interpret p mem = interpret remP newMem
    where (remP, newMem) = performOp p mem

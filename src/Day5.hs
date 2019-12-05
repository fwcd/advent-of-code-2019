-- Part 1

replaceNth :: Int -> a -> [a] -> [a]
replaceNth n z (x:xs) | n == 0 = z : xs
                      | otherwise = x : replaceNth (n - 1) z xs

-- Performs a single operation in Intcode.
-- The first argument is the opcode.
-- The second argument is the rest of the program AFTER the opcode.
-- The third argument is the memory before running the operation.
-- Returns a pair of the remaining program AFTER consuming the operation's parameters and the new memory.
performOp :: Int -> [Int] -> [Int] -> IO ([Int], [Int])
performOp op p m = case op of
                          1 -> let (x:y:z:p') = p in pure $ (p', replaceNth z ((m !! x) + (m !! y)) m)
                          2 -> let (x:y:z:p') = p in pure $ (p', replaceNth z ((m !! x) * (m !! y)) m)
                          3 -> let (z:p') = p in (\x -> (p', replaceNth z x m)) <$> read <$> getLine
                          4 -> let (x:p') = p in (putStrLn $ show $ m !! x) >> pure (p', m)
                          n -> error $ "Invalid op " <> show n

-- Interprets a program written in Intcode.
interpret :: [Int] -> IO [Int]
interpret m = interpret' m m
    where interpret' :: [Int] -> [Int] -> IO [Int]
          interpret' [] = pure
          interpret' (99:_) = pure
          interpret' (op:p) = \m -> do
              (p', m') <- performOp op p m
              interpret' p' m'


import Control.Monad

-- Day 5: Sunny with a Chance of Asteroids
-- aka. Intcode interpreter v2

-- Part 1

debugEnabled :: Bool
debugEnabled = True

-- A parameter mode determines how an operation's parameter should be turned into a value.
-- This represented using a function that takes the parameter and the memory and evaluates to the value.
type ParameterMode = Int -> [Int] -> Int

immediateMode :: ParameterMode
immediateMode = const

positionMode :: ParameterMode
positionMode = flip (!!)

modeOf :: Int -> ParameterMode
modeOf 0 = positionMode
modeOf 1 = immediateMode

replaceNth :: Int -> a -> [a] -> [a]
replaceNth n z (x:xs) | n == 0 = z : xs
                      | otherwise = x : replaceNth (n - 1) z xs

parseOp :: Int -> (ParameterMode, ParameterMode, ParameterMode, Int)
parseOp o = (modeOf $ (o `div` 10000) `mod` 10,
             modeOf $ (o `div` 1000)  `mod` 10,
             modeOf $ (o `div` 100)   `mod` 10,
                       o              `mod` 100)

-- Performs a single operation in Intcode.
-- The first argument is the opcode.
-- The second argument is the rest of the program AFTER the opcode.
-- The third argument is the memory before running the operation.
-- Returns a pair of the consumed parameter count and the new memory.
performOp :: Int -> [Int] -> [Int] -> IO (Int, [Int])
performOp o p m = do
    when debugEnabled $ putStrLn $ "Operation: " <> show op <> " " <> show p <> show m
    case op of
         1 -> let (x:y:z:_) = p in pure $ (3, replaceNth z ((mode1 x m) + (mode2 y m)) m)
         2 -> let (x:y:z:_) = p in pure $ (3, replaceNth z ((mode1 x m) * (mode2 y m)) m)
         3 -> let (z:_) = p in (\x -> (1, replaceNth z x m)) <$> read <$> getLine
         4 -> let (x:_) = p in (putStrLn $ show $ mode1 x m) >> pure (1, m)
         n -> error $ "Invalid op " <> show n
    where (mode3, mode2, mode1, op) = parseOp o

-- Interprets a program written in Intcode.
interpret :: [Int] -> IO [Int]
interpret = interpret' 0
    where interpret' :: Int -> [Int] -> IO [Int]
          interpret' ip m = case drop ip m of
                                 [] -> pure m
                                 (99:_) -> pure m
                                 (o:p) -> do
                                     (dip, m') <- performOp o p m
                                     interpret' (ip + 1 + dip) m'


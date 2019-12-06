import Control.Monad
import Data.Vector.Unboxed as V
import Data.Vector.Unboxed.Mutable as VM

-- Day 5: Sunny with a Chance of Asteroids
-- aka. Intcode interpreter v2

-- Part 1

debugEnabled :: Bool
debugEnabled = False

-- A parameter mode determines how an operation's parameter should be turned into a value.
-- This represented using a function that takes the parameter and the memory and evaluates to the value.
type ParameterMode = Int -> V.Vector Int -> Int

immediateMode :: ParameterMode
immediateMode = const

positionMode :: ParameterMode
positionMode = flip (V.!)

modeOf :: Int -> ParameterMode
modeOf 0 = positionMode
modeOf 1 = immediateMode

replaceNth :: V.Unbox a => Int -> a -> V.Vector a -> V.Vector a
replaceNth n z = V.modify (\v -> VM.write v n z)

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
performOp :: Int -> V.Vector Int -> V.Vector Int -> IO (Int, V.Vector Int)
performOp o p m = do
    when debugEnabled $ putStrLn $ "Operation: " <> show op <> " " <> show p <> show m
    case op of
         1 -> pure $ (3, replaceNth z ((mode1 x m) + (mode2 y m)) m)
            where x = p V.! 0
                  y = p V.! 1
                  z = p V.! 2
         2 -> pure $ (3, replaceNth z ((mode1 x m) * (mode2 y m)) m)
            where x = p V.! 0
                  y = p V.! 1
                  z = p V.! 2
         3 -> (\x -> (1, replaceNth z x m)) <$> Prelude.read <$> getLine
            where z = V.head p
         4 -> (putStrLn $ show $ mode1 x m) >> pure (1, m)
            where x = V.head p
         n -> error $ "Invalid op " <> show n
    where (mode3, mode2, mode1, op) = parseOp o

-- Interprets a program written in Intcode.
interpret :: [Int] -> IO [Int]
interpret = (toList <$>) . (interpret' 0) . fromList
    where interpret' :: Int -> Vector Int -> IO (Vector Int)
          interpret' ip m = if V.null pp || V.head pp == 99 then pure m
                                                            else do
                                                                (dip, m') <- performOp (V.head pp) (V.tail pp) m
                                                                interpret' (ip + 1 + dip) m'
              where pp = V.drop ip m


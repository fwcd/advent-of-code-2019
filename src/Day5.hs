import Control.Monad
import Data.Vector.Unboxed as V
import Data.Vector.Unboxed.Mutable as VM

-- Day 5: Sunny with a Chance of Asteroids
-- aka. Intcode interpreter v2

debugEnabled :: Bool
debugEnabled = True

type Memory = V.Vector Int
type InstructionPointerUpdate = Int -> Int

-- A parameter mode determines how an operation's parameter should be turned into a value.
type ParameterMode = Int -> Memory -> Int

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

intOfBool :: Bool -> Int
intOfBool True = 1
intOfBool False = 0

-- Performs a single operation in Intcode.
-- The first argument is the opcode.
performOp :: Int -> Memory -> Memory -> IO (InstructionPointerUpdate, Memory)
performOp o p m = case op of
                       -- Add
                       1 -> pure $ ((+3), replaceNth n ((pval 0) + (pval 1)) m)
                          where n = param 2

                       -- Multiply
                       2 -> pure $ ((+3), replaceNth n ((pval 0) * (pval 1)) m)
                          where n = param 2

                       -- Input
                       3 -> (\v -> ((+1), replaceNth n v m)) <$> Prelude.read <$> getLine
                          where n = param 0

                       -- Print
                       4 -> (putStrLn $ show $ pval 0) >> pure ((+1), m)
                       
                       -- Jump-if-true
                       5 -> pure (if (pval 0) /= 0 then const $ pval 0 else (+1), m)
                       
                       -- Jump-if-false
                       6 -> pure (if (pval 0) == 0 then const $ pval 0 else (+1), m)

                       -- Less-than
                       7 -> pure ((+3), replaceNth n (intOfBool $ (pval 0) < (pval 1)) m)
                          where n = pval 2
                       
                       -- Equals
                       8 -> pure ((+3), replaceNth n (intOfBool $ (pval 0) == (pval 1)) m)
                          where n = pval 2

                       -- Unknown
                       n -> error $ "Invalid op " <> show n
    where (mode2, mode1, mode0, op) = parseOp o

          param :: Int -> Int
          param = (p V.!)
          pval :: Int -> Int
          pval 0 = mode0 (param 0) m
          pval 1 = mode1 (param 1) m
          pval 2 = mode2 (param 2) m

-- Interprets a program written in Intcode.
interpret :: [Int] -> IO [Int]
interpret = (toList <$>) . (interpret' 0) . fromList
    where interpret' :: Int -> Memory -> IO Memory
          interpret' ip m = if V.null pp then pure m
                                         else let op = V.head pp
                                                  p = V.tail pp
                                                  in do
                                                      (op, p) <- pure $ (V.head pp, V.tail pp)
                                                      when debugEnabled $ putStrLn $ "Operation: " <> show op <> " " <> show p <> show m
                                                      if V.null pp || V.head pp == 99 then pure m
                                                                                      else do
                                                                                          (ipu, m') <- performOp op p m
                                                                                          interpret' (ipu $ ip + 1) m'
              where pp = V.drop ip m


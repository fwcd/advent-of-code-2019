import Data.Maybe
import Data.Vector
import Data.Vector.Unboxed as V
import Data.Vector.Unboxed.Mutable as VM

-- Day 7: Amplification Circuit
-- aka. Intcode interpreter v3

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

-- Performs a single operation with automated inputs and outputs in Intcode.
-- The first argument is the opcode.
performOp :: Int -> [Int] -> Memory -> Memory -> ([Int], Maybe Int, InstructionPointerUpdate, Memory)
performOp o is p m = case op of
                       -- Add
                       1 -> noIO ((+3), replaceNth n ((pval 0) + (pval 1)) m)
                          where n = param 2

                       -- Multiply
                       2 -> noIO ((+3), replaceNth n ((pval 0) * (pval 1)) m)
                          where n = param 2

                       -- Input
                       3 -> (is', Nothing, (+1), replaceNth n i m)
                          where n = param 0
                                (i:is') = is

                       -- Print
                       4 -> (is, Just $ pval 0, (+1), m)
                       
                       -- Jump-if-true
                       5 -> noIO (if (pval 0) /= 0 then const $ pval 1 else (+2), m)
                       
                       -- Jump-if-false
                       6 -> noIO (if (pval 0) == 0 then const $ pval 1 else (+2), m)

                       -- Less-than
                       7 -> noIO ((+3), replaceNth n (intOfBool $ (pval 0) < (pval 1)) m)
                          where n = param 2
                       
                       -- Equals
                       8 -> noIO ((+3), replaceNth n (intOfBool $ (pval 0) == (pval 1)) m)
                          where n = param 2

                       -- Unknown
                       n -> error $ "Invalid op " <> show n
    where (mode2, mode1, mode0, op) = parseOp o

          param :: Int -> Int
          param = (p V.!)
          pval :: Int -> Int
          pval 0 = mode0 (param 0) m
          pval 1 = mode1 (param 1) m
          pval 2 = mode2 (param 2) m

          noIO :: (a, b) -> ([Int], Maybe c, a, b)
          noIO (x, y) = (is, Nothing, x, y)

-- Interprets a program written in Intcode with the given list of inputs, producing a list of outputs.
interpretWith :: [Int] -> [Int] -> [Int]
interpretWith is = Prelude.reverse . fst . (interpret' 0 is []) . V.fromList
    where interpret' :: Int -> [Int] -> [Int] -> Memory -> ([Int], Memory)
          interpret' ip is os m = if V.null pp then pure m
                                               else let op = V.head pp
                                                        p = V.tail pp
                                                        in if V.null pp || V.head pp == 99 then (os, m)
                                                                                           else let (is', o, ipUpdate, m') = performOp op is p m
                                                                                                    ip' = ipUpdate $ ip + 1
                                                                                                    os' = maybeToList o <> os
                                                                                                    in interpret' ip' is' os' m'
              where pp = V.drop ip m

-- Part 1.

-- Evaluates the thruster signal using the given phase setting sequence on the given program.
thrusterSignal :: [Int] -> [Int] -> Int
thrusterSignal = thrusterSignal' 0
    where thrusterSignal' :: Int -> [Int] -> [Int] -> Int
          thrusterSignal' i [] _ = i
          thrusterSignal' i (p:ps) pro = thrusterSignal' (Prelude.head $ interpretWith [p, i] pro) ps pro

import Control.Monad.State
import Data.Maybe
import Debug.Trace
import qualified Data.List as L
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Mutable as VUM

-- Day 9: Sensor Boost
-- aka. Intcode interpreter v4

type Memory = VU.Vector Int
type InstructionPointerUpdate = Int -> Int
data IntcodeMachine = IntcodeMachine { memory :: Memory, instPointer :: Int, relativeBase :: Int } deriving Show

-- A parameter mode determines how an operation's parameter should be turned into a value.
type ParameterMode = Int -> Int -> Memory -> (Int, Int)

immediateMode :: ParameterMode
immediateMode x r m = (error "Immediate-mode parameters have no position", x)

positionMode :: ParameterMode
positionMode x r m = (x, m VU.! x) 

relativeMode :: ParameterMode
relativeMode x r m = (x + r, m VU.! x + r)

modeOf :: Int -> ParameterMode
modeOf 0 = positionMode
modeOf 1 = immediateMode
modeOf 2 = relativeMode

replaceNth :: VU.Unbox a => Int -> a -> VU.Vector a -> VU.Vector a
replaceNth n z = VU.modify $ \v -> VUM.write v n z

replaceNthBoxed :: Int -> a -> V.Vector a -> V.Vector a
replaceNthBoxed n z = V.modify $ \v -> VM.write v n z

parseOp :: Int -> (ParameterMode, ParameterMode, ParameterMode, Int)
parseOp o = (modeOf $ (o `div` 10000) `mod` 10,
             modeOf $ (o `div` 1000)  `mod` 10,
             modeOf $ (o `div` 100)   `mod` 10,
                       o              `mod` 100)

intOfBool :: Bool -> Int
intOfBool True = 1
intOfBool False = 0

expectJust :: String -> Maybe a -> a
expectJust _ (Just x) = x
expectJust msg Nothing = error msg

safeTail :: [a] -> [a]
safeTail [] = []
safeTail (x:xs) = xs

-- Performs a single operation with automated inputs and outputs in Intcode.
-- The first parameter is the opcode,
-- the second parameter the input,
-- the third parameter the relative base,
-- the fourth parameter the memory starting at the parameter
-- and the fifth parameter the entire memory.
-- Returns the output, whether the input was used, the instruction pointer update function, the updated relative base and the new memory.
performOp :: Int -> Maybe Int -> Int -> Memory -> Memory -> (Maybe Int, Bool, Int, InstructionPointerUpdate, Memory)
performOp o i r p m = case op of
                       -- Add
                       1 -> noIO ((+3), replaceNth n ((pval 0) + (pval 1)) m)
                          where n = ppos 2
                       -- Multiply
                       2 -> noIO ((+3), replaceNth n ((pval 0) * (pval 1)) m)
                          where n = ppos 2
                       -- Input
                       3 -> (Nothing, True, r, (+1), replaceNth n inp m)
                          where n = ppos 0
                                inp = expectJust "Missing input during op 3!" i
                       -- Print
                       4 -> (Just $ pval 0, False, r, (+1), m)
                       -- Jump-if-true
                       5 -> noIO (if (pval 0) /= 0 then const $ pval 1 else (+2), m)
                       -- Jump-if-false
                       6 -> noIO (if (pval 0) == 0 then const $ pval 1 else (+2), m)
                       -- Less-than
                       7 -> noIO ((+3), replaceNth n (intOfBool $ (pval 0) < (pval 1)) m)
                          where n = ppos 2
                       -- Equals
                       8 -> noIO ((+3), replaceNth n (intOfBool $ (pval 0) == (pval 1)) m)
                          where n = ppos 2
                       -- Update relative base
                       9 -> (Nothing, False, pval 0, (+1), m)
                       -- Unknown
                       n -> error $ "Invalid op " <> show n
    where (mode2, mode1, mode0, op) = parseOp o
          mode :: Int -> ParameterMode
          mode 0 = mode0
          mode 1 = mode1
          mode 2 = mode2
          param :: Int -> (Int, Int)
          param x = mode x r (p VU.! x) m
          pval :: Int -> Int
          pval = snd . param
          ppos :: Int -> Int
          ppos = fst . param
          noIO :: (a, b) -> (Maybe c, Bool, Int, a, b)
          noIO (x, y) = (Nothing, False, r, x, y)

-- Performs the next operation on the Intcode computer, possibly producing output and possibly halting.
-- The resulting boolean determines whether the machine is "still running".
performNextOp :: State IntcodeMachine (Maybe Int, Bool)
performNextOp = (\(o, c, _) -> (o, c)) <$> performNextOpWith Nothing

-- Performs the next operation on the Intcode computer with the given input, possibly producing output and possibly halting.
-- The resulting boolean determines whether the machine is "still running" and whether the input was used.
performNextOpWith :: Maybe Int -> State IntcodeMachine (Maybe Int, Bool, Bool)
performNextOpWith i = do
    mcn <- get
    let ip = instPointer mcn
        r = relativeBase mcn
        m = memory mcn
    
    case nextInstruction mcn of
        Just (op, p) | op /= 99 -> let (o, usedInput, r', ipUpdate, m') = performOp op i r p m
                                       ip' = ipUpdate $ ip + 1
                                       in do
                                           put $ IntcodeMachine { memory = m', instPointer = ip', relativeBase = r' }
                                           return (o, usedInput, True)
        _ -> return (Nothing, False, False)

-- Fetches the next opcode and the subsequent memory cells.
nextInstruction :: IntcodeMachine -> Maybe (Int, Memory)
nextInstruction mcn | VU.null pp = Nothing
                    | otherwise = Just (VU.head pp, VU.tail pp)
    where pp = VU.drop (instPointer mcn) (memory mcn)

-- Performs the next operations on the Intcode computer using the given input sequence.
-- Returns the outputs and whether the machine should continue.
performNextOpsWith :: [Int] -> State IntcodeMachine ([Int], Bool)
performNextOpsWith is = do
    (o, usedInput, continue) <- performNextOpWith $ listToMaybe is
    let is' = if usedInput then safeTail is
                           else is
    mcn <- get
    let nextOpUsesInput = maybe False (== 3) $ fst <$> nextInstruction mcn

    if continue && not (null is' && nextOpUsesInput) then do (o', continue') <- performNextOpsWith is'
                                                             return (maybeToList o ++ o', continue')
                                                     else return (maybeToList o, continue)

-- Creates a new machine with the given program loaded.
newMachine :: [Int] -> IntcodeMachine
newMachine pro = IntcodeMachine { memory = VU.fromList pro, instPointer = 0, relativeBase = 0 }

-- Interprets a program written in Intcode with the given list of inputs, producing a list of outputs.
interpretWith :: [Int] -> [Int] -> [Int]
interpretWith is = fst . runState (interpret' is) . newMachine
    where interpret' :: [Int] -> State IntcodeMachine [Int]
          interpret' is = do
              (o, usedInput, continue) <- performNextOpWith $ listToMaybe is
              let is' = if usedInput then safeTail is
                                     else is
              if continue then do os <- interpret' is'
                                  return $ maybeToList o ++ os
                          else return $ maybeToList o

-- Interprets a program without inputs.
interpret :: [Int] -> [Int]
interpret = interpretWith []

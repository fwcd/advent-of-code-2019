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
data IntcodeMachine = IntcodeMachine { memory :: Memory, instPtr :: Int, relativeBase :: Int } deriving Show

-- A parameter mode determines how an operation's parameter should be turned into a value.
type ParameterMode = Int -> Int -> Memory -> (Int, Int)

(!) :: Memory -> Int -> Int
m ! x | x < 0 = error $ "Memory addresses may not be negative: " <> show x
      | x < VU.length m = m VU.! x
      | otherwise = 0

immediateMode :: ParameterMode
immediateMode x r m = (x, x)

positionMode :: ParameterMode
positionMode x r m = (x, m ! x) 

relativeMode :: ParameterMode
relativeMode x r m = (x + r, m ! (x + r))

modeOf :: Int -> ParameterMode
modeOf 0 = positionMode
modeOf 1 = immediateMode
modeOf 2 = relativeMode

replaceNth :: VU.Unbox a => Int -> a -> VU.Vector a -> VU.Vector a
replaceNth n z = VU.modify $ \vm -> VUM.write vm n z

replaceNthBoxed :: Int -> a -> V.Vector a -> V.Vector a
replaceNthBoxed n z = V.modify $ \v -> VM.write v n z

ignore :: Functor f => f a -> f ()
ignore = fmap (\_ -> ())

ensureSize :: Int -> Memory -> Memory
ensureSize n m | n >= l = m VU.++ VU.replicate (n - l) 0
               | otherwise = m
    where l = VU.length m

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
-- the third parameter the memory starting at the parameter.
-- Returns the output and whether the input was used.
performOp :: Int -> Maybe Int -> Memory -> State IntcodeMachine (Maybe Int, Bool)
performOp o i p = trace ("Performing op " <> show op <> " on " <> show p) $ case op of
                       -- Add
                       1 -> do
                           x <- pval 0
                           y <- pval 1
                           n <- ppos 2
                           memSet n $ x + y
                           moveIP (+3)
                           return noIO
                       -- Multiply
                       2 -> do
                           x <- pval 0
                           y <- pval 1
                           n <- ppos 2
                           m0 <- get
                           memSet n $ x * y
                           moveIP (+3)
                           return noIO
                       -- Input
                       3 -> do
                           n <- ppos 0
                           let inp = expectJust "Missing input during op 3!" i
                           memSet n inp
                           moveIP (+1)
                           return (Nothing, True)
                       -- Print
                       4 -> do
                           x <- pval 0
                           moveIP (+1)
                           return (Just x, False)
                       -- Jump-if-true
                       5 -> do
                           x <- pval 0
                           y <- pval 1
                           moveIP $ if x /= 0 then const y else (+2)
                           return noIO
                       -- Jump-if-false
                       6 -> do
                           x <- pval 0
                           y <- pval 2
                           moveIP $ if x == 0 then const y else (+2)
                           return noIO
                       -- Less-than
                       7 -> do
                           x <- pval 0
                           y <- pval 1
                           n <- ppos 2
                           memSet n $ intOfBool $ x < y
                           moveIP (+3)
                           return noIO
                       -- Equals
                       8 -> do
                           x <- pval 0
                           y <- pval 1
                           n <- ppos 2
                           memSet n $ intOfBool $ x == y
                           moveIP (+3)
                           return noIO
                       -- Update relative base
                       9 -> do
                           x <- pval 0
                           moveBase $ const x
                           moveIP (+1)
                           return noIO
                       -- Unknown
                       n -> error $ "Invalid op " <> show n
    where (mode2, mode1, mode0, op) = parseOp o
          mode :: Int -> ParameterMode
          mode 0 = mode0
          mode 1 = mode1
          mode 2 = mode2
          param :: Int -> State IntcodeMachine (Int, Int)
          param x = do
              mcn <- get
              return $ mode x (p ! x) (relativeBase mcn) $ memory mcn
          pval :: Int -> State IntcodeMachine Int
          pval = (snd <$>) . param
          ppos :: Int -> State IntcodeMachine Int
          ppos = (fst <$>) . param
          noIO = (Nothing, False)
          moveIP :: InstructionPointerUpdate -> State IntcodeMachine ()
          moveIP f = modify $ \mcn -> mcn { instPtr = f $ 1 + instPtr mcn }
          moveBase :: (Int -> Int) -> State IntcodeMachine ()
          moveBase f = modify $ \mcn -> mcn { relativeBase = f $ relativeBase mcn }
          memSet :: Int -> Int -> State IntcodeMachine ()
          memSet n x = modify $ \mcn -> mcn { memory = replaceNth n x $ ensureSize (n + 1) $ memory mcn }

-- Performs the next operation on the Intcode computer, possibly producing output and possibly halting.
-- The resulting boolean determines whether the machine is "still running".
performNextOp :: State IntcodeMachine (Maybe Int, Bool)
performNextOp = (\(o, c, _) -> (o, c)) <$> performNextOpWith Nothing

-- Performs the next operation on the Intcode computer with the given input, possibly producing output and possibly halting.
-- The resulting boolean determines whether the machine is "still running" and whether the input was used.
performNextOpWith :: Maybe Int -> State IntcodeMachine (Maybe Int, Bool, Bool)
performNextOpWith i = do
    mcn <- get
    
    case nextInstruction mcn of
        Just (op, p) | op /= 99 -> do (o, usedInput) <- performOp op i p
                                      return (o, usedInput, True)
        _ -> return (Nothing, False, False)

-- Fetches the next opcode and the subsequent memory cells.
nextInstruction :: IntcodeMachine -> Maybe (Int, Memory)
nextInstruction mcn | VU.null pp = Nothing
                    | otherwise = Just (VU.head pp, VU.tail pp)
    where pp = VU.drop (instPtr mcn) (memory mcn)

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
newMachine pro = IntcodeMachine { memory = VU.fromList pro, instPtr = 0, relativeBase = 0 }

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

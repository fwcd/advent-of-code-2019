import Control.Monad.State
import Data.Maybe
import Debug.Trace
import qualified Data.List as L
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Mutable as VUM

-- Day 7: Amplification Circuit
-- aka. Intcode interpreter v3

type Memory = VU.Vector Int
type InstructionPointerUpdate = Int -> Int
data IntcodeMachine = IntcodeMachine { memory :: Memory, instPointer :: Int }

-- A parameter mode determines how an operation's parameter should be turned into a value.
type ParameterMode = Int -> Memory -> Int

immediateMode :: ParameterMode
immediateMode = const

positionMode :: ParameterMode
positionMode = flip (VU.!)

modeOf :: Int -> ParameterMode
modeOf 0 = positionMode
modeOf 1 = immediateMode

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
-- The first argument is the opcode, the second argument the input.
performOp :: Int -> Maybe Int -> Memory -> Memory -> (Maybe Int, InstructionPointerUpdate, Memory)
performOp o i p m = case op of
                       -- Add
                       1 -> noIO ((+3), replaceNth n ((pval 0) + (pval 1)) m)
                          where n = param 2

                       -- Multiply
                       2 -> noIO ((+3), replaceNth n ((pval 0) * (pval 1)) m)
                          where n = param 2

                       -- Input
                       3 -> (Nothing, (+1), replaceNth n inp m)
                          where n = param 0
                                inp = expectJust "Missing input during op 3!" i

                       -- Print
                       4 -> (Just $ pval 0, (+1), m)
                       
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
          param = (p VU.!)
          pval :: Int -> Int
          pval 0 = mode0 (param 0) m
          pval 1 = mode1 (param 1) m
          pval 2 = mode2 (param 2) m

          noIO :: (a, b) -> (Maybe c, a, b)
          noIO (x, y) = (Nothing, x, y)

-- Performs the next operation on the Intcode computer, possibly producing output and possibly halting.
-- The resulting boolean determines whether the machine is "still running".
performNextOp :: State IntcodeMachine (Maybe Int, Bool)
performNextOp = performNextOpWith Nothing

-- Performs the next operation on the Intcode computer with the given input, possibly producing output and possibly halting.
-- The resulting boolean determines whether the machine is "still running".
performNextOpWith :: Maybe Int -> State IntcodeMachine (Maybe Int, Bool)
performNextOpWith i = do
    mcn <- get
    let ip = instPointer mcn
        m = memory mcn
        pp = VU.drop ip m
        
    if VU.null pp then return (Nothing, False)
                 else let op = VU.head pp
                          p = VU.tail pp
                          in if VU.head pp == 99 then return (Nothing, False)
                                                else let (o, ipUpdate, m') = performOp op i p m
                                                         ip' = ipUpdate $ ip + 1
                                                         in do
                                                             put $ IntcodeMachine { memory = m', instPointer = ip' }
                                                             return (o, True)

-- Interprets a program written in Intcode with the given list of inputs, producing a list of outputs.
interpretWith :: [Int] -> [Int] -> [Int]
interpretWith is pro = fst $ runState (interpret' is) $ IntcodeMachine { memory = VU.fromList pro, instPointer = 0 }
    where interpret' :: [Int] -> State IntcodeMachine [Int]
          interpret' is = do
              (o, continue) <- performNextOpWith $ listToMaybe is
              if continue then do os <- interpret' $ safeTail is
                                  return $ maybeToList o ++ os
                          else return $ maybeToList o

-- Part 1.

-- Evaluates the thruster signal using the given phase setting sequence on the given program/"amp configuration".
thrusterSignal1 :: [Int] -> [Int] -> Int
thrusterSignal1 = thrusterSignal1' 0
    where thrusterSignal1' :: Int -> [Int] -> [Int] -> Int
          thrusterSignal1' i [] _ = i
          thrusterSignal1' i (p:ps) pro = thrusterSignal1' (head $ interpretWith [p, i] pro) ps pro

-- Brute-forces the maximum thruster signal for a given program/"amp configuration".
maxThrusterSignal1 :: [Int] -> Int
maxThrusterSignal1 pro = L.maximum $ flip thrusterSignal1 pro <$> L.permutations [0..4]

-- Part 2.


-- Evaluates the thruster signal as a feedback loop of "amps".
thrusterSignal2 :: [Int] -> [Int] -> Int
thrusterSignal2 is pro = fst $ runState (thrusterSignal2' 0 $ V.fromList is) $ V.replicate 5 $ IntcodeMachine { memory = VU.fromList pro, instPointer = 0 }
    where thrusterSignal2' :: Int -> V.Vector Int -> State (V.Vector IntcodeMachine) Int
          thrusterSignal2' k is = trace ("Now @ " <> show k <> " with is " <> show is) $ do
              mcns <- get

              let mcn = mcns V.! k
                  ((o, continue), mcn') = runState (performNextOpWith $ Just $ is V.! k) mcn
                  out = expectJust ("Amp " <> show k <> " produced no output") o
                  is' = replaceNthBoxed k out is
              modify $ replaceNthBoxed k mcn'

              if k >= (V.length mcns) - 1 then if continue then thrusterSignal2' 0 is'
                                                           else return out
                                          else thrusterSignal2' (k + 1) is'

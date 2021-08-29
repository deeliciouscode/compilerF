module Emulator where

import DataStructures
import GHC.Types (Any)
import EmulatorTests
import Helpers


-- Notes
-- P := P + 1 bei jedem neuen Befehl es sei denn es wird 
-- expizit geändert (Reset, Reduce, Return)

-- Zyklus:
-- P := 0; I := Code[P]
-- while (I /= Halt) 
--      P := P + 1;
--      <Run Instruction in I>
--      I := Code[P];


-- Aliases
type NumArgs    = Int
type CodeAdr    = Int
type HeapAdr    = Int
data Value      = Bool Bool | Int Int
            deriving (Show, Eq)

-- Stack & Heap Structures
data StackType  = C Int
                | H Int
            deriving (Show, Eq)

data HeapType   = DEF String NumArgs CodeAdr
                | IND HeapAdr
                | APP' HeapAdr HeapAdr
                | VAL Value
            deriving (Show, Eq)

-- Storages 
type Code       = [Instructions]
type Stack      = [StackType]
type Global     = [(String, (NumArgs, CodeAdr))] -- Maybe have function that translates Operators to strings
type Heap       = [HeapType]

-- Registers
type I          = Instructions
type T          = Int
type P          = Int

-- Results
data Result     = RBool Bool
                | RInt Int
                | Placeholder
                | Debug String
                | RuntimeError String
            deriving (Show)

-- Test
runtest1 :: Result
runtest1 = emulate (code1, stack1, global1, heap1, i1, t1, p1)

-- Emulator
type Context = (Code, Stack, Global, Heap, I, T, P)

emulate :: Context -> Result
emulate all@(code, stack, global, heap, i, t, p)
                                | null code             = RuntimeError "Received no code"
                                | stack /= []           = RuntimeError "Received non empty stack"
                                | null global           = RuntimeError "Received empty global environment"
                                | heap /= []            = RuntimeError "Received non empty heap"
                                | i /= EmptyInstruction = RuntimeError "Received non EmptyInstruction"
                                | t /= 0                = RuntimeError "Received non zero t"
                                | p /= 0                = RuntimeError "Received non zero p"
                                | otherwise             = execute $ fillHeap all


-- Fill up heap with global environment
fillHeap :: Context -> Context
fillHeap (code, stack, global, heap, i, t, p) = (code, stack, global, filledHeap, i, t, p)
                                where
                                    filledHeap = fillFromGlobal global heap

fillFromGlobal :: Global -> Heap -> Heap
fillFromGlobal [] heap                      = heap
fillFromGlobal ((name, (n, a)):rest) heap   = DEF name n a : fillFromGlobal rest heap

execute :: Context -> Result
execute all@(code, stack, global, heap, i, t, p) =
    case readCode code p of
        -- Reset               -> Debug (show Reset)
        -- Reset               -> Debug (show . execReset $ increaseP all)
        Reset               -> execute . execReset $ increaseP all

        -- Pushfun name        -> Debug (show (Pushfun name))
        -- Pushfun name        -> Debug (show . get2 . execPushfun name $ increaseP all)
        Pushfun name        -> execute . execPushfun name $ increaseP all

        -- Call                -> Debug (show Call)
        -- Call                -> Debug (show . get2 . execCall $ increaseP all)
        Call                -> execute . execCall $ increaseP all

        -- Update int          -> Debug (show (Update int))
        -- Update int          -> Debug (show . get2 . execUpdate int $ increaseP all)
        Update int          -> execute . execUpdate int $ increaseP all

        -- Slide int           -> Debug (show (Slide int))
        -- Slide int           -> Debug (show . get2 . execSlide int $ increaseP all)
        Slide int           -> execute . execSlide int $ increaseP all

        -- Unwind              -> Debug (show Unwind)
        -- Unwind              -> Debug (show . get4 . execUnwind $ increaseP all)
        Unwind              -> execute . execUnwind $ increaseP all

        -- Pushval val         -> Debug (show (Pushval val))
        -- Pushval val         -> Debug (show . get6 . execPushval val $ increaseP all)
        Pushval val         -> execute . execPushval val $ increaseP all

        -- Return              -> Debug (show Return)
        -- Return              -> Debug (show . get2 . execReturn $ increaseP all)
        Return              -> execute . execReturn $ increaseP all

        -- Halt                -> Debug (show Halt)
        Halt                -> execHalt $ increaseP all

        Pushparam int       -> Debug (show (Pushparam int))
        Makeapp             -> Debug (show Makeapp)
        Operator op         -> Debug (show (Operator op))
        Alloc               -> Debug (show Alloc)
        SlideLet int        -> Debug (show (SlideLet int))
        EmptyInstruction    -> Debug (show EmptyInstruction)


readCode :: Code -> P -> I
readCode code p = code !! p

increaseP :: Context -> Context
increaseP (code, stack, global, heap, i, t, p) = (code, stack, global, heap, i, t, p')
                                where p' = p + 1

execReset :: Context -> Context
execReset (code, stack, global, heap, i, t, p) = (code, stack, global, heap, i, t', p)
                                where
                                    t' = -1

execPushfun :: String -> Context -> Context
execPushfun name (code, stack, global, heap, i, t, p) = (code, stack', global, heap, i, t', p)
                                where
                                    t' = t + 1
                                    stack' = pushHRef name stack heap

pushHRef :: String -> Stack -> Heap -> Stack
pushHRef name stack heap = stack ++ [H index]
                                where
                                    index = indexByName name heap

indexByName :: String -> Heap -> Int
indexByName name heap = indexByNameFromZero name heap 0

indexByNameFromZero :: String -> Heap -> Int -> Int
indexByNameFromZero name (DEF defName n a : rest) i
                                            | name == defName   = i
                                            | otherwise         = indexByNameFromZero name rest (i + 1)
indexByNameFromZero name _ i = error "Something went wrong in indexByNameFromZero. Only DEF entry expected here."

execCall :: Context -> Context
execCall pass@(code, stack, global, heap, i, t, p) =
    case getAdr heap stack t of
        (DEF name numArgs codeAddr) -> (code, stack', global, heap, i, t', p')
                                where
                                    t'      = t + 1
                                    stack'  = pushCRef p stack
                                    p'      = codeAddr

        (VAL val)                   -> pass
        _                           ->  error "Something went wrong in execCall Only DEF or VAL entry expected here."

pushCRef :: Int -> Stack -> Stack
pushCRef i stack = stack ++ [C i]

getAdr :: Heap -> Stack -> T -> HeapType
getAdr heap stack t =
    case stack !! t of
        (C i) -> error "Something went wrong in getAdr. Only H i entry expected here."
        (H i) -> heap !! i
        --   DEF name numArgs codeAddr -> codeAddr
        --   _                         -> error $ "Something went wrong in getAdr. Only DEF entry expected here." 
        --     ++ " stack: " ++ show stack ++ " heap: " ++ show heap ++ " t:" ++ show t 

execUpdate :: NumArgs -> Context -> Context
execUpdate numArgs (code, stack, global, heap, i, t, p) =
    case stack !! t of
        (C index) -> error "Something went wrong in execUpdate. C i entry not expected here."
        (H index) -> (code, stack, global, heap', i, t, p)
            where
                heap' = setIND stack heap numArgs index t

setIND :: Stack -> Heap -> NumArgs -> Int -> Int -> Heap
setIND stack heap numArgs index t = let (head, elem:tail) = splitAt indexForSplit heap in head ++ [IND index] ++ tail
            where
                indexForSplit = case stack !! (t - numArgs - 2) of
                    (C i) -> error "Something went wrong in setIND. C i entry not expected here."
                    (H i) -> i

execSlide :: Int -> Context -> Context
execSlide n (code, stack, global, heap, i, t, p) = (code, stack', global, heap, i, t', p)
            where
                stack' = cutFromStack stack 2 n
                t' = t - n

cutFromStack :: Stack -> Int -> Int -> Stack
cutFromStack stack offset n = let (head, twoLast) = splitAt (length stack - offset) stack in cutN head n ++ twoLast
            where
                cutN list n = take (length list - n) list

execUnwind :: Context -> Context
execUnwind pass@(code, stack, global, heap, i, t, p) =
    case stack !! t of
        (C index) -> error "Something went wrong in execUnwind. Only H i entry expected here."
        (H index) -> case heap !! index of
            IND heapAdr -> (code, stack', global, heap, i, t, p')
                where
                    stack' = cutFromStack stack 0 1 ++ [C heapAdr] -- May be wrong - need to test 
                    p' = p - 1
            APP' heapAdr1 heapAdr2 -> (code, stack, global, heap, i, t', p)
                where
                    t' = t + 1
                    stack' = stack ++ [C heapAdr1]
                    p' = p - 1
            DEF name numArgs codeAddr -> pass
            VAL value -> pass

execPushval :: Expr -> Context -> Context
execPushval (IntX x) (code, stack, global, heap, i, t, p) = (code, stack', global, heap', i, t', p)
                where
                    t' = t + 1
                    heap' = heap ++ [VAL (Int x)]
                    stack' = stack ++ [H (length heap' - 1)]

execPushval (BoolX x) (code, stack, global, heap, i, t, p) = (code, stack', global, heap', i, t', p)
                where
                    t' = t + 1
                    heap' = heap ++ [VAL (Bool x)]
                    stack' = stack ++ [H (length heap' - 1)]
execPushval _ _ = error "Something went wrong in execPushval. Only IntX or BoolX is expected here as a first argument."

execReturn :: Context -> Context
execReturn (code, stack, global, heap, i, t, p) =
    case stack !! (t-1) of
        C ix -> (code, stack', global, heap, i, t', p')
                where
                    p' = ix
                    stack' = cutFromStack stack 1 1
                    t' = t - 1
        H i -> error "Something went wrong in execReturn. Only C i is expected here."


execHalt :: Context -> Result
execHalt (code, stack, global, heap, i, t, p)
    | length stack /= 1 = error "Something went wrong in execHalt. Only one element should be on stack."
    | otherwise = case head stack of
        C n -> error "Something went wrong in execHalt. Only H i is expected here."
        H n -> case heap !! n of
          VAL (Bool bool)   -> RBool bool
          VAL (Int int)     -> RInt int
          _ -> error "Something went wrong in execHalt. Only VAL value is expected here."


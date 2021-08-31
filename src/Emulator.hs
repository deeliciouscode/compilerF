module Emulator where

import DataStructures
import GHC.Types (Any)
import EmulatorTests
import Helpers


-- Notes
-- P := P + 1 bei jedem neuen Befehl es sei denn es wird 
-- expizit geändert.

-- Zyklus:
-- P := 0; I := Code[P]
-- while (I /= Halt) 
--      P := P + 1;
--      <Run Instruction in I>
--      I := Code[P];


-- Test
runtest1 :: Result
runtest1 = emulate (code1, stack1, global1, heap1, i1, t1, p1)

runtest2 :: Result
runtest2 = emulate (code2, stack2, global2, heap2, i2, t2, p2)

runtest3 :: Result
runtest3 = emulate (code3, stack2, global2, heap2, i2, t2, p2)

-- data HeapType   = DEF String NumArgs CodeAdr
--                 | IND HeapAdr
--                 | APP' HeapAdr HeapAdr
--                 | VAL Value

testHeap :: [HeapType]
testHeap = [DEF "a" 1 2, IND 1, APP' 1 2, VAL (Int 1)]

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
        -- Pushfun "c"         -> Debug (show . get2 . execPushfun "a" $ increaseP all)
        Pushfun name        -> execute . execPushfun name $ increaseP all

        -- Call                -> Debug (show Call)
        -- Call                -> Debug (show . get2 . execCall $ increaseP all)
        Call                -> execute . execCall $ increaseP all

        -- Update int          -> Debug (show (Update int))
        -- Update int          -> Debug (show . get4 . execUpdate int $ increaseP all)
        -- Update 2            -> Debug (show . get4 . execUpdate 2 $ increaseP all)
        Update int          -> execute . execUpdate int $ increaseP all

        -- Slide int           -> Debug (show (Slide int))
        -- Slide int           -> Debug (show [(get4 . execSlide int $ increaseP all) !! 23])
        -- Slide 3             -> Debug (show . get6 . execSlide 3 $ increaseP all)
        Slide int           -> execute . execSlide int $ increaseP all

        -- Unwind              -> Debug (show Unwind)
        -- Unwind              
        --                     | i == Halt -> Debug (show . get2 . execUnwind $ increaseP all)
        --                     | otherwise -> execute . execUnwind $ increaseP all
        Unwind              -> execute . execUnwind $ increaseP all

        -- Pushval val         -> Debug (show (Pushval val))
        -- Pushval val         -> Debug (show . get6 . execPushval val $ increaseP all)
        Pushval val         -> execute . execPushval val $ increaseP all

        -- Return              -> Debug (show Return)
        -- Return              -> Debug (show . get6 . execReturn $ increaseP all)
        Return              -> execute . execReturn $ increaseP all

        -- Halt                -> Debug (show Halt)
        Halt                -> execHalt $ increaseP all

        -- Makeapp             -> Debug (show Makeapp)
        -- Makeapp             -> Debug ((show . get2 . execMakeapp $ increaseP all) ++ (show . get4 . execMakeapp $ increaseP all) ++ (show . get6 . execMakeapp $ increaseP all))
        Makeapp             -> execute . execMakeapp $ increaseP all

        -- Pushparam n         -> Debug (show (Pushparam int))
        -- Pushparam n
        --                     | i == Halt -> Debug (show . get6 . execPushparam n $ increaseP all)
        --                     | otherwise -> execute . execPushparam n $ increaseP all
        Pushparam n         -> execute . execPushparam n $ increaseP all
        
        -- Operator op         -> Debug (show (Operator op))
        -- Operator op         -> Debug (show . get6 . execOperator op $ increaseP all)
        Operator op         -> execute . execOperator op $ increaseP all

        Alloc               -> Debug (show Alloc)
        
        SlideLet int        -> Debug (show (SlideLet int))
        
        -- should never be called
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
indexByNameFromZero name [] i = error $ "Something went wrong in indexByNameFromZero. Name not found. " ++ show name  
indexByNameFromZero name (DEF defName n a : rest) i
                                            | name == defName   = i
indexByNameFromZero name (other:rest) i = indexByNameFromZero name rest (i + 1)

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
cutFromStack stack offset n = let (head, rest) = splitAt (length stack - offset) stack in cutN head n ++ rest
            where
                cutN list n = take (length list - n) list

execUnwind :: Context -> Context
execUnwind pass@(code, stack, global, heap, i, t, p) =
    case stack !! t of
        (C index) -> error $ "Something went wrong in execUnwind. Only H i entry expected here. " ++ "stack: " ++ show stack
        (H index) -> case heap !! index of
            IND heapAdr -> (code, stack', global, heap, i, t, p')
                where
                    stack' = cutFromStack stack 0 1 ++ [H heapAdr] -- May be wrong - need to test 
                    p' = p - 1
            APP' heapAdr1 heapAdr2 -> (code, stack', global, heap, i, t', p')
                where
                    t' = t + 1
                    stack' = stack ++ [H heapAdr1]
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


execMakeapp :: Context -> Context
execMakeapp (code, stack, global, heap, i, t, p) = (code, stack', global, heap', i, t', p)
                where
                    heap'   = pushApp heap stack t
                    stack'  = cutFromStack stack 0 2 ++ [H (length heap' - 1)]
                    t'      = t - 1

pushApp :: Heap -> Stack -> T -> Heap
pushApp heap stack t = 
    case stack !! t of
        C i1 -> error "Something went wrong in pushApp. Only H i1 is expected here."
        H i1 -> case stack !! (t - 1) of
            C i2 -> error "Something went wrong in pushApp. Only H i2 is expected here."
            H i2 -> heap ++ [APP' i1 i2]

execPushparam :: Int -> Context -> Context
execPushparam n (code, stack, global, heap, i, t, p) = (code, stack', global, heap, i, t', p)
                where
                    t'      = t + 1
                    adr     = add2arg stack heap t' n
                    stack'  = stack ++ [adr]

add2arg :: Stack -> Heap -> T -> Int -> StackType
add2arg stack heap t n = 
    case stack !! (t - n - 2) of
        C i -> error "Something went wrong in pushApp. Only H i is expected here."
        H i -> case heap !! i of
          APP' a1 a2    -> H a2
          _             -> error "Something went wrong in pushApp. Only APP' a1 a2 is expected here."

-- TODO: clean up and generalize
execOperator :: OpInstrConstr -> Context -> Context
execOperator Negate (code, stack, global, heap, i, t, p) = 
    case stack !! t of 
        C n -> error "Something went wrong in execOperator Negate. Only H n is expected here."
        H n -> case heap !! n of
            VAL (Int x)     -> (code, stack', global, heap', i, t, p)
                where
                    stack'  = take (length stack - 1) stack ++ [H (length heap)]
                    heap'   = heap ++ [VAL (Int (-x))] -- TODO: check for correctness
            VAL (Bool b)    -> error "Something went wrong in execOperator Negate. Can't negate Bool."
            _               -> error "Something went wrong in execOperator Negate. Only Val v is expected here."
execOperator Plus (code, stack, global, heap, i, t, p) = 
    case stack !! (t-1) of
        C n -> error "Something went wrong in execOperator Plus. Only H n is expected here."
        H n -> case stack !! t of
            C m -> error "Something went wrong in execOperator Plus. Only H m is expected here."
            H m -> (code, stack', global, heap', i, t', p)
                where
                    t'      = t - 1
                    stack'  = take (length stack - 2) stack ++ [H (length heap)]
                    heap'   = let (VAL (Int x1)) = heap !! n in let (VAL (Int x2)) = heap !! m in heap ++ [VAL $ Int (x1 + x2)]
execOperator Minus (code, stack, global, heap, i, t, p) = 
    case stack !! (t-1) of
        C n -> error "Something went wrong in execOperator Minus. Only H n is expected here."
        H n -> case stack !! t of
            C m -> error "Something went wrong in execOperator Minus. Only H m is expected here."
            H m -> (code, stack', global, heap', i, t', p)
                where
                    t'      = t - 1
                    stack'  = take (length stack - 2) stack ++ [H (length heap)]
                    heap'   = let (VAL (Int x1)) = heap !! n in let (VAL (Int x2)) = heap !! m in heap ++ [VAL $ Int (x1 - x2)]
execOperator Times (code, stack, global, heap, i, t, p) =
    case stack !! (t-1) of
        C n -> error "Something went wrong in execOperator Times. Only H n is expected here."
        H n -> case stack !! t of
            C m -> error "Something went wrong in execOperator Times. Only H m is expected here."
            H m -> (code, stack', global, heap', i, t', p)
                where
                    t'      = t - 1
                    stack'  = take (length stack - 2) stack ++ [H (length heap)]
                    heap'   = let (VAL (Int x1)) = heap !! n in let (VAL (Int x2)) = heap !! m in heap ++ [VAL $ Int (x1 * x2)]
execOperator DividedBy (code, stack, global, heap, i, t, p) =
    case stack !! (t-1) of
        C n -> error "Something went wrong in execOperator DividedBy. Only H n is expected here."
        H n -> case stack !! t of
            C m -> error "Something went wrong in execOperator DividedBy. Only H m is expected here."
            H m -> (code, stack', global, heap', i, t', p)
                where
                    t'      = t - 1
                    stack'  = take (length stack - 2) stack ++ [H (length heap)]
                    heap'   = let (VAL (Int x1)) = heap !! n in let (VAL (Int x2)) = heap !! m in heap ++ [VAL $ Int (x1 `div` x2)]
execOperator LessThan (code, stack, global, heap, i, t, p) =
    case stack !! (t-1) of
        C n -> error "Something went wrong in execOperator LessThan. Only H n is expected here."
        H n -> case stack !! t of
            C m -> error "Something went wrong in execOperator LessThan. Only H m is expected here."
            H m -> (code, stack', global, heap', i, t', p)
                where
                    t'      = t - 1
                    stack'  = take (length stack - 2) stack ++ [H (length heap)]
                    heap'   = let (VAL (Int x1)) = heap !! n in let (VAL (Int x2)) = heap !! m in heap ++ [VAL $ Bool (x1 < x2)]
execOperator Equals (code, stack, global, heap, i, t, p) =
    case stack !! (t-1) of
        C n -> error "Something went wrong in execOperator Equals. Only H n is expected here."
        H n -> case stack !! t of
            C m -> error "Something went wrong in execOperator Equals. Only H m is expected here."
            H m -> (code, stack', global, heap', i, t', p)
                where
                    t'      = t - 1
                    stack'  = take (length stack - 2) stack ++ [H (length heap)]
                    heap'   = let (VAL val1) = heap !! n in let (VAL val2) = heap !! m in heap ++ [VAL $ Bool (val1 == val2)]
execOperator And (code, stack, global, heap, i, t, p) = 
    case stack !! (t-1) of
        C n -> error "Something went wrong in execOperator And. Only H n is expected here."
        H n -> case stack !! t of
            C m -> error "Something went wrong in execOperator And. Only H m is expected here."
            H m -> (code, stack', global, heap', i, t', p)
                where
                    t'      = t - 1
                    stack'  = take (length stack - 2) stack ++ [H (length heap)]
                    heap'   = let (VAL (Bool x1)) = heap !! n in let (VAL (Bool x2)) = heap !! m in heap ++ [VAL $ Bool (x1 && x2)]
execOperator Or (code, stack, global, heap, i, t, p) =
    case stack !! (t-1) of
        C n -> error "Something went wrong in execOperator Or. Only H n is expected here."
        H n -> case stack !! t of
            C m -> error "Something went wrong in execOperator Or. Only H m is expected here."
            H m -> (code, stack', global, heap', i, t', p)
                where
                    t'      = t - 1
                    stack'  = take (length stack - 2) stack ++ [H (length heap)]
                    heap'   = let (VAL (Bool x1)) = heap !! n in let (VAL (Bool x2)) = heap !! m in heap ++ [VAL $ Bool (x1 || x2)]
execOperator Not (code, stack, global, heap, i, t, p) =
    case last stack of -- like stack !! (t-1)
        C n -> error "Something went wrong in execOperator Not. Only H n is expected here."
        H n -> case heap !! n of
            VAL (Bool b)    -> (code, stack', global, heap', i, t, p)
                where
                    stack'  = take (length stack - 1) stack ++ [H (length heap)]
                    heap'   = heap ++ [VAL (Bool (not b))] -- TODO: check for correctness
            VAL (Int x)     -> error "Something went wrong in execOperator Not. Can't 'not' Int."
            _               -> error "Something went wrong in execOperator Not. Only Val v is expected here."
execOperator If (code, stack, global, heap, i, t, p) =
    case stack !! (t-2) of
        C n -> error "Something went wrong in execOperator If. Only H n is expected here."
        H n -> case heap !! n of
            VAL (Bool True)    -> (code, stack', global, heap, i, t', p) -- THEN Case
                where
                    t'      = t - 2
                    stack'  = take (length stack - 3) stack ++ [stack !! (t - 1)]
            VAL (Bool False)    -> (code, stack', global, heap, i, t', p) -- ELSE Case
                where
                    t'      = t - 2
                    stack'  = take (length stack - 3) stack ++ [stack !! t]
            VAL (Int x)     -> error "Something went wrong in execOperator If. Can't 'not' Bool."
            _               -> error "Something went wrong in execOperator If. Only Val v is expected here."

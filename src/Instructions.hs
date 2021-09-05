module Instructions where
import DataStructures

mHead :: [Instruction]
mHead = [Reset, Pushfun "main", Call, Halt]

mTail :: [Instruction]
mTail = mFalse ++ mTrue ++ mNot ++ mNegate ++  mOr ++ mAnd ++ mPlus ++ mMinus ++ mMult ++ mDiv ++ mEq ++ mLess ++ mIf

makeapp :: [Instruction] -> [Instruction]
makeapp a = a ++ [Makeapp]

push :: String -> [Instruction]
push op = [Pushfun op, Makeapp]

functionTail :: Int -> [Instruction]
functionTail n = [Update n, Slide (n+1), Unwind, Call, Return]

varTail :: [Instruction]
varTail = [Update 0, Slide 1, Unwind, Call, Return]

allocMake :: [Instruction]
allocMake = [Alloc, Makeapp]

slideLet :: Int -> [Instruction]
slideLet n = [SlideLet n]

initDef :: [DefCell]
initDef = 
    [ ("false",(0,6))
    , ("true",(0,6))
    , ("not",(1,7))
    , ("negate",(1,7))
    , ("|",(2,10))
    , ("&",(2,10))
    , ("+",(2,10))
    , ("-",(2,10))
    , ("*",(2,10))
    , ("/",(2,10))
    , ("==",(2,10))
    , ("<",(2,10))
    , ("if", (3,11))
    ]

mFalse :: [Instruction]
mFalse = [Pushval (BoolX False)
    , Update 0
    , Slide 1
    , Unwind
    , Call
    , Return
    ]

mTrue :: [Instruction]
mTrue = [Pushval (BoolX True)
    , Update 0
    , Slide 1
    , Unwind
    , Call
    , Return
    ]

mNegate :: [Instruction]
mNegate = [Pushparam 1
    , Unwind
    , Call
    , Operator Negate
    , Update 1
    , Slide 2
    , Return
    ]

mNot :: [Instruction]
mNot = [Pushparam 1
    , Unwind
    , Call
    , Operator Not
    , Update 1
    , Slide 2
    , Return
    ]

mOr :: [Instruction]
mOr = [Pushparam 1
    , Unwind
    , Call
    , Pushparam 3
    , Unwind
    , Call
    , Operator Or
    , Update 2
    , Slide 3   
    , Return
    ]    

mAnd :: [Instruction]
mAnd = [Pushparam 1
    , Unwind
    , Call
    , Pushparam 3
    , Unwind
    , Call
    , Operator And
    , Update 2
    , Slide 3
    , Return
    ]

mPlus :: [Instruction]
mPlus = [Pushparam 1
    , Unwind
    , Call
    , Pushparam 3
    , Unwind
    , Call
    , Operator Plus
    , Update 2
    , Slide 3
    , Return
    ]    

mMinus :: [Instruction]
mMinus = [Pushparam 1
    , Unwind
    , Call
    , Pushparam 3
    , Unwind
    , Call
    , Operator Minus
    , Update 2
    , Slide 3
    , Return
    ]   

mMult :: [Instruction]
mMult = [Pushparam 1
    , Unwind
    , Call
    , Pushparam 3
    , Unwind
    , Call
    , Operator Times
    , Update 2
    , Slide 3
    , Return
    ]    

mDiv :: [Instruction]
mDiv = [Pushparam 1
    , Unwind
    , Call
    , Pushparam 3
    , Unwind
    , Call
    , Operator DividedBy
    , Update 2
    , Slide 3
    , Return
    ]    

mEq :: [Instruction]
mEq = [Pushparam 1
    , Unwind
    , Call
    , Pushparam 3
    , Unwind
    , Call
    , Operator Equals
    , Update 2
    , Slide 3
    , Return
    ] 

mLess :: [Instruction]
mLess = [Pushparam 1
    , Unwind
    , Call
    , Pushparam 3
    , Unwind
    , Call
    , Operator LessThan
    , Update 2
    , Slide 3
    , Return
    ]
    
mIf :: [Instruction]
mIf = [Pushparam 1
    , Unwind
    , Call
    , Pushparam 3
    , Pushparam 5
    , Operator If
    , Update 3
    , Slide 4
    , Unwind
    , Call
    , Return
    ]
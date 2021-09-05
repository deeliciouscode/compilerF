module Instructions where
import Data.Maybe
import Parser
import Helpers
import Lexer
import DataStructures

mHead :: [Instructions]
mHead = [Reset, Pushfun "main", Call, Halt]

mTail :: [Instructions]
mTail = mFalse ++ mTrue ++ mNot ++ mNegate ++  mOr ++ mAnd ++ mPlus ++ mMinus ++ mMult ++ mDiv ++ mEq ++ mLess ++ mIf

makeapp :: [Instructions] -> [Instructions]
makeapp a = a ++ [Makeapp]

push :: String -> [Instructions]
push op = [Pushfun op, Makeapp]

functionTail :: Int -> [Instructions]
functionTail n = [Update n, Slide (n+1), Unwind, Call, Return]

varTail :: [Instructions]
varTail = [Update 0, Slide 1, Unwind, Call, Return]

allocMake :: [Instructions]
allocMake = [Alloc, Makeapp]

slideLet :: Int -> [Instructions]
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

mFalse :: [Instructions]
mFalse = [Pushval (BoolX False)
    , Update 0
    , Slide 1
    , Unwind
    , Call
    , Return
    ]

mTrue :: [Instructions]
mTrue = [Pushval (BoolX True)
    , Update 0
    , Slide 1
    , Unwind
    , Call
    , Return
    ]

mNegate :: [Instructions]
mNegate = [Pushparam 1
    , Unwind
    , Call
    , Operator Negate
    , Update 1
    , Slide 2
    , Return
    ]

mNot :: [Instructions]
mNot = [Pushparam 1
    , Unwind
    , Call
    , Operator Not
    , Update 1
    , Slide 2
    , Return
    ]

mOr :: [Instructions]
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

mAnd :: [Instructions]
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

mPlus :: [Instructions]
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

mMinus :: [Instructions]
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

mMult :: [Instructions]
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

mDiv :: [Instructions]
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

mEq :: [Instructions]
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

mLess :: [Instructions]
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
    
mIf :: [Instructions]
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
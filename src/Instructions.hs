import Data.Maybe
import Data.Map

import Parser
-- import Relude
import Data.Text

import Helpers
import Lexer
import DataStructures

import SemanticTree


mFalse = [Pushval (Bool False),
    Update 0,
    Slide 1,
    Unwind,
    Call,
    Return]

mTrue = [Pushval (Bool True),
    Update 0,
    Slide 1,
    Unwind,
    Call,
    Return]
    
-- (* unaerer negate-Operator *)
mNegate = [Pushparam 1,
    Unwind,
    Call,
    Operator Negate,
    Update 1,
    Slide 2,
    Return]

mNot = [Pushparam 1,
    Unwind,
    Call,
    Operator Not,
    Update 1,
    Slide 2,
    Return]

mAnd = [Pushparam 1,
    Unwind,
    Call,
    Pushparam 3,
    Unwind,
    Call,
    Operator And,
    Update 2,
    Slide 3,
    Return]

mOr = [Pushparam 1,
    Unwind,
    Call,
    Pushparam 3,
    Unwind,
    Call,
    Operator Or,
    Update 2,
    Slide 3,
    Return]

-- (* binaerer +-Operator *)
mPlus = [Pushparam 1,
    Unwind,
    Call,
    Pushparam 3,
    Unwind,
    Call,
    Operator Plus,
    Update 2,
    Slide 3,
    Return]

mMinus = [Pushparam 1,
    Unwind,
    Call,
    Pushparam 3,
    Unwind,
    Call,
    Operator Minus,
    Update 2,
    Slide 3,
    Return]

-- (* binaerer *-Operator *)
mMult = [Pushparam 1,
    Unwind,
    Call,
    Pushparam 3,
    Unwind,
    Call,
    Operator Times,
    Update 2,
    Slide 3,
    Return]

mDiv = [Pushparam 1,
    Unwind,
    Call,
    Pushparam 3,
    Unwind,
    Call,
    Operator DividedBy,
    Update 2,
    Slide 3,
    Return]

mEq = [Pushparam 1,
    Unwind,
    Call,
    Pushparam 3,
    Unwind,
    Call,
    Operator Equals,
    Update 2,
    Slide 3,
    Return]

mLess = [Pushparam 1,
    Unwind,
    Call,
    Pushparam 3,
    Unwind,
    Call,
    Operator LessThan,
    Update 2,
    Slide 3,
    Return]

-- (* ternaerer if-Operator *)
mIf = [Pushparam 1,
    Unwind,
    Call,
    Pushparam 3,
    Pushparam 5,
    Operator If,
    Update 3,
    Slide 4,
    Unwind,
    Call,
    Return]
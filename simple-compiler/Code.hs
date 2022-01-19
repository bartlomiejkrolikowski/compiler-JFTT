module Code
( Code(..)
, Instruction(..)
, Register(..)
, CondType(..)
, negateCond
) where

type Code = [Instruction]
data Instruction =
    Get | Put |
    Load { reg::Register } | Store { reg::Register } |
    Add { reg::Register } | Sub { reg::Register } | Shift { reg::Register } |
    Swap { reg::Register } | Reset { reg::Register } | Inc { reg::Register } |
    Dec { reg::Register } |
    Jump { offset::Int } | Jpos { offset::Int } | Jzero { offset::Int } |
    Jneg { offset::Int }
    deriving (Eq,Show)

data Register =
    Ra | Rb | Rc | Rd | Re | Rf | Rg | Rh
    deriving (Eq,Show)

data CondType =
    CondEq | CondNEq | CondLe | CondGe | CondLEq | CondGEq
    deriving (Eq,Show)

negateCond :: CondType -> CondType
negateCond CondEq = CondNEq
negateCond CondNEq = CondEq
negateCond CondLe = CondGEq
negateCond CondGe = CondLEq
negateCond CondLEq = CondGe
negateCond CondGEq = CondLe

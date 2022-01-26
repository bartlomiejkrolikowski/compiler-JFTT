module Translation.Code
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
    Jneg { offset::Int } |
    Halt
    deriving Eq

data Register =
    Ra | Rb | Rc | Rd | Re | Rf | Rg | Rh
    deriving Eq

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

instance Show Instruction where
    show Get = "GET"
    show Put = "PUT"
    show (Load r) = "LOAD " ++ show r
    show (Store r) = "STORE " ++ show r
    show (Add r) = "ADD " ++ show r
    show (Sub r) = "SUB " ++ show r
    show (Shift r) = "SHIFT " ++ show r
    show (Swap r) = "SWAP " ++ show r
    show (Reset r) = "RESET " ++ show r
    show (Inc r) = "INC " ++ show r
    show (Dec r) = "DEC " ++ show r
    show (Jump o) = "JUMP " ++ show o
    show (Jpos o) = "JPOS " ++ show o
    show (Jzero o) = "JZERO " ++ show o
    show (Jneg o) = "JNEG " ++ show o
    show Halt = "HALT"

instance Show Register where
    show Ra = "a"
    show Rb = "b"
    show Rc = "c"
    show Rd = "d"
    show Re = "e"
    show Rf = "f"
    show Rg = "g"
    show Rh = "h"

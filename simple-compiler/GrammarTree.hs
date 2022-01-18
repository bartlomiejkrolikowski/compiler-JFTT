module GrammarTree
( Program(..)
, Variables(..)
, Variable(..)
, Commands(..)
, Command(..)
, Expression(..)
, Condition(..)
, Value(..)
, Identifier(..)
, assign
) where

import qualified Data.Map as Map

data Program = Program Variables Commands deriving (Eq,Show)
type Variables = Map.Map String Variable -- nazwa zmiennej i informacje o niej
data Variable = SingleVar { name::String, assigned::Bool, address::Int } | -- address = numer komorki pamieci
                Array { name::String, begin::Int, end::Int, assigned::Bool, address::Int } | -- address = numer komorki o indeksie 0 (nawet jesli jest poza zakresem)
                Constant { name::String, assigned::Bool, address::Int } -- address = numer komorki pamieci
                deriving (Eq,Show)

-- przypisanie zmiennej wartosci
-- assign :: Variable -> Variable
-- assign SingleVar n _ -> SingleVar n True
-- assign Array n b e _ -> Array n b e True
-- assign Constant n _ -> error ("cannot assign iterator: " ++ n)

type Commands = [Command]
data Command =
  Assign Identifier Expression |
  IfElse Condition Commands Commands |
  If Condition Commands |
  While Condition Commands |
  Repeat Commands Condition |
  ForTo { nameFT::String, fromT::Value, toT::Value, commandsT::Commands } |
  ForDownTo { nameFD::String, fromD::Value, toD::Value, commandsD::Commands } |
  Read Identifier |
  Write Value
  deriving (Eq,Show)

data Expression =
  Single Value |
  Plus Value Value |
  Minus Value Value |
  Times Value Value |
  Div Value Value |
  Mod Value Value
  deriving (Eq,Show)

data Condition =
  Eq Value Value |
  NEq Value Value |
  Le Value Value |
  Ge Value Value |
  LEq Value Value |
  GEq Value Value
  deriving (Eq,Show)

data Value = Number Int | Identifier Identifier deriving (Eq,Show)
data Identifier = Var { decl::Variable } |
                  ArrNum { decl::Variable, indexInt::Int } |
                  ArrVar { decl::Variable, indexDecl::Variable }
                  deriving (Eq,Show)

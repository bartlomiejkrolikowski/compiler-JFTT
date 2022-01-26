module Grammar.Data
( Program(..)
, Declarations(..)
, Declaration(..)
, Iterator(..)
, Commands(..)
, Command(..)
, Expression(..)
, Condition(..)
, Value(..)
, Identifier(..)
) where

data Program = Program Declarations Commands deriving (Eq,Show)
type Declarations = [Declaration]
data Declaration = SingleVar { name::String } |
                   Array { name::String, begin::Int, end::Int }
                   deriving (Eq,Show)

data Iterator = Iterator { iterName::String } deriving (Eq,Show)

type Commands = [Command]
data Command =
  Assign Identifier Expression |
  IfElse Condition Commands Commands |
  If Condition Commands |
  While Condition Commands |
  Repeat Commands Condition |
  ForTo { iterFT::Iterator, fromT::Value, toT::Value, commandsT::Commands } |
  ForDownTo { iterFD::Iterator, fromD::Value, toD::Value, commandsD::Commands } |
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
data Identifier = Var { idName::String } |
                  ArrNum { idName::String, indexNum::Int } |
                  ArrVar { idName::String, indexVar::String }
                  deriving (Eq,Show)

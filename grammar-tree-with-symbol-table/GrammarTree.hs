module GrammarTree
( Program(..)
, Declarations(..)
, Declaration(..)
, Commands(..)
, Command(..)
, Expression(..)
, Condition(..)
, Value(..)
, Identifier(..)
) where

import qualified Data.Map as Map

data Program = Program Declarations Commands deriving (Eq,Show)
type Declarations = Map.Map String Declaration -- nazwa zmiennej i informacje o niej
data Declaration = Variable { name::String } |
                   Array { name::String, begin::Int, end::Int } |
                   Constant { name::String }
                   deriving (Eq,Show)

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
data Identifier = Var { decl::Declaration } |
                  ArrNum { decl::Declaration, indexInt::Int } |
                  ArrVar { decl::Declaration, indexDecl::Declaration }
                  deriving (Eq,Show)

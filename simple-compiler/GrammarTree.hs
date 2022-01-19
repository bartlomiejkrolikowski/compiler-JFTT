module GrammarTree
( Program(..)
, Declarations(..)
, Variables(..)
, Variable(..)
, Commands(..)
, Command(..)
, Expression(..)
, Condition(..)
, Value(..)
, Identifier(..)
, setAssigned
, chooseAssigned
) where

import qualified Data.Map as Map

data Program = Program Declarations (Commands, Variables) deriving (Eq,Show)
type Declarations = (Variables, Int) -- tablica zmiennych i najnizszy dostepny adres
type Variables = Map.Map String Variable -- nazwa zmiennej i informacje o niej
data Variable = SingleVar { name::String, assigned::Bool, address::Int } | -- address = numer komorki pamieci
                Array { name::String, begin::Int, end::Int, assigned::Bool, address::Int } | -- address = numer komorki o indeksie 0 (nawet jesli jest poza zakresem)
                Iterator { name::String, assigned::Bool, address::Int } -- address = numer komorki pamieci
                deriving (Eq,Show)

-- przypisanie zmiennej wartosci
setAssigned :: Variable -> Variable
setAssigned (SingleVar n _ a) = SingleVar n True a
setAssigned (Array n b e _ a) = Array n b e True a
setAssigned (Iterator n _ a) = error ("cannot assign iterator: " ++ n)

-- z dwoch efektow dzialania sciezek obliczen dla danej zmiennej wybiera ta, w ktorej zmienna jest zadeklarowana
chooseAssigned :: Variable -> Variable -> Variable
chooseAssigned (SingleVar n1 asgn1 addr1) (SingleVar n2 asgn2 addr2) = (SingleVar n1 (asgn1 || asgn2) addr1)
chooseAssigned (Array n1 b1 e1 asgn1 addr1) (Array n2 b2 e2 asgn2 addr2) = (Array n1 b1 e1 (asgn1 || asgn2) addr1)
chooseAssigned iter1@(Iterator _ _ _) (Iterator _ _ _) = iter1 -- iterator sie nie zmienia
chooseAssigned _ _ = error "one key with two different values"

type Commands = [Command]
data Command =
  Assign Identifier Expression |
  IfElse Condition Commands Commands |
  If Condition Commands |
  While Condition Commands |
  Repeat Commands Condition |
  ForTo { iterFT::Variable, fromT::Value, toT::Value, commandsT::Commands } |
  ForDownTo { iterFD::Variable, fromD::Value, toD::Value, commandsD::Commands } |
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

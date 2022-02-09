module Optimization.Data
( Var(..)
) where

data Var = OneVar { vmem::Int } |
           OneArr { vmem::Int, vcIndex::Int } |
           AllArr { vmem::Int }
           deriving (Eq, Ord, Show)

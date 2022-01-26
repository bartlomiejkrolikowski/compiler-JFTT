module Variables.Data
( Variables(..)
) where

import Grammar.Data
import qualified Data.Map as Map

type Variables = Map.Map String Declaration

module Variables.Assignment
( getNotAssignable
, getUninitialized
--, Uninitialized(..)
) where

-- Bartlomiej Krolikowski

import Variables.Data
import Grammar.Data
import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Data.List as List

type Iterators = Set.Set String
--type Uninitialized = Set.Set String

-- sprawdza czy nie ma przypisan do iteratora
-- jesli sa to zwraca informacje dla uzytkownika o rodzaju bledu
getNotAssignable :: Commands -> Either String ()
getNotAssignable = cmdsGetNotAssignable Set.empty

-- jak wyzej
cmdsGetNotAssignable :: Iterators -> Commands -> Either String ()
cmdsGetNotAssignable iters cmds = List.foldl' (\ errAcc cmd -> do errAcc; cmdGetNotAssignable iters cmd) (Right ()) cmds
-- do ... -- Either String to monada -- (do a; b) jest rownowazne (a >>= (\_ -> b)) -- definicja (>>=):
--                                                                                         Left l >>= f = Left l
--                                                                                         Right r >>= f = f r

-- jak wyzej
cmdGetNotAssignable :: Iterators -> Command -> Either String ()
cmdGetNotAssignable iters (Assign ident _) = if Set.notMember (idName ident) iters
                                               then Right ()
                                               else Left ("cannot modify '" ++ (idName ident) ++ "' by ASSIGN because it is an iterator")
cmdGetNotAssignable iters (Read ident) = if Set.notMember (idName ident) iters
                                           then Right ()
                                           else Left ("cannot modify '" ++ (idName ident) ++ "' by READ because it is an iterator")
cmdGetNotAssignable iters (IfElse _ cmdsT cmdsF) = do cmdsGetNotAssignable iters cmdsT
                                                      cmdsGetNotAssignable iters cmdsF
cmdGetNotAssignable iters (If _ cmdsT) = cmdsGetNotAssignable iters cmdsT
cmdGetNotAssignable iters (While _ cmds) = cmdsGetNotAssignable iters cmds
cmdGetNotAssignable iters (Repeat cmds _) = cmdsGetNotAssignable iters cmds
cmdGetNotAssignable iters (ForTo iter _ _ cmds) = cmdsGetNotAssignable (Set.insert (iterName iter) iters) cmds
cmdGetNotAssignable iters (ForDownTo iter _ _ cmds) = cmdsGetNotAssignable (Set.insert (iterName iter) iters) cmds
cmdGetNotAssignable _ _ = Right ()

-- inicjalizacja

-- usuwa wszystkie zmienne zapisywane w cmds z vars
deleteVars :: Commands -> Variables -> Variables
deleteVars cmds vars = List.foldl' cmdDeleteVar vars cmds
    where cmdDeleteVar varAcc (Assign ident _) = Map.delete (idName ident) varAcc
          cmdDeleteVar varAcc (IfElse _ cmdsT cmdsF) = Map.intersection (deleteVars cmdsT varAcc) (deleteVars cmdsF varAcc)
          cmdDeleteVar varAcc (If _ cmdsT) = deleteVars cmdsT varAcc
          cmdDeleteVar varAcc (While _ cmds) = deleteVars cmds varAcc
          cmdDeleteVar varAcc (Repeat cmds _) = deleteVars cmds varAcc
          cmdDeleteVar varAcc (ForTo _ _ _ cmds) = deleteVars cmds varAcc
          cmdDeleteVar varAcc (ForDownTo _ _ _ cmds) = deleteVars cmds varAcc
          cmdDeleteVar varAcc (Read ident) = Map.delete (idName ident) varAcc
          cmdDeleteVar varAcc (Write _) = varAcc

-- sprawdza czy kazda zmienna zostala zainicjowana przed uzyciem
-- jesli nie to zwraca informacje dla uzytkownika o rodzaju bledu
getUninitialized :: Variables -> Commands -> Either String Variables
getUninitialized vars cmds = cmdsGetUninit cmds vars

-- nie przewiduje czy dany warunek jest spelniony, wiec dopuszczam niezainicjowane zmienne w if i petlach

-- szukam zmiennych niezainicjowanych przed uzyciem i zapisuje informacje o bledzie jesli znajde
cmdsGetUninit :: Commands -> Variables -> Either String Variables
cmdsGetUninit cmds vars = List.foldl' (\ errAcc cmd -> errAcc >>= cmdGetUninit cmd) (Right vars) cmds

-- jak wyzej
cmdGetUninit :: Command -> Variables -> Either String Variables
cmdGetUninit (Assign ident expr) vars = do idGetUninit ident vars
                                           exGetUninit expr vars
                                           return $ Map.delete (idName ident) vars
cmdGetUninit (IfElse cond cmdsT cmdsF) vars = do condGetUninit cond vars -- nie sprawdzam wewnatrz if
                                                 return $ deleteVars cmdsT $ deleteVars cmdsF vars
cmdGetUninit (If cond cmdsT) vars = do condGetUninit cond vars -- nie sprawdzam wewnatrz if
                                       return $ deleteVars cmdsT vars
cmdGetUninit (While cond cmds) vars = do condGetUninit cond vars -- nie sprawdzam wewnatrz while
                                         return $ deleteVars cmds vars
cmdGetUninit (Repeat cmds cond) vars = condGetUninit cond $ deleteVars cmds vars -- nie sprawdzam wewnatrz repeat
cmdGetUninit (ForTo _ from to cmds) vars = do valGetUninit from vars
                                              valGetUninit to vars
                                              return $ deleteVars cmds vars -- nie sprawdzam wewnatrz for
cmdGetUninit (ForDownTo _ from to cmds) vars = do valGetUninit from vars
                                                  valGetUninit to vars
                                                  return $ deleteVars cmds vars -- nie sprawdzam wewnatrz for
cmdGetUninit (Read ident) vars = do idGetUninit ident vars
                                    return $ Map.delete (idName ident) vars
cmdGetUninit (Write val) vars = valGetUninit val vars

-- jak wyzej
exGetUninit :: Expression -> Variables -> Either String Variables
exGetUninit (Single val) vars = valGetUninit val vars
exGetUninit (Plus valL valR) vars = do valGetUninit valL vars; valGetUninit valR vars
exGetUninit (Minus valL valR) vars = do valGetUninit valL vars; valGetUninit valR vars
exGetUninit (Times valL valR) vars = do valGetUninit valL vars; valGetUninit valR vars
exGetUninit (Div valL valR) vars = do valGetUninit valL vars; valGetUninit valR vars
exGetUninit (Mod valL valR) vars = do valGetUninit valL vars; valGetUninit valR vars

-- jak wyzej
condGetUninit :: Condition -> Variables -> Either String Variables
condGetUninit (Eq valL valR) vars = do valGetUninit valL vars; valGetUninit valR vars
condGetUninit (NEq valL valR) vars = do valGetUninit valL vars; valGetUninit valR vars
condGetUninit (Le valL valR) vars = do valGetUninit valL vars; valGetUninit valR vars
condGetUninit (Ge valL valR) vars = do valGetUninit valL vars; valGetUninit valR vars
condGetUninit (LEq valL valR) vars = do valGetUninit valL vars; valGetUninit valR vars
condGetUninit (GEq valL valR) vars = do valGetUninit valL vars; valGetUninit valR vars

-- jak wyzej
valGetUninit :: Value -> Variables -> Either String Variables
valGetUninit (Number _) vars = Right vars
valGetUninit (Identifier (Var name)) vars = singVarGetUninit name vars
valGetUninit (Identifier (ArrNum name _)) vars = arrGetUninit name vars
valGetUninit (Identifier (ArrVar name indName)) vars = do arrGetUninit name vars
                                                          singVarGetUninit indName vars

-- jak wyzej, ale na potreby zapisywania
idGetUninit :: Identifier -> Variables -> Either String Variables
idGetUninit (ArrVar _ indName) vars = singVarGetUninit indName vars
idGetUninit _ vars = Right vars

singVarGetUninit :: String -> Variables -> Either String Variables
singVarGetUninit name vars = if Map.member name vars
                               then Left ("reading variable '" ++ name ++ "' before it was assigned")
                               else Right vars

arrGetUninit :: String -> Variables -> Either String Variables
arrGetUninit name vars = if Map.member name vars
                           then Left ("reading from array '" ++ name ++ "' before any of its elements was assigned")
                           else Right vars

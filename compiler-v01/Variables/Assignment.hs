module Variables.Assignment
( getNotAssignable
, getUninitialized
--, Uninitialized(..)
) where

import Variables.Data
import Grammar.Data
import qualified Data.Set as Set
import qualified Data.List as List

type Iterators = Set.Set String
type Uninitialized = Set.Set Variables

-- zwraca Just _ jesli ktorykolwiek argument jest Just _
getJust :: Maybe a -> Maybe a -> Maybe a
getJust (Just a) _ = Just a
getJust Nothing b = b

-- sprawdza czy nie ma przypisan do iteratora
-- jesli sa to zwraca informacje dla uzytkownika o rodzaju bledu
getNotAssignable :: Commands -> Either String ()
getNotAssignable = cmdsGetNotAssignable Set.empty

-- jak wyzej
cmdsGetNotAssignable :: Iterators -> Commands -> Either String ()
cmdsGetNotAssignable iters cmds = List.foldl' (\ errAcc cmd -> do errAcc; cmdGetNotAssignable iters cmd) Nothing cmds
-- do ... -- Either String to monada -- (do a; b) jest rownowazne (a >>= (\_ -> b)) -- definicja (>>=):
--                                                                                         Left l >>= f = Left l
--                                                                                         Right r >>= f = f r

-- jak wyzej
cmdGetNotAssignable :: Iterators -> Command -> Either String ()
cmdGetNotAssignable iters (Assign ident _) = if Set.notMember (name ident) iters
                                               then Right ()
                                               else Left ("cannot modify '" ++ (name ident) ++ "' by ASSIGN because it is an iterator")
cmdGetNotAssignable iters (Read ident) = if Set.notMember (name ident) iters
                                           then Right ()
                                           else Left ("cannot modify '" ++ (name ident) ++ "' by READ because it is an iterator")
cmdGetNotAssignable _ _ = Right ()

-- inicjalizacja

-- informacja o tym, czy jestem w if wewnatrz jakiejkolwiek petli
-- (wtedy inicjalizacja moze byc zapisana ponizej, a wykonac sie wczesniej, zaleznie od warunku)
data Context = Global | Loop | IfInLoop deriving (Eq,Show)




























-- sprawdza czy kazda czytana zmienna zostala zainicjowana
-- jesli nie to zwraca informacje dla uzytkownika o rodzaju bledu
-- jesli tak to zwraca zbior niezainicjowanych (niewykorzystanych) zmiennych
-- (pobiera zbior niezainicjowanych zmiennych)
getUninitialized :: Variables -> Commands -> Either String Variables
getUninitialized vars cmds = List.foldl' (\ errAcc cmd -> errAcc >>= cmdGetUninitialized Global cmd) vars cmds

-- jak wyzej
cmdsGetUninitialized :: Context -> Command -> Variables -> Either String Variables
cmdsGetUninitialized ctx cmds vars = getUninitialized ctx vars cmds

-- jak wyzej
cmdGetUninitialized :: Context -> Command -> Variables -> Either String Variables
cmdGetUninitialized ctx (Assign ident expr) vars = do idGetUninitialized ctx ident vars
                                                      exGetUninitialized ctx expr vars
                                                      return $ Map.delete (idName ident) vars -- ident jest juz zainicjowany
cmdGetUninitialized ctx (IfElse cond cmdsT cmdsF) vars = do condGetUninitialized cond vars
                                                        tVars <- getUninitialized cmdsT vars
                                                        fVars <- getUninitialized cmdsF vars
                                                        return $ Map.intersection tVars fVars
                                                        -- uznaje ze zmienna jest zainicjowana jesli jest zainicjowana w ktorymkolwiek przypadku
cmdGetUninitialized ctx (If cond cmdsT) vars = do condGetUninitialized cond vars; getUninitialized cmdsT vars -- po if napewno nie ma mniej zainicjowanych
cmdGetUninitialized ctx (While cond cmds) vars = do condGetUninitialized cond vars
                                                getUninitialized cmds vars
                                                if vars










-- TODO ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------










cmdGetUninitialized ctx (Repeat cmds cond) vars = do getUninitialized vars cmds; condGetUninitialized vars cond
cmdGetUninitialized ctx (ForTo iter from to cmds) vars = let itName = iterName iter
                                                       forVars = Map.insert itName (SingleVar itName) vars
                                                   in  do itGetUninitialized vars iter
                                                          valGetUninitialized vars from
                                                          valGetUninitialized vars to
                                                          getUninitialized forVars cmds
cmdGetUninitialized ctx (ForDownTo iter from to cmds) vars = let itName = iterName iter
                                                           forVars = Map.insert itName (SingleVar itName) vars
                                                       in  do itGetUninitialized vars iter
                                                              valGetUninitialized vars from
                                                              valGetUninitialized vars to
                                                              getUninitialized forVars cmds
cmdGetUninitialized ctx (Read ident) vars = idGetUninitialized vars ident
cmdGetUninitialized ctx (Write val) vars = valGetUninitialized vars val

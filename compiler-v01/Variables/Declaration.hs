module Variables.Declaration
( getVariables
, getMisusedVars
) where

import Variables.Data
import Grammar.Data
import qualified Data.Map as Map
import qualified Data.List as List

-- sprawdza czy nie ma podwojnych deklaracji i czy zakresy w tablicach sa poprawne:
--   - jesli nie to zwraca zadeklarowane zmienne globalne
--   - jesli tak to zwraca informacje o bledzie dla uzytkownika
-- ((>>=) dla Either nic nie zmienia dla konstruktora Left _)
getVariables :: Declarations -> Either String Variables
getVariables decls = List.foldl' (\ varAcc decl -> varAcc >>= checkAndInsert decl) (Right Map.empty) decls
    where checkAndInsert decl@(SingleVar n) vars = checkRedeclaraiton decl vars
          checkAndInsert decl@(Array n b e) vars = if b <= e
                                                     then checkRedeclaraiton decl vars
                                                     else Left ("arrays of non-positive length are not allowed: " ++ n ++ "[ " ++ show b ++ " : " ++ show e ++ " ]")
          checkRedeclaraiton decl vars = let v = name decl
                                         in  if Map.notMember v vars
                                               then Right (Map.insert v decl vars)
                                               else Left ("redeclaration of  a variable '" ++ v ++ "'")

-- sprawdza czy kazda uzyta zmienna zostala zadeklarowana
-- jesli nie ta zwraca informacje dla uzytkownika o rodzaju bledu
getMisusedVars :: Variables -> Commands -> Either String ()
getMisusedVars vars cmds = List.foldl' (\ errAcc cmd -> do errAcc; cmdGetMisusedVars vars cmd) (Right ()) cmds
-- do ... -- Either String to monada -- (do a; b) jest rownowazne (a >>= (\_ -> b)) -- definicja (>>=):
--                                                                                         Left l >>= f = Left l
--                                                                                         Right r >>= f = f r

-- jak wyzej
itGetMisusedVars :: Variables -> Iterator -> Either String ()
itGetMisusedVars vars (Iterator name) = if Map.notMember name vars
                                          then Right ()
                                          else Left ("redeclaration of  a variable '" ++ name ++ "' in for")

-- jak wyzej
cmdGetMisusedVars :: Variables -> Command -> Either String ()
cmdGetMisusedVars vars (Assign ident expr) = do idGetMisusedVars vars ident; exGetMisusedVars vars expr
cmdGetMisusedVars vars (IfElse cond cmdsT cmdsF) = do condGetMisusedVars vars cond
                                                      getMisusedVars vars cmdsT
                                                      getMisusedVars vars cmdsF
cmdGetMisusedVars vars (If cond cmdsT) = do condGetMisusedVars vars cond; getMisusedVars vars cmdsT
cmdGetMisusedVars vars (While cond cmds) = do condGetMisusedVars vars cond; getMisusedVars vars cmds
cmdGetMisusedVars vars (Repeat cmds cond) = do getMisusedVars vars cmds; condGetMisusedVars vars cond
cmdGetMisusedVars vars (ForTo iter from to cmds) = let itName = iterName iter
                                                       forVars = Map.insert itName (SingleVar itName) vars
                                                   in  do itGetMisusedVars vars iter
                                                          valGetMisusedVars vars from
                                                          valGetMisusedVars vars to
                                                          getMisusedVars forVars cmds
cmdGetMisusedVars vars (ForDownTo iter from to cmds) = let itName = iterName iter
                                                           forVars = Map.insert itName (SingleVar itName) vars
                                                       in  do itGetMisusedVars vars iter
                                                              valGetMisusedVars vars from
                                                              valGetMisusedVars vars to
                                                              getMisusedVars forVars cmds
cmdGetMisusedVars vars (Read ident) = idGetMisusedVars vars ident
cmdGetMisusedVars vars (Write val) = valGetMisusedVars vars val

-- jak wyzej
exGetMisusedVars :: Variables -> Expression -> Either String ()
exGetMisusedVars vars (Single val) = valGetMisusedVars vars val
exGetMisusedVars vars (Plus valL valR) = do valGetMisusedVars vars valL; valGetMisusedVars vars valR
exGetMisusedVars vars (Minus valL valR) = do valGetMisusedVars vars valL; valGetMisusedVars vars valR
exGetMisusedVars vars (Times valL valR) = do valGetMisusedVars vars valL; valGetMisusedVars vars valR
exGetMisusedVars vars (Div valL valR) = do valGetMisusedVars vars valL; valGetMisusedVars vars valR
exGetMisusedVars vars (Mod valL valR) = do valGetMisusedVars vars valL; valGetMisusedVars vars valR

-- jak wyzej
condGetMisusedVars :: Variables -> Condition -> Either String ()
condGetMisusedVars vars (Eq valL valR) = do valGetMisusedVars vars valL; valGetMisusedVars vars valR
condGetMisusedVars vars (NEq valL valR) = do valGetMisusedVars vars valL; valGetMisusedVars vars valR
condGetMisusedVars vars (Le valL valR) = do valGetMisusedVars vars valL; valGetMisusedVars vars valR
condGetMisusedVars vars (Ge valL valR) = do valGetMisusedVars vars valL; valGetMisusedVars vars valR
condGetMisusedVars vars (LEq valL valR) = do valGetMisusedVars vars valL; valGetMisusedVars vars valR
condGetMisusedVars vars (GEq valL valR) = do valGetMisusedVars vars valL; valGetMisusedVars vars valR

-- jak wyzej
valGetMisusedVars :: Variables -> Value -> Either String ()
valGetMisusedVars _ (Number _) = Right ()
valGetMisusedVars vars (Identifier ident) = idGetMisusedVars vars ident

-- jak wyzej
idGetMisusedVars :: Variables -> Identifier -> Either String ()
idGetMisusedVars = checkMisused
    where checkMisused vars (Var name) = checkVar vars name
          checkMisused vars (ArrNum name _) = checkArr vars name
          checkMisused vars (ArrVar name indName) = do checkArr vars name; checkVar vars indName
          checkVar vars name = case Map.lookup name vars of
                                   Just (SingleVar _) -> Right ()
                                   Just (Array _ _ _) -> Left ("identifier '" ++ name ++ "' is an array so it should be indexed")
                                   Nothing -> Left ("undeclared identifier: " ++ name)
          checkArr vars name = case Map.lookup name vars of
                                   Just (Array _ _ _) -> Right ()
                                   Just (SingleVar _) -> Left ("identifier '" ++ name ++ "' is not an array so it should not be indexed")
                                   Nothing -> Left ("undeclared array identifier: " ++ name)

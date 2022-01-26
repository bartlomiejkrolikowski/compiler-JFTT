module Grammar
( parse
) where

import Tokens
import GrammarTree
import Instructions
import qualified Data.Map as Map
import qualified Data.Set as Set

let vars = $2; (cmds,newVars,_) = ($4 (fst vars) (snd vars) False False) in Program vars (cmds,newVars)
let vars = (Map.empty, 0); (cmds,newVars,_) = ($2 (fst vars) (snd vars) False False) in Program vars (cmds,newVars)

let (decls, lowestUnusedAddress) = $1
in  if Map.member $3 decls
      then error ("redeclaration of a variable: " ++ $3)
      else (Map.insert $3 (SingleVar $3 False lowestUnusedAddress) decls, lowestUnusedAddress+1)
let (decls, lowestUnusedAddress) = $1
    ; index0Address = lowestUnusedAddress - $5
    ; arrayLength = $7 - $5 + 1
    in  if $5 > $7
          then error ("arrays of non-positive length are not allowed: " ++ $3 ++ "[" ++ show $5 ++ ":" ++ show $7 ++ "]")
          else if Map.member $3 decls
                 then error ("redeclaration of a variable: " ++ $3)
                 else (Map.insert $3 (Array $3 $5 $7 False index0Address) decls, lowestUnusedAddress + arrayLength)

(Map.singleton $1 (SingleVar $1 False 0), 1)
let index0Address = - $3
    ; arrayLength = $5 - $3 + 1
    in  if $3 > $5
          then error ("arrays of non-positive length are not allowed: " ++ $1 ++ "[" ++ show $3 ++ ":" ++ show $5 ++ "]")
          else (Map.singleton $1 (Array $1 $3 $5 False index0Address), arrayLength)
\vars unusedAddr inLoop inIfinLoop ->
     let (cmds, prevVars, prevNas) = $1 vars unusedAddr inLoop inIfinLoop
         (cmd, newVars, newNas) = $2 prevVars unusedAddr inLoop inIfinLoop
     in  (cmds ++ [cmd], newVars, Set.union prevNas newNas)
\vars unusedAddr inLoop inIfinLoop ->
     let (cmd, newVars, newNas) = $1 vars unusedAddr inLoop inIfinLoop
     in  ([cmd], newVars, newNas)

\vars unusedAddr inLoop inIfinLoop ->
     let successfulResult assignId = let (expr, nas) = $3 vars inLoop inIfinLoop
                                     in  (Assign assignId expr, Map.adjust setAssigned (name . decl \$ assignId) vars, nas)
     in  case $1 vars of
             Var (Iterator c _ _)    -> error ("cannot modify '" ++ c ++ "' by ASSIGN because it is an iterator")
             assignId@(ArrVar _ ind) -> let succAId = successfulResult assignId
                                        in  if assigned ind
                                              then succAId
                                              else if inIfinLoop -- jesli jestem w if, w petli to zmienna moze byc zainicjowana w innym warunku w poprzednim przebiegu
                                                     then Set.insert (name ind) <\$> succAId
                                                     else error ("reading variable '" ++ name ind ++ "' before it was assigned")
             assignId                -> successfulResult assignId
\vars unusedAddr inLoop inIfinLoop ->
     let isInIfinLoop = inLoop || inIfinLoop -- albo juz jestem wewnatrz if w petli, albo tylko w petli i wchodze do if
         (cmdsT, varsT, nasT) = $4 vars unusedAddr inLoop isInIfinLoop -- po przejsciu przez if zmienna jest zainicjowana
         (cmdsF, varsF, nasF) = $6 vars unusedAddr inLoop isInIfinLoop --    gdy jest inicjowana w co najmniej jednym z przebiegow
         (cond, nasC) = $2 vars inLoop inIfinLoop -- warunek jest liczony na poczatku if - w zewnetrznym kontekscie
     in  (IfElse cond cmdsT cmdsF, Map.unionWith chooseAssigned varsT varsF, Set.union nasC \$ Set.union nasT nasF)
\vars unusedAddr inLoop inIfinLoop ->
     let isInIfinLoop = inLoop || inIfinLoop -- albo juz jestem wewnatrz if w petli, albo tylko w petli i wchodze do if
         (cmdsT, varsT, nasT) = $4 vars unusedAddr inLoop isInIfinLoop -- po przejsciu przez taki if napewno nie bedzie mniej zainicjowanych
         (cond, nasC) = $2 vars inLoop inIfinLoop -- warunek jest liczony na poczatku if - w zewnetrznym kontekscie
     in  (If cond cmdsT, varsT, Set.union nasC nasT)
\vars unusedAddr inLoop inIfinLoop ->
     if inIfinLoop
       then let (cond, nasC) = $2 vars inLoop inIfinLoop -- analogicznie jak dla pozostalych (nie moge byc bardziej w petli)
                (cmdsW, varsW, nasW) = $4 vars unusedAddr True inIfinLoop -- po przejsciu przez while napewno nie bedzie mniej zainicjowanych
            in  (While cond cmdsW, varsW, Set.union nasC nasW)
       else let (cond, nasC) = $2 vars inLoop inIfinLoop -- warunek jest jeszcze poza petla
                (cmdsW, varsW, nasW) = $4 vars unusedAddr True inIfinLoop -- po przejsciu przez while napewno nie bedzie mniej zainicjowanych
                stillNotAssigned = assertAssignment varsW nasW -- sprawdzam czy wszystkie czytane w petli, w if-ach zmienne zostaly zainicjowane przed koncem petli (jesli nie to blad)
            in  (While cond cmdsW, varsW, Set.union nasC stillNotAssigned)
\vars unusedAddr inLoop inIfinLoop ->
     if inIfinLoop
       then let (cmdsR, varsR, nasR) = $2 vars unusedAddr True inIfinLoop -- po przejsciu przez repeat napewno nie bedzie mniej zainicjowanych + w warunku sa juz zainicjowane
                (cond, nasC) = $4 varsR inLoop inIfinLoop -- analogicznie jak dla pozostalych (nie moge byc bardziej w petli)
            in  (Repeat cmdsR cond, varsR, Set.union nasR nasC)
       else let (cmdsR, varsR, nasR) = $2 vars unusedAddr True inIfinLoop -- po przejsciu przez repeat napewno nie bedzie mniej zainicjowanych + w warunku sa juz zainicjowane
                cond, nasC) = $4 varsR inLoop inIfinLoop -- warunek jest juz poza petla
                stillNotAssigned = assertAssignment varsR nasR -- sprawdzam czy wszystkie czytane w petli, w if-ach zmienne zostaly zainicjowane przed koncem petli (jesli nie to blad)
            in  (Repeat cmdsR cond, varsR, Set.union nasC stillNotAssigned)
\vars unusedAddr inLoop inIfinLoop ->
     case Map.lookup $2 vars of
          Nothing ->
              if inIfinLoop
                then let (valFrom, nasFrom) = $4 vars inLoop inIfinLoop
                         (valTo, nasTo) = $6 vars inLoop inIfinLoop
                         localVars = Map.insert $2 (Iterator $2 True unusedAddr) vars
                         (cmdsF, varsF, nasF) = $8 localVars (unusedAddr+2) True inIfinLoop -- +2 bo pamietam jeszcze koniec;ponizej usuwam iterator
                     in  (ForTo (Iterator $2 True unusedAddr) valFrom valTo cmdsF, Map.delete $2 varsF, Set.union nasFrom \$ Set.union nasTo nasF)
                else let (valFrom, nasFrom) = $4 vars inLoop inIfinLoop -- konce zakresow sa obliczane jeszcze przed petla
                         (valTo, nasTo) = $6 vars inLoop inIfinLoop
                         localVars = Map.insert $2 (Iterator $2 True unusedAddr) vars
                         (cmdsF, varsF, nasF) = $8 localVars (unusedAddr+2) True inIfinLoop -- +2 bo pamietam jeszcze koniec;ponizej usuwam iterator
                         stillNotAssigned = assertAssignment varsF nasF -- sprawdzam czy wszystkie czytane w petli, w if-ach zmienne zostaly zainicjowane przed koncem petli (jesli nie to blad)
                     in  (ForTo (Iterator $2 True unusedAddr) valFrom valTo cmdsF, Map.delete $2 varsF, Set.union nasFrom \$ Set.union nasTo stillNotAssigned)
          Just _  -> error ($2 ++ " is already declared")
\vars unusedAddr inLoop inIfinLoop ->
     case Map.lookup $2 vars of
         Nothing ->
             if inIfinLoop
               then let (valFrom, nasFrom) = $4 vars inLoop inIfinLoop
                        (valTo, nasTo) = $6 vars inLoop inIfinLoop
                        localVars = Map.insert $2 (Iterator $2 True unusedAddr) vars
                        (cmdsF, varsF, nasF) = $8 localVars (unusedAddr+2) True inIfinLoop -- +2 bo pamietam jeszcze koniec;ponizej usuwam iterator
                    in  (ForDownTo (Iterator $2 True unusedAddr) valFrom valTo cmdsF, Map.delete $2 varsF, Set.union nasFrom \$ Set.union nasTo nasF)
               else let (valFrom, nasFrom) = $4 vars inLoop inIfinLoop -- konce zakresow sa obliczane jeszcze przed petla
                        (valTo, nasTo) = $6 vars inLoop inIfinLoop
                        localVars = Map.insert $2 (Iterator $2 True unusedAddr) vars
                        (cmdsF, varsF, nasF) = $8 localVars (unusedAddr+2) True inIfinLoop -- +2 bo pamietam jeszcze koniec;ponizej usuwam iterator
                        stillNotAssigned = assertAssignment varsF nasF -- sprawdzam czy wszystkie czytane w petli, w if-ach zmienne zostaly zainicjowane przed koncem petli (jesli nie to blad)
                    in  (ForDownTo (Iterator $2 True unusedAddr) valFrom valTo cmdsF, Map.delete $2 varsF, Set.union nasFrom \$ Set.union nasTo stillNotAssigned)
         Just _  -> error ($2 ++ " is already declared")
\vars unusedAddr inLoop inIfinLoop ->
     let successfulResult readId = (Read readId, Map.adjust setAssigned (name . decl \$ readId) vars, Set.empty)
     in  case $2 vars of
             Var (Iterator c _ _)  -> error ("cannot modify '" ++ c ++ "' by READ because it is an iterator")
             readId@(ArrVar _ ind) -> if assigned ind
                                        then successfulResult readId
                                        else if inIfinLoop -- jesli jestem w if, w petli to zmienna moze byc zainicjowana w innym warunku w poprzednim przebiegu
                                               then Set.insert (name ind) <\$> successfulResult readId
                                               else error ("reading variable '" ++ name ind ++ "' before it was assigned")
             readId                -> successfulResult readId
\vars _ inLoop inIfinLoop -> let (val, nas) = ($2 vars inLoop inIfinLoop) in (Write val, vars, nas)

\vars inLoop inIfinLoop  -> let (val, nas) = ($1 vars inLoop inIfinLoop) in (Single val, nas)
\vars inLoop inIfinLoop ->
     let (valL, nasL) = $1 vars inLoop inIfinLoop
         (valR, nasR) = $3 vars inLoop inIfinLoop
     in  (Plus valL valR, Set.union nasL nasR)
\vars inLoop inIfinLoop ->
     let (valL, nasL) = $1 vars inLoop inIfinLoop
         (valR, nasR) = $3 vars inLoop inIfinLoop
     in  (Minus valL valR, Set.union nasL nasR)
\vars inLoop inIfinLoop ->
     let (valL, nasL) = $1 vars inLoop inIfinLoop
         (valR, nasR) = $3 vars inLoop inIfinLoop
     in  (Times valL valR, Set.union nasL nasR)
\vars inLoop inIfinLoop ->
     let (valL, nasL) = $1 vars inLoop inIfinLoop
         (valR, nasR) = $3 vars inLoop inIfinLoop
     in  (Div valL valR, Set.union nasL nasR)
\vars inLoop inIfinLoop ->
     let (valL, nasL) = $1 vars inLoop inIfinLoop
         (valR, nasR) = $3 vars inLoop inIfinLoop
     in  (Mod valL valR, Set.union nasL nasR)

\vars inLoop inIfinLoop ->
     let (valL, nasL) = $1 vars inLoop inIfinLoop
         (valR, nasR) = $3 vars inLoop inIfinLoop
     in  (Eq valL valR, Set.union nasL nasR)
\vars inLoop inIfinLoop ->
     let (valL, nasL) = $1 vars inLoop inIfinLoop
         (valR, nasR) = $3 vars inLoop inIfinLoop
     in  (NEq valL valR, Set.union nasL nasR)
\vars inLoop inIfinLoop ->
     let (valL, nasL) = $1 vars inLoop inIfinLoop
         (valR, nasR) = $3 vars inLoop inIfinLoop
     in  (Le valL valR, Set.union nasL nasR)
\vars inLoop inIfinLoop ->
     let (valL, nasL) = $1 vars inLoop inIfinLoop
         (valR, nasR) = $3 vars inLoop inIfinLoop
     in  (Ge valL valR, Set.union nasL nasR)
\vars inLoop inIfinLoop ->
     let (valL, nasL) = $1 vars inLoop inIfinLoop
         (valR, nasR) = $3 vars inLoop inIfinLoop
     in  (LEq valL valR, Set.union nasL nasR)
\vars inLoop inIfinLoop ->
     let (valL, nasL) = $1 vars inLoop inIfinLoop
         (valR, nasR) = $3 vars inLoop inIfinLoop
     in  (GEq valL valR, Set.union nasL nasR)

\vars _ _ -> (Number $1, Set.empty)
\vars inLoop inIfinLoop ->
     case ($1 vars) of
         var@(Var _) -> if assigned \$ decl var
                          then (Identifier var, Set.empty)
                          else if inIfinLoop
                                 then (Identifier var, Set.singleton \$ name \$ decl var)
                                 else error ("reading variable '" ++ (name \$ decl var) ++ "' before it was assigned")
         var@(ArrNum _ _) -> if assigned \$ decl var
                               then (Identifier var, Set.empty)
                               else if inIfinLoop
                                      then (Identifier var, Set.singleton \$ name \$ decl var)
                                      else error ("reading from array '" ++ (name \$ decl var) ++ "' before any of its elements was assigned")
         var@(ArrVar _ _) -> if assigned \$ decl var
                               then if assigned \$ indexDecl var
                                      then (Identifier var, Set.empty)
                                      else if inIfinLoop
                                             then (Identifier var, Set.singleton \$ name \$ indexDecl var)
                                             else error ("reading variable '" ++ (name \$ indexDecl var) ++ "' before it was assigned")
                               else if inIfinLoop
                                      then (Identifier var, Set.singleton \$ name \$ decl var)
                                      else error ("reading from array '" ++ (name \$ decl var) ++ "' before any of its elements was assigned")

\vars ->
     case Map.lookup $1 vars of
         Nothing -> error ("undeclared identifier: " ++ $1)
         Just (Array _ _ _ _ _) -> error ("missing index for accessing an array: " ++ $1)
         Just decl -> Var decl
\vars ->
     case Map.lookup $1 vars of
         Nothing -> error ("undeclared array identifier: " ++ $1)
         Just arrDecl@(Array _ _ _ _ _) -> case Map.lookup $3 vars of
                                               Nothing -> error ("undeclared identifier: " ++ $3)
                                               Just (Array _ _ _ _ _) -> error ("identifier '" ++ $3 ++ "' may not be an array")
                                               Just indDecl -> ArrVar arrDecl indDecl
         Just _  -> error ("identifier '" ++ $1 ++ "' should be an array")
\vars ->
     case Map.lookup $1 vars of
         Nothing -> error ("undeclared array identifier: " ++ $1)
         Just arrDecl@(Array _ _ _ _ _) -> ArrNum arrDecl $3
         Just _  -> error ("identifier '" ++ $1 ++ "' should be an array")

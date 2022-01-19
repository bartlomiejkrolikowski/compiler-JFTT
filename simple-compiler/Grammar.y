{
module Grammar
( parse
) where

import Tokens
import GrammarTree
import Instructions
import qualified Data.Map as Map
}

%name parse
%tokentype { Token }
%error { parseError }

%token
    num           { Num $$ }
    pidentifier   { Pidentifier $$ }
    VAR           { VarK }
    BEGIN         { BeginK }
    END           { EndK }
    ASSIGN        { AssignK }
    IF            { IfK }
    THEN          { ThenK }
    ELSE          { ElseK }
    ENDIF         { EndIfK }
    WHILE         { WhileK }
    DO            { DoK }
    ENDWHILE      { EndWhileK }
    REPEAT        { RepeatK }
    UNTIL         { UntilK }
    FOR           { ForK }
    FROM          { FromK }
    TO            { ToK }
    DOWNTO        { DownToK }
    ENDFOR        { EndForK }
    READ          { ReadK }
    WRITE         { WriteK }
    PLUS          { PlusK }
    MINUS         { MinusK }
    TIMES         { TimesK }
    DIV           { DivK }
    MOD           { ModK }
    EQ            { EqK }
    NEQ           { NEqK }
    LE            { LeK }
    GE            { GeK }
    LEQ           { LEqK }
    GEQ           { GEqK }
    ','           { Comma }
    '['           { LBrac }
    ':'           { Colon }
    ']'           { RBrac }
    ';'           { Semicolon }

%%

program :: { Program }
      : VAR declarations BEGIN commands END { let vars = $2 in Program vars ($4 (fst vars) (snd vars)) }
      | BEGIN commands END                  { let vars = (Map.empty, 0) in Program vars ($2 (fst vars) (snd vars)) }

declarations :: { (Map.Map String Variable, Int) }
      : declarations ',' pidentifier                     { let (decls, lowestUnusedAddress) = $1
                                                           in  if Map.member $3 decls
                                                                 then error ("redeclaration of a variable: " ++ $3)
                                                                 else (Map.insert $3 (SingleVar $3 False lowestUnusedAddress) decls, lowestUnusedAddress+1) }
      | declarations ',' pidentifier '[' num ':' num ']' { let (decls, lowestUnusedAddress) = $1
                                                               ; index0Address = lowestUnusedAddress - $5
                                                               ; arrayLength = $7 - $5 + 1
                                                           in  if $5 > $7
                                                                 then error ("arrays of non-positive length are not allowed: " ++ $3 ++ "[" ++ show $5 ++ ":" ++ show $7 ++ "]")
                                                                 else if Map.member $3 decls
                                                                        then error ("redeclaration of a variable: " ++ $3)
                                                                        else (Map.insert $3 (Array $3 $5 $7 False index0Address) decls, lowestUnusedAddress + arrayLength) }
      | pidentifier                                      { (Map.singleton $1 (SingleVar $1 False 0), 1) }
      | pidentifier '[' num ':' num ']'                  { let index0Address = - $3
                                                               ; arrayLength = $5 - $3 + 1
                                                           in  if $3 > $5
                                                                 then error ("arrays of non-positive length are not allowed: " ++ $1 ++ "[" ++ show $3 ++ ":" ++ show $5 ++ "]")
                                                                 else (Map.singleton $1 (Array $1 $3 $5 False index0Address), arrayLength) }

commands :: { Variables -> Int -> ([Command], Variables) }
      : commands command { \vars unusedAddr ->
                               let (cmds, prevVars) = $1 vars unusedAddr
                                   (cmd, newVars) = $2 prevVars unusedAddr
                               in  (cmds ++ [cmd], newVars) }
      | command          { \vars unusedAddr ->
                               let (cmd, newVars) = $1 vars unusedAddr
                               in  ([cmd], newVars) }

command :: { Variables -> Int -> (Command, Variables) }
      : identifier ASSIGN expression ';'                           { \vars unusedAddr ->
                                                                         let successfulResult assignId = (Assign assignId ($3 vars), Map.adjust setAssigned (name . decl \$ assignId) vars)
                                                                         in  case $1 vars of
                                                                                 Var (Iterator c _ _)    -> error ("cannot modify '" ++ c ++ "' by ASSIGN because it is an iterator")
                                                                                 assignId@(ArrVar _ ind) -> if assigned ind
                                                                                                              then successfulResult assignId
                                                                                                              else error ("reading variable '" ++ name ind ++ "' before it was assigned")
                                                                                 assignId                -> successfulResult assignId }
      | IF condition THEN commands ELSE commands ENDIF             { \vars unusedAddr ->
                                                                         let (cmdsT, varsT) = $4 vars unusedAddr -- po przejsciu przez if zmienna jest zainicjowana
                                                                             (cmdsF, varsF) = $6 vars unusedAddr --    gdy jest inicjowana w co najmniej jednym z przebiegow
                                                                         in  (IfElse ($2 vars) cmdsT cmdsF, Map.unionWith chooseAssigned varsT varsF) }
      | IF condition THEN commands ENDIF                           { \vars unusedAddr ->
                                                                         let (cmdsT, varsT) = $4 vars unusedAddr -- po przejsciu przez taki if napewno nie bedzie mniej zainicjowanych
                                                                         in  (If ($2 vars) cmdsT, varsT) }
      | WHILE condition DO commands ENDWHILE                       { \vars unusedAddr ->
                                                                         let (cmdsW, varsW) = $4 vars unusedAddr -- po przejsciu przez while napewno nie bedzie mniej zainicjowanych
                                                                         in  (While ($2 vars) cmdsW, varsW) }
      | REPEAT commands UNTIL condition ';'                        { \vars unusedAddr ->
                                                                         let (cmdsR, varsR) = $2 vars unusedAddr -- po przejsciu przez repeat napewno nie bedzie mniej zainicjowanych
                                                                         in  (Repeat cmdsR ($4 vars), varsR) }
      | FOR pidentifier FROM value TO value DO commands ENDFOR     { \vars unusedAddr ->
                                                                         case Map.lookup $2 vars of
                                                                             Nothing -> let localVars = Map.insert $2 (Iterator $2 True unusedAddr) vars
                                                                                            (cmdsF, varsF) = $8 localVars (unusedAddr+1) -- vvv ponizej usuwam iterator vvv
                                                                                        in  (ForTo (Iterator $2 True unusedAddr) ($4 vars) ($6 vars) cmdsF, Map.delete $2 varsF)
                                                                             Just _  -> error ($2 ++ " is already declared") }
      | FOR pidentifier FROM value DOWNTO value DO commands ENDFOR { \vars unusedAddr ->
                                                                         case Map.lookup $2 vars of
                                                                             Nothing -> let localVars = Map.insert $2 (Iterator $2 True unusedAddr) vars
                                                                                            (cmdsF, varsF) = $8 localVars (unusedAddr+1) -- vvv ponizej usuwam iterator vvv
                                                                                        in  (ForDownTo (Iterator $2 True unusedAddr) ($4 vars) ($6 vars) cmdsF, Map.delete $2 varsF)
                                                                             Just _  -> error ($2 ++ " is already declared") }
      | READ identifier ';'                                        { \vars unusedAddr ->
                                                                         let successfulResult readId = (Read readId, Map.adjust setAssigned (name . decl \$ readId) vars)
                                                                         in  case $2 vars of
                                                                                 Var (Iterator c _ _)  -> error ("cannot modify '" ++ c ++ "' by READ because it is an iterator")
                                                                                 readId@(ArrVar _ ind) -> if assigned ind
                                                                                                            then successfulResult readId
                                                                                                            else error ("reading variable '" ++ name ind ++ "' before it was assigned")
                                                                                 readId                -> successfulResult readId }
      | WRITE value ';'                                            { \vars _ -> (Write ($2 vars), vars) }

expression :: { Variables -> Expression }
      : value             { \vars -> Single ($1 vars) }
      | value PLUS value  { \vars -> Plus ($1 vars) ($3 vars) }
      | value MINUS value { \vars -> Minus ($1 vars) ($3 vars) }
      | value TIMES value { \vars -> Times ($1 vars) ($3 vars) }
      | value DIV value   { \vars -> Div ($1 vars) ($3 vars) }
      | value MOD value   { \vars -> Mod ($1 vars) ($3 vars) }

condition :: { Variables -> Condition }
      : value EQ value  { \vars -> Eq ($1 vars) ($3 vars) }
      | value NEQ value { \vars -> NEq ($1 vars) ($3 vars) }
      | value LE value  { \vars -> Le ($1 vars) ($3 vars) }
      | value GE value  { \vars -> Ge ($1 vars) ($3 vars) }
      | value LEQ value { \vars -> LEq ($1 vars) ($3 vars) }
      | value GEQ value { \vars -> GEq ($1 vars) ($3 vars) }

value :: { Variables -> Value }
      : num        { \vars -> Number $1 }
      | identifier { \vars ->
                         case ($1 vars) of
                             var@(Var _) -> if assigned \$ decl var
                                              then Identifier var
                                              else error ("reading variable '" ++ (name \$ decl var) ++ "' before it was assigned")
                             var@(ArrNum _ _) -> if assigned \$ decl var
                                                   then Identifier var
                                                   else error ("reading from array '" ++ (name \$ decl var) ++ "' before any of its elements was assigned")
                             var@(ArrVar _ _) -> if assigned \$ decl var
                                                   then if assigned \$ indexDecl var
                                                          then Identifier var
                                                          else error ("reading variable '" ++ (name \$ indexDecl var) ++ "' before it was assigned")
                                                   else error ("reading from array '" ++ (name \$ decl var) ++ "' before any of its elements was assigned") }

identifier :: { Variables -> Identifier }
      : pidentifier                     { \vars ->
                                              case Map.lookup $1 vars of
                                                  Nothing -> error ("undeclared identifier: " ++ $1)
                                                  Just (Array _ _ _ _ _) -> error ("missing index for accessing an array: " ++ $1)
                                                  Just decl -> Var decl }
      | pidentifier '[' pidentifier ']' { \vars ->
                                              case Map.lookup $1 vars of
                                                  Nothing -> error ("undeclared array identifier: " ++ $1)
                                                  Just arrDecl@(Array _ _ _ _ _) -> case Map.lookup $3 vars of
                                                                                        Nothing -> error ("undeclared identifier: " ++ $3)
                                                                                        Just (Array _ _ _ _ _) -> error ("identifier '" ++ $3 ++ "' may not be an array")
                                                                                        Just indDecl -> ArrVar arrDecl indDecl
                                                  Just _  -> error ("identifier '" ++ $1 ++ "' should be an array") }
      | pidentifier '[' num ']'         { \vars ->
                                              case Map.lookup $1 vars of
                                                  Nothing -> error ("undeclared array identifier: " ++ $1)
                                                  Just arrDecl@(Array _ _ _ _ _) -> ArrNum arrDecl $3
                                                  Just _  -> error ("identifier '" ++ $1 ++ "' should be an array") }

{
parseError :: [Token] -> a
parseError _ = error "Parse error"
}

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
      : VAR declarations BEGIN commands END { let vars = $2 in Program vars (fst ($4 vars)) }
      | BEGIN commands END                  { let vars = Map.empty in Program vars (fst ($2 vars)) }

declarations :: { (Map.Map String Variable, Int) }
      : declarations ',' pidentifier                     { let (decls, lowestUnusedAddress) = $1
                                                           in  if Map.member $3 decls
                                                                 then error ("redeclaration of a variable: " ++ $3)
                                                                 else (Map.insert $3 (SingleVar $3 False lowestUnusedAddress) decls, lowestUnusedAddress+1) }
      | declarations ',' pidentifier '[' num ':' num ']' { let (decls, lowestUnusedAddress) = $1
                                                               index0Address = lowestUnusedAddress - $5
                                                               arrayLength = $7 - $5 + 1
                                                           in  if $5 > $7
                                                                 then error ("arrays of non-positive length are not allowed: " ++ $3 ++ "[" ++ show $5 ++ ":" ++ show $7 ++ "]")
                                                                 else if Map.member $3 decls
                                                                        then error ("redeclaration of a variable: " ++ $3)
                                                                        else (Map.insert $3 (Array $3 $5 $7 False index0Address) decls, lowestUnusedAddress + arrayLength) }
      | pidentifier                                      { (Map.singleton $1 (SingleVar $1 False 0), 1) }
      | pidentifier '[' num ':' num ']'                  { let index0Address = - $5
                                                               arrayLength = $7 - $5 + 1
                                                           in  if $3 > $5
                                                                 then error ("arrays of non-positive length are not allowed: " ++ $1 ++ "[" ++ show $3 ++ ":" ++ show $5 ++ "]")
                                                                 else (Map.singleton $1 (Array $1 $3 $5 False index0Address), arrayLength) }

commands :: { Variables -> ([Command], Variables) }
      : commands command { \vars -> let (cmds, prevVars) = $1 vars
                                        (cmd, newVars) = $2 prevVars
                                    in  (cmds ++ [cmd], newVars) }
      | command          { \vars -> let (cmd, newVars) = $1 vars
                                    in  ([cmd], newVars) }

command :: { Variables -> (Command, Variables) }
      : identifier ASSIGN expression ';'                           { \vars -> case ($1 vars) of
                                                                                   Var (Constant c) -> error ("cannot modify '" ++ c ++ "' by ASSIGN because it is an iterator")
                                                                                   assignId         -> (Assign assignId ($3 vars), Map.adjust assign (name . decl $ assignId) vars) }
      | IF condition THEN commands ELSE commands ENDIF             { \vars -> IfElse ($2 vars) ($4 vars) ($6 vars) } -- TODO-------------------------------------------------------------------
      | IF condition THEN commands ENDIF                           { \vars -> If ($2 vars) ($4 vars) }
      | WHILE condition DO commands ENDWHILE                       { \vars -> While ($2 vars) ($4 vars) }
      | REPEAT commands UNTIL condition ';'                        { \vars -> Repeat ($2 vars) ($4 vars) }
      | FOR pidentifier FROM value TO value DO commands ENDFOR     { \vars -> case Map.lookup $2 vars of
                                                                                  Nothing -> let localVars = Map.insert $2 (Constant $2) vars
                                                                                             in  ForTo $2 ($4 vars) ($6 vars) ($8 localVars)
                                                                                  Just _  -> error ($2 ++ " is already declared") }
      | FOR pidentifier FROM value DOWNTO value DO commands ENDFOR { \vars -> case Map.lookup $2 vars of
                                                                                  Nothing -> let localVars = Map.insert $2 (Constant $2) vars
                                                                                             in  ForDownTo $2 ($4 vars) ($6 vars) ($8 localVars)
                                                                                  Just _  -> error ($2 ++ " is already declared") }
      | READ identifier ';'                                        { \vars -> case $2 vars of
                                                                                   Var (Constant c) -> error ("cannot modify '" ++ c ++ "' by READ because it is an iterator")
                                                                                   readId           -> (Read readId, Map.adjust assign (name . decl $ readId) vars) }
      | WRITE value ';'                                            { \vars -> Write ($2 vars) }

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
      | identifier { \vars -> Identifier ($1 vars) }

identifier :: { Variables -> Identifier }
      : pidentifier                     { \vars -> case Map.lookup $1 vars of
                                                       Nothing -> error ("undeclared identifier: " ++ $1)
                                                       Just (Array _ _ _) -> error ("missing index for accessing an array: " ++ $1)
                                                       Just decl -> Var decl }
      | pidentifier '[' pidentifier ']' { \vars -> case Map.lookup $1 vars of
                                                       Nothing -> error ("undeclared array identifier: " ++ $1)
                                                       Just arrDecl@(Array _ _ _) -> case Map.lookup $3 vars of
                                                                                         Nothing -> error ("undeclared identifier: " ++ $3)
                                                                                         Just (Array _ _ _) -> error ("identifier '" ++ $3 ++ "' may not be an array")
                                                                                         Just indDecl -> ArrVar arrDecl indDecl
                                                       Just _  -> error ("identifier '" ++ $1 ++ "' should be an array") }
      | pidentifier '[' num ']'         { \vars -> case Map.lookup $1 vars of
                                                       Nothing -> error ("undeclared array identifier: " ++ $1)
                                                       Just arrDecl@(Array _ _ _) -> ArrNum arrDecl $3
                                                       Just _  -> error ("identifier '" ++ $1 ++ "' should be an array") }

{
parseError :: [Token] -> a
parseError _ = error "Parse error"
}

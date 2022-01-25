{
module Grammar.Parse
( parse
, Program(..)
, Declarations(..)
, Declaration(..)
, Commands(..)
, Command(..)
, Expression(..)
, Condition(..)
, Value(..)
, Identifier(..)
) where

import Tokens
import Grammar.Data
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
      : VAR declarations BEGIN commands END { Program (reverse $2) (reverse $4) }
      | BEGIN commands END                  { Program [] (reverse $2) }

declarations :: { Declarations }
      : declarations ',' pidentifier                     { SingleVar { name = $3 } : $1 }
      | declarations ',' pidentifier '[' num ':' num ']' { Array { name = $3, begin = $5, end = $7 } : $1 }
      | pidentifier                                      { [ SingleVar { name = $1 } ] }
      | pidentifier '[' num ':' num ']'                  { [ Array { name = $1, begin = $3, end = $5 } ] }

commands :: { Commands }
      : commands command { $2 : $1 }
      | command          { [$1] }

command :: { Command }
      : identifier ASSIGN expression ';'                           { Assign $1 $3 }
      | IF condition THEN commands ELSE commands ENDIF             { IfElse $2 (reverse $4) (reverse $6) }
      | IF condition THEN commands ENDIF                           { If $2 (reverse $4) }
      | WHILE condition DO commands ENDWHILE                       { While $2 (reverse $4) }
      | REPEAT commands UNTIL condition ';'                        { Repeat (reverse $2) $4 }
      | FOR pidentifier FROM value TO value DO commands ENDFOR     { ForTo { iterFT = Iterator $2, fromT = $4, toT = $6, commandsT = (reverse $8) } }
      | FOR pidentifier FROM value DOWNTO value DO commands ENDFOR { ForDownTo { iterFD = Iterator $2, fromD = $4, toD = $6, commandsD = (reverse $8) } }
      | READ identifier ';'                                        { Read $2 }
      | WRITE value ';'                                            { Write $2 }

expression :: { Expression }
      : value             { Single $1 }
      | value PLUS value  { Plus $1 $3 }
      | value MINUS value { Minus $1 $3 }
      | value TIMES value { Times $1 $3 }
      | value DIV value   { Div $1 $3 }
      | value MOD value   { Mod $1 $3 }

condition :: { Condition }
      : value EQ value  { Eq $1 $3 }
      | value NEQ value { NEq $1 $3 }
      | value LE value  { Le $1 $3 }
      | value GE value  { Ge $1 $3 }
      | value LEQ value { LEq $1 $3 }
      | value GEQ value { GEq $1 $3 }

value :: { Value }
      : num        { Number $1 }
      | identifier { Identifier $1 }

identifier :: { Identifier }
      : pidentifier                     { Var { name = $1 } }
      | pidentifier '[' pidentifier ']' { ArrVar { name = $1, indexVar = $3 } }
      | pidentifier '[' num ']'         { ArrNum { name = $1, indexNum = $3 } }

{
parseError :: [Token] -> a
parseError _ = error "Syntax error"
}

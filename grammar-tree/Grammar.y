{
module Grammar
( parse
) where

import Tokens
import GrammarTree

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
      : VAR declarations BEGIN commands END { Program (Just $2) $4 }
      | BEGIN commands END                  { Program Nothing $2 }

declarations :: { [Declaration] }
      : declarations ',' pidentifier                     { $1 ++ [Variable $3] }
      | declarations ',' pidentifier '[' num ':' num ']' { $1 ++ [Array $3 $5 $7] }
      | pidentifier                                      { [Variable $1] }
      | pidentifier '[' num ':' num ']'                  { [Array $1 $3 $5] }

commands :: { [Command] }
      : commands command { $1 ++ [$2] }
      | command          { [$1] }

command :: { Command }
      : identifier ASSIGN expression ';'                           { Assign $1 $3 }
      | IF condition THEN commands ELSE commands ENDIF             { IfElse $2 $4 $6 }
      | IF condition THEN commands ENDIF                           { If $2 $4 }
      | WHILE condition DO commands ENDWHILE                       { While $2 $4 }
      | REPEAT commands UNTIL condition ';'                        { Repeat $2 $4 }
      | FOR pidentifier FROM value TO value DO commands ENDFOR     { ForTo $2 $4 $6 $8 }
      | FOR pidentifier FROM value DOWNTO value DO commands ENDFOR { ForDownTo $2 $4 $6 $8 }
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
      : pidentifier                     { Var $1 }
      | pidentifier '[' pidentifier ']' { ArrVar $1 $3 }
      | pidentifier '[' num ']'         { ArrNum $1 $3 }

{
parseError :: [Token] -> a
parseError _ = error "Parse error"
}

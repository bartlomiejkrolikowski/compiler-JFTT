{
module Grammar
( Program(..)
, parse
) where

import Tokens

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

%left PLUS MINUS
%left TIMES DIV MOD

%%

program
      : VAR declarations BEGIN commands END { Program }
      | BEGIN commands END                  { Program }

declarations
      : declarations ',' pidentifier                     { () }
      | declarations ',' pidentifier '[' num ':' num ']' { () }
      | pidentifier                                      { () }
      | pidentifier '[' num ':' num ']'                  { () }

commands
      : commands command { () }
      | command          { () }

command
      : identifier ASSIGN expression ';'                           { () }
      | IF condition THEN commands ELSE commands ENDIF             { () }
      | IF condition THEN commands ENDIF                           { () }
      | WHILE condition DO commands ENDWHILE                       { () }
      | REPEAT commands UNTIL condition ';'                        { () }
      | FOR pidentifier FROM value TO value DO commands ENDFOR     { () }
      | FOR pidentifier FROM value DOWNTO value DO commands ENDFOR { () }
      | READ identifier ';'                                        { () }
      | WRITE value ';'                                            { () }

expression
      : value             { () }
      | value PLUS value  { () }
      | value MINUS value { () }
      | value TIMES value { () }
      | value DIV value   { () }
      | value MOD value   { () }

condition
      : value EQ value  { () }
      | value NEQ value { () }
      | value LE value  { () }
      | value GE value  { () }
      | value LEQ value { () }
      | value GEQ value { () }

value : num        { () }
      | identifier { () }

identifier
      : pidentifier                     { () }
      | pidentifier '[' pidentifier ']' { () }
      | pidentifier '[' num ']'         { () }

{

data Program = Program deriving(Eq,Show)

parseError :: [Token] -> a
parseError _ = error "Parse error"
}

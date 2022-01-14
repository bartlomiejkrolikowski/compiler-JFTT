{
module Tokens
( Token(..)
, alexScanTokens
) where
}

%wrapper "basic"

@num = [0-9]+
@pidentifier = [_a-z]+
@comment = "("([^\)]|\n)*")"

tokens :-

   @num		{ \s -> Num (read s) }
   @pidentifier	{ \s -> Pidentifier s }

   "VAR"	{ \s -> VarK }
   "BEGIN"	{ \s -> BeginK }
   "END"	{ \s -> EndK }

   "ASSIGN"	{ \s -> AssignK }

   "IF"		{ \s -> IfK }
   "THEN"	{ \s -> ThenK }
   "ELSE"	{ \s -> ElseK }
   "ENDIF"	{ \s -> EndIfK }

   "WHILE"	{ \s -> WhileK }
   "DO"		{ \s -> DoK }
   "ENDWHILE"	{ \s -> EndWhileK }

   "REPEAT"	{ \s -> RepeatK }
   "UNTIL"	{ \s -> UntilK }

   "FOR"	{ \s -> ForK }
   "FROM"	{ \s -> FromK }
   "TO"		{ \s -> ToK }
   "DOWNTO"	{ \s -> DownToK }
   "ENDFOR"	{ \s -> EndForK }

   "READ"	{ \s -> ReadK }
   "WRITE"	{ \s -> WriteK }

   "PLUS"	{ \s -> PlusK }
   "MINUS"	{ \s -> MinusK }
   "TIMES"	{ \s -> TimesK }
   "DIV"	{ \s -> DivK }
   "MOD"	{ \s -> ModK }

   "EQ"		{ \s -> EqK }
   "NEQ"	{ \s -> NEqK }
   "LE"		{ \s -> LeK }
   "GE"		{ \s -> GeK }
   "LEQ"	{ \s -> LEqK }
   "GEQ"	{ \s -> GEqK }

   ","		{ \s -> Comma }
   "["		{ \s -> LBrac }
   ":"		{ \s -> Colon }
   "]"		{ \s -> RBrac }
   ";"		{ \s -> Semicolon }

   @comment	;
   $white+	;

{
data Token =
    Num Int |
    Pidentifier String |
    VarK | BeginK | EndK |
    AssignK |
    IfK | ThenK | ElseK | EndIfK |
    WhileK | DoK | EndWhileK |
    RepeatK | UntilK |
    ForK | FromK | ToK | DownToK | EndForK |
    ReadK | WriteK |
    PlusK | MinusK | TimesK | DivK | ModK |
    EqK | NEqK | LeK | GeK | LEqK | GEqK |
    Comma | LBrac | Colon | RBrac | Semicolon
    deriving (Show, Eq)
}

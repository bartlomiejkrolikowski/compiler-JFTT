module Translation.Translation
( translateProgram
) where

import qualified Data.List as List
import Grammar.Data
import Translation.Instructions

-- tlumaczenie drzewa wyprowadzenia (z atrybutami) na ciag instrukcji maszyny wirtualnej
translateProgram :: Program -> (Code, Int)
translateProgram (Program _ commands) = let (cmds, len) = translateCommands commands
                                            in  (reverse (stopExecution ++ cmds), 1 + len)
    where stopExecution = [ Halt ] -- zatrzymuje program

-- tlumaczenie ciagu instrukcji: polaczenie wynikow dla kolejnych instrukcji (wynik jest zapisany od tylu)
translateCommands :: Commands -> (Code, Int)
translateCommands cmds = List.foldl' (\ acc cmd -> joinCmds (translateCommand cmd) acc) ([],0) cmds

-- tlumaczenie pojedynczej instrukcji
translateCommand :: Command -> (Code, Int)
translateCommand (Assign ident expr) = assign ident (translateExpression expr)
translateCommand (IfElse cond cmdsT cmdsF) = let (cmdsCond, condType) = translateCondition cond
                                             in  ifElse condType cmdsCond (translateCommands cmdsT) (translateCommands cmdsF)
translateCommand (If cond cmdsT) = let (cmdsCond, condType) = translateCondition cond
                                   in  onlyIf condType cmdsCond (translateCommands cmdsT)
translateCommand (While cond cmds) = let (cmdsCond, condType) = translateCondition cond
                                     in  whileLoop condType cmdsCond (translateCommands cmds)
translateCommand (Repeat cmds cond) = let (cmdsCond, condType) = translateCondition cond
                                      in  repeatLoop condType cmdsCond (translateCommands cmds)
translateCommand (ForTo (Iterator iter) from to cmds) =
    forToLoop (Var iter) (translateValue from) (translateValue to) (translateCommands cmds) -- na potrzeby funkcji traktuje iter jak zmienna po lewej stronie przypisania
translateCommand (ForDownTo (Iterator iter) from to cmds) =
    forDownToLoop (Var iter) (translateValue from) (translateValue to) (translateCommands cmds) -- na potrzeby funkcji traktuje iter jak zmienna po lewej stronie przypisania
translateCommand (Read ident) = readAndStore ident
translateCommand (Write val) = writeVal (translateValue val)

-- tlumaczenie obliczania wyrazenia
translateExpression :: Expression -> (Code, Int)
translateExpression (Single val) = translateValue val
translateExpression (Plus valL valR) = plusExpr valL valR
translateExpression (Minus valL valR) = minusExpr valL valR
translateExpression (Times valL valR) = timesExpr valL valR
translateExpression (Div valL valR) = divExpr valL valR
translateExpression (Mod valL valR) = modExpr valL valR

-- tlumaczenie obliczania warunku
translateCondition :: Condition -> ((Code, Int), CondType)
translateCondition (Eq valL valR) = eqCond valL valR
translateCondition (NEq valL valR) = neqCond valL valR
translateCondition (Le valL valR) = leCond valL valR
translateCondition (Ge valL valR) = geCond valL valR
translateCondition (LEq valL valR) = leqCond valL valR
translateCondition (GEq valL valR) = geqCond valL valR

-- tlumaczenie pobierania wartosci
translateValue :: Value -> (Code, Int)
translateValue val = appendLength $ loadVal val

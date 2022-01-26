module Variables.Correctness
( checkCorrectness
) where

import Grammar.Data
import Variables.Data
import Variables.Assignment
import Variables.Declaration

checkCorrectness :: Program -> Either String ()
checkCorrectness (Program decls cmds) = do
                                          vars <- getVariables decls
                                          getMisusedVars vars cmds
                                          getNotAssignable cmds
                                          getUninitialized vars cmds
                                          return ()

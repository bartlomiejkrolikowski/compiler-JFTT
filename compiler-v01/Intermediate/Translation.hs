module Intermediate.Translation
( getInterProg
) where

import Grammar.Data
import Intermediate.Data
import qualified Data.Map as Map
import Data.Map((!)) -- Map.lookup, ale zwraca a zamiast Maybe a (error jesli nie ma klucza)
import qualified Data.List as List

-- adresy zmiennych i minimalny dostepny adres
type Locations = (Map.Map String Int, Int)

-- kierunek for
data Direction = Up | Down deriving (Eq,Show)

-- przydziela pamiec dla iteratora (nizszy adres) i gornego konca zakresu (wyzszy adres)
insertIterator :: String -> Locations -> Locations
insertIterator name (addrs, minAddr) = (Map.insert name minAddr addrs, minAddr + 2)

-- podaje adres konca zakresu zwiazanego z podanym iteratorem
rangeAddr :: String -> Locations -> Int
rangeAddr name (addrs, _) = (addrs ! name) + 1

-- podaje zaprzeczenie warunku
condNot :: Condition -> Condition
condNot (Eq valL valR) = NEq valL valR
condNot (NEq valL valR) = Eq valL valR
condNot (Le valL valR) = Ge valL valR
condNot (Ge valL valR) = Le valL valR
condNot (LEq valL valR) = GEq valL valR
condNot (GEq valL valR) = LEq valL valR

-- zwraca kod posredni dla instrukcji z programu
getInterProg :: Program -> [InterInstr]
getInterProg (Program decls cmds) = getInterCmds locations cmds
    where locations = List.foldl' setLocation (Map.empty, 0) decls
          setLocation (addrs, minAddr) (SingleVar n) = (Map.insert n minAddr addrs, minAddr + 1)
          setLocation (addrs, minAddr) (Array n b e) = (Map.insert n (minAddr - b) addrs, minAddr - b + e + 1)

-- tlumaczy komendy na kod posredni
getInterCmds :: Locations -> Commands -> [InterInstr]
getInterCmds _ [] = []
getInterCmds locs (Assign ident expr : cmds) = interAssign locs ident expr ++ getInterCmds locs cmds
getInterCmds locs (IfElse cond cmdsT cmdsF : cmds) = interIf locs cond cmdsT cmdsF ++ getInterCmds locs cmds
getInterCmds locs (If cond cmdsT : cmds) = interIf locs cond cmdsT [] ++ getInterCmds locs cmds
getInterCmds locs (While cond cmdsL : cmds) = interLoop locs cond cmdsL cmds
getInterCmds locs (Repeat cmdsL cond : cmds) = getInterCmds locs cmdsL ++ interLoop locs (condNot cond) cmdsL cmds
getInterCmds locs (ForTo (Iterator itName) from to cmdsL : cmds) = interFor locs Up itName from to cmdsL cmds
getInterCmds locs (ForDownTo (Iterator itName) from to cmdsL : cmds) = interFor locs Down itName from to cmdsL cmds
getInterCmds locs (Read ident : cmds) = interRead locs ident ++ getInterCmds locs cmds
getInterCmds locs (Write val : cmds) = interWrite locs val ++ getInterCmds locs cmds

-- tlumaczy assign na kod posredni
interAssign :: Locations -> Identifier -> Expression -> [InterInstr]
interAssign locs (Var name) (Single (Number n)) = [ IStoreVC { memDst = fst locs ! name, num = n } ]
interAssign locs (Var name) (Single (Identifier (Var nameDst))) = [ IStoreVV { memDst = fst locs ! name, memSrc = fst locs ! nameDst } ]
interAssign locs (Var name) (Single (Identifier (ArrNum nameDst indexN))) = [ IStoreVAc { memDst = fst locs ! name, memSrc = fst locs ! nameDst, cIndexSrc = indexN } ]
interAssign locs (Var name) (Single (Identifier (ArrVar nameDst indexV))) = [ IStoreVA { memDst = fst locs ! name, memSrc = fst locs ! nameDst, indexSrc = fst locs ! indexV } ]
interAssign locs (Var name) expr = [ IStoreVE { memDst = fst locs ! name, exprSrc = getInterExpr locs expr } ]
interAssign locs (ArrNum name indN) (Single (Number n)) = [ IStoreAcC { memDst = fst locs ! name, cIndexDst = indN, num = n } ]
interAssign locs (ArrNum name indN) (Single (Identifier (Var nameDst))) = [ IStoreAcV { memDst = fst locs ! name, cIndexDst = indN, memSrc = fst locs ! nameDst } ]
interAssign locs (ArrNum name indN) (Single (Identifier (ArrNum nameDst indexN))) = [ IStoreAcAc { memDst = fst locs ! name, cIndexDst = indN, memSrc = fst locs ! nameDst, cIndexSrc = indexN } ]
interAssign locs (ArrNum name indN) (Single (Identifier (ArrVar nameDst indexV))) = [ IStoreAcA { memDst = fst locs ! name, cIndexDst = indN, memSrc = fst locs ! nameDst, indexSrc = fst locs ! indexV } ]
interAssign locs (ArrNum name indN) expr = [ IStoreAcE { memDst = fst locs ! name, cIndexDst = indN, exprSrc = getInterExpr locs expr } ]
interAssign locs (ArrVar name indV) (Single (Number n)) = [ IStoreAC { memDst = fst locs ! name, indexDst = fst locs ! indV, num = n } ]
interAssign locs (ArrVar name indV) (Single (Identifier (Var nameDst))) = [ IStoreAV { memDst = fst locs ! name, indexDst = fst locs ! indV, memSrc = fst locs ! nameDst } ]
interAssign locs (ArrVar name indV) (Single (Identifier (ArrNum nameDst indexN))) = [ IStoreAAc { memDst = fst locs ! name, indexDst = fst locs ! indV, memSrc = fst locs ! nameDst, cIndexSrc = indexN } ]
interAssign locs (ArrVar name indV) (Single (Identifier (ArrVar nameDst indexV))) =
    [ IStoreAA { memDst = fst locs ! name, indexDst = fst locs ! indV, memSrc = fst locs ! nameDst, indexSrc = fst locs ! indexV } ]
interAssign locs (ArrVar name indV) expr = [ IStoreAE { memDst = fst locs ! name, indexDst = fst locs ! indV, exprSrc = getInterExpr locs expr } ]

-- tlumaczy if-else na kod posredni
interIf :: Locations -> Condition -> Commands -> Commands -> [InterInstr]
interIf locs (Eq valL valR) cmdsT cmdsF = [ IBranchEq { instrCond = interSub locs valL valR, instrT = getInterCmds locs cmdsT, instrF = getInterCmds locs cmdsF } ]
interIf locs (NEq valL valR) cmdsT cmdsF = [ IBranchEq { instrCond = interSub locs valL valR, instrT = getInterCmds locs cmdsF, instrF = getInterCmds locs cmdsT } ] -- zamienione cmdsT i cmdsF
interIf locs (Le valL valR) cmdsT cmdsF = [ IBranchLe { instrCond = interSub locs valL valR, instrT = getInterCmds locs cmdsT, instrF = getInterCmds locs cmdsF } ]
interIf locs (Ge valL valR) cmdsT cmdsF = [ IBranchLe { instrCond = interSub locs valR valL, instrT = getInterCmds locs cmdsT, instrF = getInterCmds locs cmdsF } ] -- zamienione valL i valR
interIf locs (LEq valL valR) cmdsT cmdsF = [ IBranchLEq { instrCond = interSub locs valL valR , instrT = getInterCmds locs cmdsT, instrF = getInterCmds locs cmdsF } ]
interIf locs (GEq valL valR) cmdsT cmdsF = [ IBranchLEq { instrCond = interSub locs valR valL , instrT = getInterCmds locs cmdsT, instrF = getInterCmds locs cmdsF } ] -- zamienione valL i valR

-- tlumaczy petle na kod posredni
interLoop :: Locations -> Condition -> Commands -> Commands -> [InterInstr]
interLoop locs (Eq valL valR) cmdsT cmdsF = [ ILoopEq { instrCond = interSub locs valL valR, instrT = getInterCmds locs cmdsT, instrF = getInterCmds locs cmdsF } ]
interLoop locs (NEq valL valR) cmdsT cmdsF = [ ILoopNEq { instrCond = interSub locs valL valR, instrT = getInterCmds locs cmdsT, instrF = getInterCmds locs cmdsF } ]
interLoop locs (Le valL valR) cmdsT cmdsF = [ ILoopLe { instrCond = interSub locs valL valR, instrT = getInterCmds locs cmdsT, instrF = getInterCmds locs cmdsF } ]
interLoop locs (Ge valL valR) cmdsT cmdsF = [ ILoopLe { instrCond = interSub locs valR valL, instrT = getInterCmds locs cmdsT, instrF = getInterCmds locs cmdsF } ] -- zamienione valL i valR
interLoop locs (LEq valL valR) cmdsT cmdsF = [ ILoopLEq { instrCond = interSub locs valL valR, instrT = getInterCmds locs cmdsT, instrF = getInterCmds locs cmdsF } ]
interLoop locs (GEq valL valR) cmdsT cmdsF = [ ILoopLEq { instrCond = interSub locs valR valL, instrT = getInterCmds locs cmdsT, instrF = getInterCmds locs cmdsF } ] -- zamienione valL i valR

-- tlumaczy for na kod posredni (cmdsL - komendy w petli, cmds - komendy za petla)
interFor :: Locations -> Direction -> String -> Value -> Value -> Commands -> Commands -> [InterInstr]
interFor locs dir iter from to cmdsL cmds = let iterLocs = insertIterator iter locs
                                                itAddr = fst iterLocs ! iter
                                                toAddr = rangeAddr iter iterLocs
                                            in  assignFrom itAddr from ++ storeTo toAddr to ++ interLoopFor iterLocs itAddr toAddr cmdsL cmds
    where assignFrom itAddr (Number n) = [ IStoreVC { memDst = itAddr, num = n } ]
          assignFrom itAddr (Identifier (Var name)) = [ IStoreVV { memDst = itAddr, memSrc = fst locs ! name } ]
          assignFrom itAddr (Identifier (ArrNum name indN)) = [ IStoreVV { memDst = itAddr, memSrc = (fst locs ! name) + indN } ]
          assignFrom itAddr (Identifier (ArrVar name indV)) = [ IStoreVA { memDst = itAddr, memSrc = fst locs ! name, indexSrc = fst locs ! indV } ]
          storeTo toAddr (Number n) = [ IStoreVC { memDst = toAddr, num = n } ]
          storeTo toAddr (Identifier (Var name)) = [ IStoreVV { memDst = toAddr, memSrc = fst locs ! name } ]
          storeTo toAddr (Identifier (ArrNum name indN)) = [ IStoreVV { memDst = toAddr, memSrc = (fst locs ! name) + indN } ]
          storeTo toAddr (Identifier (ArrVar name indV)) = [ IStoreVA { memDst = toAddr, memSrc = fst locs ! name, indexSrc = fst locs ! indV } ]
          interLoopFor iterLocs itAddr toAddr cmdsL cmds = [ ILoopLEq { instrCond = interCmp itAddr toAddr, instrT = getInterCmds iterLocs cmdsL ++ iterIncOrDec itAddr, instrF = getInterCmds locs cmds } ]
          interCmp itAddr toAddr = if dir == Up
                                     then [ ISubVV { memL = itAddr, memR = toAddr } ] -- porownanie: czy iter <= koniec
                                     else [ ISubVV { memL = toAddr, memR = itAddr } ] -- porownanie: czy iter >= koniec
          iterIncOrDec itAddr = if dir == Up
                                  then [ IStoreVE { memDst = itAddr, exprSrc = [ IAddCV { numL = 1, memR = itAddr } ] } ] -- inkrementacja
                                  else [ IStoreVE { memDst = itAddr, exprSrc = [ ISubVC { memL = itAddr, numR = 1 } ] } ] -- dekrementacja

-- tlumaczy read na kod posredni
interRead :: Locations -> Identifier -> [InterInstr]
interRead locs (Var name) = [ IGetV { mem = fst locs ! name } ]
interRead locs (ArrNum name indN) = [ IGetAc { mem = fst locs ! name, cIndex = indN } ]
interRead locs (ArrVar name indV) = [ IGetA { mem = fst locs ! name, index = fst locs ! indV } ]

-- tlumaczy write na kod posredni
interWrite :: Locations -> Value -> [InterInstr]
interWrite locs (Number n) = [ IPutC { num = n } ]
interWrite locs (Identifier (Var name)) = [ IPutV { mem = fst locs ! name } ]
interWrite locs (Identifier (ArrNum name indN)) = [ IPutAc { mem = fst locs ! name, cIndex = indN } ]
interWrite locs (Identifier (ArrVar name indV)) = [ IPutA { mem = fst locs ! name, index = fst locs ! indV } ]

-- jak wyzej dla wyrazen
getInterExpr :: Locations -> Expression -> [InterInstr]
getInterExpr locs (Single _) = error "internal error: getInterExpr is valid only for binary expressions"
getInterExpr locs (Plus valL valR) = interAdd locs valL valR
getInterExpr locs (Minus valL valR) = interSub locs valL valR
getInterExpr locs (Times valL valR) = interMul locs valL valR
getInterExpr locs (Div valL valR) = interDiv locs valL valR
getInterExpr locs (Mod valL valR) = interMod locs valL valR

-- jak wyzej - dodawanie
interAdd :: Locations -> Value -> Value -> [InterInstr]
interAdd locs (Number l) (Number r) = [ IConst { num = l+r } ]
interAdd locs (Number l) (Identifier (Var nameR)) = [ IAddCV { numL = l, memR = fst locs ! nameR } ]
interAdd locs (Number l) (Identifier (ArrNum nameR indNR)) = [ IAddCAc { numL = l, memR = fst locs ! nameR, cIndexR = indNR } ]
interAdd locs (Number l) (Identifier (ArrVar nameR indVR)) = [ IAddCA { numL = l, memR = fst locs ! nameR, indexR = fst locs ! indVR } ]
interAdd locs idL@(Identifier _) nR@(Number _) = interAdd locs nR idL
interAdd locs (Identifier (Var nameL)) (Identifier (Var nameR)) = [ IAddVV { memL = fst locs ! nameL, memR = fst locs ! nameR } ]
interAdd locs (Identifier (Var nameL)) (Identifier (ArrNum nameR indNR)) = [ IAddVAc { memL = fst locs ! nameL, memR = fst locs ! nameR, cIndexR = indNR } ]
interAdd locs (Identifier (Var nameL)) (Identifier (ArrVar nameR indVR)) = [ IAddVA { memL = fst locs ! nameL, memR = fst locs ! nameR, indexR = fst locs ! indVR } ]
interAdd locs idL@(Identifier _) idR@(Identifier (Var _)) = interAdd locs idR idL
interAdd locs (Identifier (ArrNum nameL indNL)) (Identifier (ArrNum nameR indNR)) = [ IAddAcAc { memL = fst locs ! nameL, cIndexL = indNL, memR = fst locs ! nameR, cIndexR = indNR } ]
interAdd locs (Identifier (ArrNum nameL indNL)) (Identifier (ArrVar nameR indVR)) = [ IAddAcA { memL = fst locs ! nameL, cIndexL = indNL, memR = fst locs ! nameR, indexR = fst locs ! indVR } ]
interAdd locs idL@(Identifier _) idR@(Identifier (ArrNum _ _)) = interAdd locs idR idL
interAdd locs (Identifier (ArrVar nameL indVL)) (Identifier (ArrVar nameR indVR)) = [ IAddAA { memL = fst locs ! nameL, indexL = fst locs ! indVL, memR = fst locs ! nameR, indexR = fst locs ! indVR } ]

-- jak wyzej - odejmowanie
interSub :: Locations -> Value -> Value -> [InterInstr]
interSub locs (Number l) (Number r) = [ IConst { num = l-r } ]
interSub locs (Number l) (Identifier (Var nameR)) = [ ISubCV { numL = l, memR = fst locs ! nameR } ]
interSub locs (Number l) (Identifier (ArrNum nameR indNR)) = [ ISubCAc { numL = l, memR = fst locs ! nameR, cIndexR = indNR } ]
interSub locs (Number l) (Identifier (ArrVar nameR indVR)) = [ ISubCA { numL = l, memR = fst locs ! nameR, indexR = fst locs ! indVR } ]
interSub locs (Identifier (Var nameL)) (Number r) = [ ISubVC { memL = fst locs ! nameL, numR = r} ]
interSub locs (Identifier (Var nameL)) (Identifier (Var nameR)) = [ ISubVV { memL = fst locs ! nameL, memR = fst locs ! nameR } ]
interSub locs (Identifier (Var nameL)) (Identifier (ArrNum nameR indNR)) = [ ISubVAc { memL = fst locs ! nameL, memR = fst locs ! nameR, cIndexR = indNR } ]
interSub locs (Identifier (Var nameL)) (Identifier (ArrVar nameR indVR)) = [ ISubVA { memL = fst locs ! nameL, memR = fst locs ! nameR, indexR = fst locs ! indVR } ]
interSub locs (Identifier (ArrNum nameL indNL)) (Number r) = [ ISubAcC { memL = fst locs ! nameL, cIndexL = indNL, numR = r} ]
interSub locs (Identifier (ArrNum nameL indNL)) (Identifier (Var nameR)) = [ ISubAcV { memL = fst locs ! nameL, cIndexL = indNL, memR = fst locs ! nameR } ]
interSub locs (Identifier (ArrNum nameL indNL)) (Identifier (ArrNum nameR indNR)) = [ ISubAcAc { memL = fst locs ! nameL, cIndexL = indNL, memR = fst locs ! nameR, cIndexR = indNR } ]
interSub locs (Identifier (ArrNum nameL indNL)) (Identifier (ArrVar nameR indVR)) = [ ISubAcA { memL = fst locs ! nameL, cIndexL = indNL, memR = fst locs ! nameR, indexR = fst locs ! indVR } ]
interSub locs (Identifier (ArrVar nameL indVL)) (Number r) = [ ISubAC { memL = fst locs ! nameL, indexL = fst locs ! indVL, numR = r} ]
interSub locs (Identifier (ArrVar nameL indVL)) (Identifier (Var nameR)) = [ ISubAV { memL = fst locs ! nameL, indexL = fst locs ! indVL, memR = fst locs ! nameR } ]
interSub locs (Identifier (ArrVar nameL indVL)) (Identifier (ArrNum nameR indNR)) = [ ISubAAc { memL = fst locs ! nameL, indexL = fst locs ! indVL, memR = fst locs ! nameR, cIndexR = indNR } ]
interSub locs (Identifier (ArrVar nameL indVL)) (Identifier (ArrVar nameR indVR)) = [ ISubAA { memL = fst locs ! nameL, indexL = fst locs ! indVL, memR = fst locs ! nameR, indexR = fst locs ! indVR } ]

-- jak wyzej - mnozenie
interMul :: Locations -> Value -> Value -> [InterInstr]
interMul locs (Number l) (Number r) = [ IConst { num = l*r } ]
interMul locs (Number l) (Identifier (Var nameR)) = [ IMulCV { numL = l, memR = fst locs ! nameR } ]
interMul locs (Number l) (Identifier (ArrNum nameR indNR)) = [ IMulCAc { numL = l, memR = fst locs ! nameR, cIndexR = indNR } ]
interMul locs (Number l) (Identifier (ArrVar nameR indVR)) = [ IMulCA { numL = l, memR = fst locs ! nameR, indexR = fst locs ! indVR } ]
interMul locs idL@(Identifier _) nR@(Number _) = interMul locs nR idL
interMul locs (Identifier (Var nameL)) (Identifier (Var nameR)) = [ IMulVV { memL = fst locs ! nameL, memR = fst locs ! nameR } ]
interMul locs (Identifier (Var nameL)) (Identifier (ArrNum nameR indNR)) = [ IMulVAc { memL = fst locs ! nameL, memR = fst locs ! nameR, cIndexR = indNR } ]
interMul locs (Identifier (Var nameL)) (Identifier (ArrVar nameR indVR)) = [ IMulVA { memL = fst locs ! nameL, memR = fst locs ! nameR, indexR = fst locs ! indVR } ]
interMul locs idL@(Identifier _) idR@(Identifier (Var _)) = interMul locs idR idL
interMul locs (Identifier (ArrNum nameL indNL)) (Identifier (ArrNum nameR indNR)) = [ IMulAcAc { memL = fst locs ! nameL, cIndexL = indNL, memR = fst locs ! nameR, cIndexR = indNR } ]
interMul locs (Identifier (ArrNum nameL indNL)) (Identifier (ArrVar nameR indVR)) = [ IMulAcA { memL = fst locs ! nameL, cIndexL = indNL, memR = fst locs ! nameR, indexR = fst locs ! indVR } ]
interMul locs idL@(Identifier _) idR@(Identifier (ArrNum _ _)) = interMul locs idR idL
interMul locs (Identifier (ArrVar nameL indVL)) (Identifier (ArrVar nameR indVR)) = [ IMulAA { memL = fst locs ! nameL, indexL = fst locs ! indVL, memR = fst locs ! nameR, indexR = fst locs ! indVR } ]

-- jak wyzej - dzielenie
interDiv :: Locations -> Value -> Value -> [InterInstr]
interDiv locs (Number l) (Number r) = if r == 0
                                        then [ IConst { num = 0 } ]
                                        else [ IConst { num = l `div` r } ]
interDiv locs (Number l) (Identifier (Var nameR)) = [ IDivCV { numL = l, memR = fst locs ! nameR } ]
interDiv locs (Number l) (Identifier (ArrNum nameR indNR)) = [ IDivCAc { numL = l, memR = fst locs ! nameR, cIndexR = indNR } ]
interDiv locs (Number l) (Identifier (ArrVar nameR indVR)) = [ IDivCA { numL = l, memR = fst locs ! nameR, indexR = fst locs ! indVR } ]
interDiv locs (Identifier (Var nameL)) (Number r) = [ IDivVC { memL = fst locs ! nameL, numR = r} ]
interDiv locs (Identifier (Var nameL)) (Identifier (Var nameR)) = [ IDivVV { memL = fst locs ! nameL, memR = fst locs ! nameR } ]
interDiv locs (Identifier (Var nameL)) (Identifier (ArrNum nameR indNR)) = [ IDivVAc { memL = fst locs ! nameL, memR = fst locs ! nameR, cIndexR = indNR } ]
interDiv locs (Identifier (Var nameL)) (Identifier (ArrVar nameR indVR)) = [ IDivVA { memL = fst locs ! nameL, memR = fst locs ! nameR, indexR = fst locs ! indVR } ]
interDiv locs (Identifier (ArrNum nameL indNL)) (Number r) = [ IDivAcC { memL = fst locs ! nameL, cIndexL = indNL, numR = r} ]
interDiv locs (Identifier (ArrNum nameL indNL)) (Identifier (Var nameR)) = [ IDivAcV { memL = fst locs ! nameL, cIndexL = indNL, memR = fst locs ! nameR } ]
interDiv locs (Identifier (ArrNum nameL indNL)) (Identifier (ArrNum nameR indNR)) = [ IDivAcAc { memL = fst locs ! nameL, cIndexL = indNL, memR = fst locs ! nameR, cIndexR = indNR } ]
interDiv locs (Identifier (ArrNum nameL indNL)) (Identifier (ArrVar nameR indVR)) = [ IDivAcA { memL = fst locs ! nameL, cIndexL = indNL, memR = fst locs ! nameR, indexR = fst locs ! indVR } ]
interDiv locs (Identifier (ArrVar nameL indVL)) (Number r) = [ IDivAC { memL = fst locs ! nameL, indexL = fst locs ! indVL, numR = r} ]
interDiv locs (Identifier (ArrVar nameL indVL)) (Identifier (Var nameR)) = [ IDivAV { memL = fst locs ! nameL, indexL = fst locs ! indVL, memR = fst locs ! nameR } ]
interDiv locs (Identifier (ArrVar nameL indVL)) (Identifier (ArrNum nameR indNR)) = [ IDivAAc { memL = fst locs ! nameL, indexL = fst locs ! indVL, memR = fst locs ! nameR, cIndexR = indNR } ]
interDiv locs (Identifier (ArrVar nameL indVL)) (Identifier (ArrVar nameR indVR)) = [ IDivAA { memL = fst locs ! nameL, indexL = fst locs ! indVL, memR = fst locs ! nameR, indexR = fst locs ! indVR } ]

-- jak wyzej - modulo
interMod :: Locations -> Value -> Value -> [InterInstr]
interMod locs (Number l) (Number r) = if r == 0
                                        then [ IConst { num = 0 } ]
                                        else [ IConst { num = l `div` r } ]
interMod locs (Number l) (Identifier (Var nameR)) = [ IModCV { numL = l, memR = fst locs ! nameR } ]
interMod locs (Number l) (Identifier (ArrNum nameR indNR)) = [ IModCAc { numL = l, memR = fst locs ! nameR, cIndexR = indNR } ]
interMod locs (Number l) (Identifier (ArrVar nameR indVR)) = [ IModCA { numL = l, memR = fst locs ! nameR, indexR = fst locs ! indVR } ]
interMod locs (Identifier (Var nameL)) (Number r) = [ IModVC { memL = fst locs ! nameL, numR = r} ]
interMod locs (Identifier (Var nameL)) (Identifier (Var nameR)) = [ IModVV { memL = fst locs ! nameL, memR = fst locs ! nameR } ]
interMod locs (Identifier (Var nameL)) (Identifier (ArrNum nameR indNR)) = [ IModVAc { memL = fst locs ! nameL, memR = fst locs ! nameR, cIndexR = indNR } ]
interMod locs (Identifier (Var nameL)) (Identifier (ArrVar nameR indVR)) = [ IModVA { memL = fst locs ! nameL, memR = fst locs ! nameR, indexR = fst locs ! indVR } ]
interMod locs (Identifier (ArrNum nameL indNL)) (Number r) = [ IModAcC { memL = fst locs ! nameL, cIndexL = indNL, numR = r} ]
interMod locs (Identifier (ArrNum nameL indNL)) (Identifier (Var nameR)) = [ IModAcV { memL = fst locs ! nameL, cIndexL = indNL, memR = fst locs ! nameR } ]
interMod locs (Identifier (ArrNum nameL indNL)) (Identifier (ArrNum nameR indNR)) = [ IModAcAc { memL = fst locs ! nameL, cIndexL = indNL, memR = fst locs ! nameR, cIndexR = indNR } ]
interMod locs (Identifier (ArrNum nameL indNL)) (Identifier (ArrVar nameR indVR)) = [ IModAcA { memL = fst locs ! nameL, cIndexL = indNL, memR = fst locs ! nameR, indexR = fst locs ! indVR } ]
interMod locs (Identifier (ArrVar nameL indVL)) (Number r) = [ IModAC { memL = fst locs ! nameL, indexL = fst locs ! indVL, numR = r} ]
interMod locs (Identifier (ArrVar nameL indVL)) (Identifier (Var nameR)) = [ IModAV { memL = fst locs ! nameL, indexL = fst locs ! indVL, memR = fst locs ! nameR } ]
interMod locs (Identifier (ArrVar nameL indVL)) (Identifier (ArrNum nameR indNR)) = [ IModAAc { memL = fst locs ! nameL, indexL = fst locs ! indVL, memR = fst locs ! nameR, cIndexR = indNR } ]
interMod locs (Identifier (ArrVar nameL indVL)) (Identifier (ArrVar nameR indVR)) = [ IModAA { memL = fst locs ! nameL, indexL = fst locs ! indVL, memR = fst locs ! nameR, indexR = fst locs ! indVR } ]

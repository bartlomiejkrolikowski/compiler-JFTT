module Translation.Expressions
( computeNumber
, storeValIn
, loadValFrom
, joinCmds
, appendLength
, loadVal
, codeSaveVal
, codeBinExpr
, cmdsAdd
, cmdsSub
, cmdsMult
, cmdsDivMod
, cmdsDiv
, cmdsMod
, plusExpr
, minusExpr
, timesExpr
, divExpr
, modExpr
, eqCond
, neqCond
, leCond
, geCond
, leqCond
, geqCond
) where

import Translation.Code
import Grammar.Data
import Translation.FastBinRegMultCode
import Translation.FastBinRegDivModCode

address = undefined

-- wyrazenia zwracaja komendy (+ ich liczba) zwracajace wartosc w Ra
-- warunki zwracaja dodatkowo rodzaj warunku (jako CondType)

-- pobierane i zwracane komendy sa w docelowej kolejnosci (czyli odwrocone)

-- obliczenie stalej wartosci do Ra (zajmuje Ra i Rb) (tylko: n > 0 -- znak w wasPositive)
computeAbsNumber :: Bool -> Int -> Code -> Code
computeAbsNumber _ 0 prog = (reverse initialCommands) ++ prog
    where initialCommands = [ Reset Ra -- Ra == 0
                            , Reset Rb -- Rb == 0
                            , Inc Rb   -- Rb == 1
                            ]
computeAbsNumber wasPositive n prog = let cmds = if n `mod` 2 == 0
                                                   then evenCommands
                                                   else oddCommands
                                      in  reverse cmds ++ computeAbsNumber wasPositive (n `div` 2) prog
    where oddCommands = [ Shift Rb         -- w Ra jest (n `div` 2) -- b==1
                        , incrementCommand -- n jest nieparzyste (+1 lub -1 w zaleznosci od rzeczywistego znaku n)
                        ]
          evenCommands = [ Shift Rb ] -- w Ra jest (n `div` 2) -- b==1
          incrementCommand = if wasPositive then Inc Ra else Dec Ra

-- pobiera stala i poprzednie komendy i dopisuje do nich komendy obliczajace ta stala
computeNumber :: Int -> Code -> Code
computeNumber n prog = computeAbsNumber (n > 0) (abs n) prog

-- zapis wartosci obliczanej podanymi komendami do adresu obliczanego komendami podanymi na 2 miejscu
--    (wszystkie obliczenia zwracaja wynik w Ra) (rejestry nie moga byc aktualnie uzywane)
storeValIn :: Code -> Code -> Code
storeValIn cmdsVal cmdsAddr = reverse restoreAndSaveResult ++ cmdsAddr ++ reverse saveTmpResult ++ cmdsVal
    where saveTmpResult = [ Swap Rh ] -- w Ra jest wynik, zachowuje go w Rh
          restoreAndSaveResult = [ Swap Rh -- w Ra jest adres, w Rh wynik
                                 , Store Rh
                                 ]

-- pobiera wartosc do Ra spod adresu obliczanego przez podane komendy (do Ra)
loadValFrom :: Code -> Code
loadValFrom cmdsAddr = reverse loadToRa ++ cmdsAddr
    where loadToRa = [ Load Ra ] -- w Ra jest adres i laduje do Ra

-- sklejenie dwoch ciagow komend
joinCmds :: (Code, Int) -> (Code, Int) -> (Code, Int)
joinCmds (cmd1,len1) (cmd2,len2) = (cmd1 ++ cmd2, len1 + len2)

-- dodaje informacje o dlugosci listy (dla Code to liczba komend)
appendLength :: [a] -> ([a],Int)
appendLength x = (x, length x)

-- zapisuje wartosc odpowiadajaca argumentowi do Ra
-- korzysta z Ra, Rb i Rc
loadVal :: Value -> Code
loadVal = matchingLoadVal
    where matchingLoadVal (Number n) = computeNumber n []
          matchingLoadVal (Identifier (Var var)) = cmdsLoadAddr $ address var
          matchingLoadVal (Identifier (ArrNum arr ind)) = cmdsLoadAddr $ address arr + ind
          matchingLoadVal (Identifier (ArrVar arr ind)) =
              reverse cmdsLoadIndexed ++ (cmdsLoadAddr $ address ind) ++ reverse cmdsSaveArrOffset ++ computeNumber (address arr) []
          -- cmdsLoad = [ Load Ra ] -- laduje wartoc spod adresu w Ra do Ra
          cmdsLoadIndexed = [ Add Rc -- podstawa tablicy jest w Rc, indeks w Ra
                            , Load Ra -- po dodaniu otrzymuje ostateczny adres
                            ]
          cmdsSaveArrOffset = [ Swap Rc ] -- zachowuje obliczona podstawe tablicy w Rc
          cmdsLoadAddr addr = loadValFrom $ computeNumber addr [] -- liczy adres zmiennej do Ra i laduje stamtad wartosc do Ra

-- zachowuje pobrana wartosc w Rd (na potrzeby pobrania kolajnej wartosci)
codeSaveVal :: Code
codeSaveVal = [ Swap Rd ]

-- wyrazenia

-- wykonuje obliczenie wyrazenia
-- pobiera komendy liczace wyrazenie (na podstawie wartosci w Ra-pierwsza i Rd-druga), ladujace pierwszy argument i ladujace drugi argument
codeBinExpr :: Code -> Code -> Code -> Code
codeBinExpr codeExpr codeA codeD = codeExpr ++ codeA ++ codeSaveVal ++ codeD

-- dodaje dwie wartosci obliczone odpowiednio w Ra i Rd przez podane komendy
cmdsAdd :: Code -> Code -> (Code, Int)
cmdsAdd codeA codeD = appendLength $ codeBinExpr codeAddRegs codeA codeD
    where codeAddRegs = [ Add Rd ] -- dodaje Ra do Rd, wynik w Ra

-- kompiluje dodawanie dwoch wartosci, wynik zapisuje do Ra
plusExpr :: Value -> Value -> (Code, Int)
plusExpr valL valR = cmdsAdd (loadVal valL) (loadVal valR)

-- odejmuje dwie wartosci obliczone odpowiednio w Ra i Rd przez podane komendy
cmdsSub :: Code -> Code -> (Code, Int)
cmdsSub codeA codeD = appendLength $ codeBinExpr codeSubRegs codeA codeD
    where codeSubRegs = [ Sub Rd ] -- odejmuje Rd od Ra, wynik w Ra

-- kompiluje odejmowanie dwoch wartosci, wynik zapisuje do Ra
minusExpr :: Value -> Value -> (Code, Int)
minusExpr valL valR = cmdsSub (loadVal valL) (loadVal valR)

-- mnozy dwie wartosci obliczone odpowiednio w Ra i Rd przez podane komendy
cmdsMult :: Code -> Code -> (Code, Int)
cmdsMult codeA codeD = appendLength $ codeBinExpr codeMultRegs codeA codeD
    where codeMultRegs = reverse fastBinRegMultCode -- mnozy Ra przez Rd, wynik w Ra

-- kompiluje dodawanie dwoch wartosci, wynik zapisuje do Ra
timesExpr :: Value -> Value -> (Code, Int)
timesExpr valL valR = cmdsMult (loadVal valL) (loadVal valR)

-- oblicza div i mod dwoch wartosci obliczonych odpowiednio w Ra i Rd przez podane komendy
cmdsDivMod :: Code -> Code -> (Code, Int)
cmdsDivMod codeA codeD = appendLength $ codeBinExpr codeDivModRegs codeA codeD
    where codeDivModRegs = reverse fastBinRegDivModCode -- wykonuje obliczenia, wyniki: div w Ra, mod w Rb

-- oblicza div dwoch wartosci obliczonych odpowiednio w Ra i Rd przez podane komendy
cmdsDiv :: Code -> Code -> (Code, Int)
cmdsDiv codeA codeD = cmdsDivMod codeA codeD

-- oblicza mod dwoch wartosci obliczonych odpowiednio w Ra i Rd przez podane komendy
cmdsMod :: Code -> Code -> (Code, Int)
cmdsMod codeA codeD = joinCmds cmdsGetMod $ cmdsDivMod codeA codeD
    where cmdsGetMod = appendLength [ Swap Rb ] -- mod obliczone w Rb

-- kompiluje div dwoch wartosci, wynik zapisuje do Ra
divExpr :: Value -> Value -> (Code, Int)
divExpr valL valR = cmdsDiv (loadVal valL) (loadVal valR)

-- kompiluje mod dwoch wartosci, wynik zapisuje do Ra
modExpr :: Value -> Value -> (Code, Int)
modExpr valL valR = cmdsMod (loadVal valL) (loadVal valR)

-- warunki

-- oblicza roznice podanych zmiennych, ktora potem bedzie potraktowana w zaleznosci od rodzaju warunku
generalCond :: Value -> Value -> (Code, Int)
generalCond = minusExpr

-- funkcje dla odpowiednich warunkow

eqCond :: Value -> Value -> ((Code, Int), CondType)
eqCond v1 v2 = (generalCond v1 v2, CondEq)

neqCond :: Value -> Value -> ((Code, Int), CondType)
neqCond v1 v2 = (generalCond v1 v2, CondNEq)

leCond :: Value -> Value -> ((Code, Int), CondType)
leCond v1 v2 = (generalCond v1 v2, CondLe)

geCond :: Value -> Value -> ((Code, Int), CondType)
geCond v1 v2 = (generalCond v1 v2, CondGe)

leqCond :: Value -> Value -> ((Code, Int), CondType)
leqCond v1 v2 = (generalCond v1 v2, CondLEq)

geqCond :: Value -> Value -> ((Code, Int), CondType)
geqCond v1 v2 = (generalCond v1 v2, CondGEq)

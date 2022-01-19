module Instructions
( Code(..)
, Instruction(..)
, Register(..)
, CondType(..)
, assign
, ifElse
, onlyIf
, whileLoop
, repeatLoop
, forToLoop
, forDownToLoop
, readAndStore
, writeVal
-- pobrane z Expressions
, computeNumber
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

import GrammarTree
import Expressions
import Code

data Direction = Up | Down deriving (Eq,Show)

-- instrukcje dostawiaja komendy na poczatek (na koniec nalezy odwrocic Code)
-- nie zostawiaja po sobie rejestrow w uzyciu ("chowaja" wszystko do pamieci)

-- wykonanie przypisanie do zmiennej (gdy obliczono w Ra wartosc do przypisania)
-- pobiera adres docelowy (dest) i poprzednie komendy, razem z licznikiem instrukcji
assignVar :: Int -> (Code, Int) -> (Code, Int)
assignVar dest (prog, len) = let cmds = reverse cmdsEnd ++ computeNumber dest (reverse cmdsBegin)
                             in  (cmds ++ prog, length cmds + len)
    where cmdsBegin = [ Swap Rh ] -- zachowuje obliczona wartosc
          cmdsEnd = [ Swap Rh  -- zamieniam obliczony adres i wartosc
                    , Store Rh -- zachowuje wartosc pod obliczonym adresem
                    ]

-- wykonanie przypisanie do tablicy (gdy obliczono w Ra wartosc do przypisania)
--    (indeks musi byc w Rb)
-- pobiera adres zerowego elementu docelowej tablicy (dest) i poprzednie komendy, razem z licznikiem instrukcji
assignArr :: Int -> (Code, Int) -> (Code, Int)
assignArr dest (prog, len) = let cmds = reverse cmdsEnd ++ computeNumber dest (reverse cmdsBegin)
                             in  (cmds ++ prog, length cmds + len)
    where cmdsBegin = [ Swap Rh -- zachowuje obliczona wartosc
                      , Swap Rb -- przenosze indeks do Ra
                      , Swap Rg -- zachowuje indeks
                      ]
          cmdsEnd = [ Add Rg   -- powiekszam obliczony adres o wartosc indeksu (wynik w Ra) (bo dotyczy indesku 0)
                    , Swap Rh  -- zamieniam obliczony adres i wartosc
                    , Store Rh -- zachowuje wartosc pod obliczonym adresem
                    ]

-- kompiluje instrukcje assign
-- pobiera identyfikator, do ktorego trzeba przypisac wartosc oraz komendy obliczajace wartosc do przypisania
--    (powinna byc obliczona do Ra)
assign :: Identifier -> (Code, Int) -> (Code, Int)
assign = matchingAssign
    where -- matchingAssign (Var (Iterator _ _ _)) _ = error "assigning to constant" -- niepotrzebne, bo sprawdzam juz w parserze, a tak moge zainicjowac iterator w for przez assign
          matchingAssign (ArrNum arr ind) cmdsExp = assignVar (address arr + ind) cmdsExp -- licze od razu docelowy adres i traktuje jak zwykla zmienna
          matchingAssign (ArrVar arr ind) cmdsExp = assignArr (address arr) $ joinCmds (getVarIndex ind) cmdsExp
          matchingAssign (Var sing) cmdsExp = assignVar (address sing) cmdsExp
          computeIndex i = let cmds = reverse restoreRa ++ computeNumber i (reverse saveRa) in (cmds, length cmds)
          getVarIndex i = let cmds = reverse restoreRa ++ reverse (loadIndex . address $ i) ++ reverse saveRa in (cmds, length cmds)
          saveRa = [ Swap Rh ] -- zachowuje obliczona wartosc
          loadIndex addr = [ Load Ra ] ++ computeNumber addr [] -- licze adres zmiennej, a na koncu pobieram stamtad wartosc indeksu
          restoreRa = [ Swap Rb -- zachowuje znaleziony indeks do Rb
                      , Swap Rh -- przywracam wartosc wyrazania do Ra
                      ]

-- kompiluje instrukcje if-else-endif
-- pobiera informacje o rodzaju warunku, komendy obliczajace warunek i komendy dla True oraz False (+ licznik)
--    (warunek powinien byc obliczany do Ra)
ifElse :: CondType -> (Code, Int) -> (Code, Int) -> (Code, Int) -> (Code, Int)
ifElse c cmdsCond cmdsT cmdsF = doIfElse c cmdsT cmdsF cmdsCond
    where doIfElse :: CondType -> (Code, Int) -> (Code, Int) -> (Code, Int) -> (Code, Int)
          -- symuluje if-else-endif (gdy w Ra jest juz obliczone wyrazenie warunkowe (roznica: pierwsza - druga wartosc))
          -- pobiera informacje o rodzaju warunku, komendy dla True, komendy dla False oraz poprzednie komendy (+ licznik instrukcji)
          doIfElse CondEq progT progF prog = generalIfElse eqJump progF progT prog
          doIfElse CondNEq progT progF prog = generalIfElse eqJump progT progF prog
          doIfElse CondLe progT progF prog = generalIfElse leJump progF progT prog
          doIfElse CondGe progT progF prog = generalIfElse leqJump progT progF prog -- leq = ~ge
          doIfElse CondLEq progT progF prog = generalIfElse leqJump progF progT prog
          doIfElse CondGEq progT progF prog = generalIfElse leJump progT progF prog -- le = ~geq
          generalIfElse jumpCode (progSkip, lenSkip) (progJumpTo, lenJumpTo) (progPrev, lenPrev) =
              let skipJump = skipping lenJumpTo
                  lenSkipJump = length skipJump
                  condJump = jumpCode (lenSkipJump + lenSkip)
                  lenCondJump = length condJump
              in  (progJumpTo ++ reverse skipJump ++ progSkip ++ reverse condJump ++ progPrev
                  , lenJumpTo + lenSkipJump + lenSkip + lenCondJump + lenPrev)
          skipping blockLength = [ Jump (blockLength + 1) ] -- przeskakuje blok podanej dlugosci (+1, bo trzeba przeskoczyc tez aktulna instrukcje
          eqJump blockLength = [ Jzero (blockLength + 1) ]
          leJump blockLength = [ Jneg (blockLength + 1) ]
          leqJump blockLength = [ Dec Ra -- sprowadzam do przypadku leJump
                                , Jneg (blockLength + 1)
                                ]

-- kompiluje instrukcje if-endif
-- pobiera informacje o rodzaju warunku, komendy obliczajace warunek i komendy dla True (+ licznik)
--    (warunek powinien byc obliczany do Ra)
onlyIf :: CondType -> (Code, Int) -> (Code, Int) -> (Code, Int)
onlyIf c cmdsCond cmdsT = ifElse c cmdsCond cmdsT ([],0) -- symuluje za pomoca if-else-endif

-- kompiluje petle
-- pobiera informacje o rodzaju warunku, komendy na wejsciu do petli, komendy obliczajace warunek,
--    komendy dla wnetrza petli, komendy na koniec petli (+ licznik instrukcji)
--    (warunek powinien byc obliczany do Ra)
generalLoop :: CondType -> (Code, Int) -> (Code, Int) -> (Code, Int) -> (Code, Int) -> (Code, Int)
generalLoop = matchingGeneralLoop
    where matchingGeneralLoop CondEq cmdsIn cmdsCond cmdsLoop cmdsLast = generalLoopSchema cmdsIn cmdsCond eqJump stop cmdsLoop cmdsLast
          matchingGeneralLoop CondNEq cmdsIn cmdsCond cmdsLoop cmdsLast = generalLoopSchema cmdsIn cmdsCond eqJump stay cmdsLoop cmdsLast
          matchingGeneralLoop CondLe cmdsIn cmdsCond cmdsLoop cmdsLast = generalLoopSchema cmdsIn cmdsCond leJump stop cmdsLoop cmdsLast
          matchingGeneralLoop CondGe cmdsIn cmdsCond cmdsLoop cmdsLast = generalLoopSchema cmdsIn cmdsCond leqJump stay cmdsLoop cmdsLast
          matchingGeneralLoop CondLEq cmdsIn cmdsCond cmdsLoop cmdsLast = generalLoopSchema cmdsIn cmdsCond leqJump stop cmdsLoop cmdsLast
          matchingGeneralLoop CondGEq cmdsIn cmdsCond cmdsLoop cmdsLast = generalLoopSchema cmdsIn cmdsCond leJump stay cmdsLoop cmdsLast
          generalLoopSchema (progIn, lenIn) (progCond, lenCond) jumpCode branchingCode (progLoop, lenLoop) (progLast, lenLast) =
              let lenJumpCode = length jumpCode
                  lenBackJump = lenBackToLoop
                  branchingCmds = branchingCode (lenLoop + lenLast + lenBackJump)
                  lenBranchingCmds = length branchingCmds
                  backJump = backToLoop (lenCond + lenJumpCode + lenBranchingCmds + lenLoop + lenLast)
              in  ( reverse backJump ++ progLast ++ progLoop ++ reverse branchingCmds ++ reverse jumpCode ++ progCond ++ progIn
                  , lenBackJump + lenLast + lenLoop + lenBranchingCmds + lenJumpCode + lenCond + lenIn )
          stop = skipping -- wyskakuje z petli o podanej pozostalej liczbie instrukcji (jesli nie zostal przeskoczony)
          lenStop = lenSkipping
          stay blockLength = skipping lenSkipping ++ skipping blockLength -- wskakuje do petli jesli nie bylo przeskoku, w przeciwnym wypadku wyskakuje z petli
          lenStay = lenSkipping + lenSkipping
          skipping blockLength = [ Jump (blockLength + 1) ] -- przeskakuje blok podanej dlugosci (+1, bo trzeba przeskoczyc tez aktulna instrukcje
          lenSkipping = 1
          eqJump = [ Jzero (lenSkipping + 1) ] -- jesli potem jest stay to skacze do wyjscia z petli (zostaje jesli niespelniony), jesli stop to do pierwszej instr. petlowej
          leJump = [ Jneg (lenSkipping + 1) ] -- jak wyzej
          leqJump = [ Dec Ra -- sprowadzam do przypadku leJump
                    , Jneg (lenSkipping + 1)
                    ]
          backToLoop loopLength = [ Jump (-loopLength) ] -- cofniecie sie o loopLength komend (czyli do poczatku petli)
          lenBackToLoop = 1

-- kompiluje instrukcje while
-- pobiera informacje o rodzaju warunku, komendy obliczajace warunek i komendy do wykonania wewnatrz petli (+ licznik)
--    (warunek powinien byc obliczany do Ra)
whileLoop :: CondType -> (Code, Int) -> (Code, Int) -> (Code, Int)
whileLoop c cmdsCond cmdsLoop = generalLoop c ([],0) cmdsCond cmdsLoop ([],0)

-- kompiluje instrukcje repeat
-- pobiera informacje o rodzaju warunku, komendy obliczajace warunek i komendy do wykonania wewnatrz petli (+ licznik)
--    (warunek powinien byc obliczany do Ra)
repeatLoop :: CondType -> (Code, Int) -> (Code, Int) -> (Code, Int)
repeatLoop c cmdsCond cmdsLoop = generalLoop (negateCond c) cmdsLoop cmdsCond cmdsLoop ([],0)

-- kompiluje instrukcje for
-- pobiera informacje o deklarowanym iteratorze, adresie konca zakresu, kierunku iteracji, komendy obliczajace poczatek zakresu,
--    komendy obliczajace koniec zakresu i komendy do wykonania wewnatrz petli (+ licznik)
forLoop :: Identifier -> Int -> Direction -> (Code, Int) -> (Code, Int) -> (Code, Int) -> (Code, Int)
forLoop iter rngAddr dir cmdsBeginRng cmdsEndRng cmdsLoop = generalLoop (iterCondition dir) cmdsIntro cmdsCond cmdsLoop (cmdsIterMove dir)
    where iterCondition :: Direction -> CondType -- warunek jaki musi spelniac iterator wzgledem konca zakresu (LEq dla Up, GEq dla Down)
          iterCondition Up = CondLEq
          iterCondition Down = CondGEq
          computeRngAddr = computeNumber rngAddr
          cmdsIterInit = assign iter cmdsBeginRng
          cmdsEndInit (codeEndRng, lenEndRng) = let codeEndInit = storeValIn codeEndRng (computeNumber rngAddr [])
                                                in  (codeEndInit, length codeEndRng)
          cmdsIntro = joinCmds (cmdsEndInit cmdsEndRng) cmdsIterInit
          cmdsCond = codeCmp (address $ decl iter) rngAddr
          cmdsIterMove Up = iterIncrement
          cmdsIterMove Down = iterDecrement
          iterIncrement = assign iter (appendLength $ [ Inc Ra ] ++ (loadVal $ Identifier iter))
          iterDecrement = assign iter (appendLength $ [ Dec Ra ] ++ (loadVal $ Identifier iter))

-- kompiluje instrukcje for-to
-- pobiera informacje o deklarowanym iteratorze, komendy obliczajace poczatek zakresu, komendy obliczajace koniec zakresu
--    i komendy do wykonania wewnatrz petli (+ licznik)
forToLoop :: Identifier -> (Code, Int) -> (Code, Int) -> (Code, Int) -> (Code, Int)
forToLoop iter cmdsBeginRng cmdsEndRng cmdsLoop = forLoop iter ((address $ decl iter) + 1) Up cmdsBeginRng cmdsEndRng cmdsLoop

-- kompiluje instrukcje for-downto
-- pobiera informacje o deklarowanym iteratorze, komendy obliczajace poczatek zakresu, komendy obliczajace koniec zakresu
--    i komendy do wykonania wewnatrz petli (+ licznik)
forDownToLoop :: Identifier -> (Code, Int) -> (Code, Int) -> (Code, Int) -> (Code, Int)
forDownToLoop iter cmdsBeginRng cmdsEndRng cmdsLoop = forLoop iter ((address $ decl iter) + 1) Down cmdsBeginRng cmdsEndRng cmdsLoop

-- kompiluje instrukcje read (uwaga: funkcja ma inna nazwe, bo read juz istnieje)
--    pobiera identyfikator, do ktorego nalezy czytac
readAndStore :: Identifier -> (Code, Int)
readAndStore ident = assign ident (reverse codeRead, length codeRead)
    where codeRead = [ Get ] -- czyta wartosc do Ra

-- kompiluje instrukcje write
--    pobiera komendy obliczajace wypisywana wartosc (powinny ja obliczac do Ra)
writeVal :: (Code, Int) -> (Code, Int)
writeVal (codeVal, lenVal) = (reverse codeWrite ++ codeVal, length codeWrite + lenVal)
    where codeWrite = [ Put ] -- wypisuje wartosc z Ra

-- oblicza warunek dla zmiennych o podanych adresach (na potrzeby for-ow)
codeCmp :: Int -> Int -> (Code, Int)
codeCmp addrL addrR = cmdsSub (loadValFrom $ computeNumber addrL []) (loadValFrom $ computeNumber addrR [])

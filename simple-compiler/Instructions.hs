module Instructions
( Code(..)
, Instruction(..)
, Register(..)
, CondType(..)
, computeNumber
, assign
) where

type Code = [Instruction]
data Instruction =
    Get | Put |
    Load { reg::Register } | Store { reg::Register } |
    Add { reg::Register } | Sub { reg::Register } | Shift { reg::Register } |
    Swap { reg::Register } | Reset { reg::Register } | Inc { reg::Register } |
    Dec { reg::Register } |
    Jump { offset::Int } | Jpos { offset::Int } | Jzero { offset::Int } |
    Jneg { offset::Int }
    deriving (Eq,Show)

data Register =
    Ra | Rb | Rc | Rd | Re | Rf | Rg | Rh
    deriving (Eq,Show)

data CondType =
    CondEq | CondNEq | CondLe | CondGe | CondLEq | CondGEq
    deriving (Eq,Show)

-- instrukcje dostawiaja komendy na poczatek (na koniec nalezy odwrocic Code)

-- obliczenie stalej wartosci do Ra (zajmuje Ra i Rb) (tylko: n > 0 -- znak w wasPositive)
computeAbsNumber :: Bool -> Int -> Code -> Code
computeAbsNumber _ 0 prog = (reverse initialCommands) ++ prog
computeAbsNumber wasPositive n prog = let cmds = if n `mod` 2 == 0
                                                   then evenCommands
                                                   else oddCommands
                                      in  reverse cmds ++ computeAbsNumber wasPositive (n `div` 2) prog
    where initialCommands = [ "RESET a" -- Ra == 0
                            , "RESET b" -- Rb == 0
                            , "INC b"   -- Rb == 1
                            ]
          oddCommands = [ "SHIFT b"        -- w Ra jest (n `div` 2) -- b==1
                        , incrementCommand -- n jest nieparzyste (+1 lub -1 w zaleznosci od rzeczywistego znaku n)
                        ]
          evenCommands = [ "SHIFT b" -- w Ra jest (n `div` 2) -- b==1
                         ]
          incrementCommand = if wasPositive then "INC a" else "DEC a"

computeNumber :: Int -> Code -> Code
computeNumber n prog = computeAbsNumber (n > 0) (abs n) prog

-- wykonanie przypisanie do zmiennej (gdy obliczono w Ra wartosc do przypisania)
-- pobiera adres docelowy (dest) i poprzednie komendy, razem z licznikiem instrukcji
assignVar :: Int -> (Code, Int) -> (Code, Int)
assignVar dest (prog, len) = let cmds = reverse cmdsEnd ++ computeNumber dest cmdsInit (reverse cmdsBegin)
                             in  (cmds ++ prog, length cmds + len)
    where cmdsBegin = [ "SWAP h" -- zachowuje obliczona wartosc
                      ]
          cmdsEnd = [ "SWAP h"  -- zamieniam obliczony adres i wartosc
                    , "STORE h" -- zachowuje wartosc pod obliczonym adresem
                    ]

-- wykonanie przypisanie do tablicy (gdy obliczono w Ra wartosc do przypisania)
--    (indeks musi byc w Rb)
-- pobiera adres zerowego elementu docelowej tablicy (dest) i poprzednie komendy, razem z licznikiem instrukcji
assignArr :: Int -> (Code, Int) -> (Code, Int)
assignArr dest (prog, len) = let cmds = reverse cmdsEnd ++ computeNumber dest cmdsInit (reverse cmdsBegin)
                             in  (cmds ++ prog, length cmds + len)
    where cmdsBegin = [ "SWAP h" -- zachowuje obliczona wartosc
                      , "SWAP b" -- przenosze indeks do Ra
                      , "SWAP g" -- zachowuje indeks
                      ]
          cmdsEnd = [ "ADD g"   -- powiekszam obliczony adres o wartosc indeksu (wynik w Ra) (bo dotyczy indesku 0)
                    , "SWAP h"  -- zamieniam obliczony adres i wartosc
                    , "STORE h" -- zachowuje wartosc pod obliczonym adresem
                    ]

-- kompiluje instrukcje assign
-- pobiera identyfikator, do ktorego trzeba przypisac wartosc oraz komendy obliczajace wartosc do przypisania
--    (powinna byc obliczona do Ra)
assign :: Identifier -> (Code, Int) -> (Code, Int)
assign (Var sing) cmdsExp = assignVar (address sing) cmdsExp
assign (ArrNum arr ind) cmdsExp = assignArr (address arr) (joinCmds (computeIndex ind) cmdsExp)
assign (ArrVar arr ind) cmdsExp = assignArr (address arr) (joinCmds (getVarIndex ind) cmdsExp)
assign (Var (Constant _ _ _)) _ = error "assigning to constant"
    where computeIndex i = let cmds = reverse restoreRa ++ computeNumber i (reverse saveRa) in (cmds, length cmds)
          getVarIndex i = let cmds = reverse restoreRa ++ reverse (loadIndex . address $ i) ++ reverse saveRa in (cmds, length cmds)
          saveRa = [ "SWAP h" -- zachowuje obliczona wartosc
                   ]
          loadIndex addr = [ "LOAD a" ] ++ computeNumber addr ([],0) -- licze adres zmiennej, a na koncu pobieram stamtad wartosc indeksu
          restoreRa = [ "SWAP b" -- zachowuje znaleziony indeks do Rb
                      , "SWAP h" -- przywracam wartosc wyrazania do Ra
                      ]

-- sklejenie dwoch ciagow komend
joinCmds (cmd1,len1) (cmd2,len2) = (cmd1 ++ cmd2, len1 + len2)

-- symuluje if-else-endif (gdy w Ra jest juz obliczone wyrazenie warunkowe (roznica: pierwsza - druga wartosc))
-- pobiera informacje o rodzaju warunku, komendy dla True, komendy dla False oraz poprzednie komendy (+ licznik instrukcji)
doIfElse :: CondType -> (Code, Int) -> (Code, Int) -> (Code, Int) -> (Code, Int)
doIfElse CondEq progT progF prog = generalIfElse eqJump progF progT
doIfElse CondNEq progT progF prog = generalIfElse eqJump progT progF
doIfElse CondLe progT progF prog = generalIfElse leJump progF progT
doIfElse CondGe progT progF prog = generalIfElse leJump progT progF
doIfElse CondLEq progT progF prog = generalIfElse leqJump progF progT
doIfElse CondGEq progT progF prog = generalIfElse leqJump progT progF
    where generalIfElse jumpCode (progSkip, lenSkip) (progJumpTo, lenJumpTo) (progPrev, lenPrev) =
              let skipJump = skipping lenJumpTo
                  lenSkipJump = length skipJump
                  condJump = jumpCode (lenSkipJump + lenSkip)
                  lenCondJump = length condJump
              in  (progJumpTo ++ reverse skipJump ++ progSkip ++ reverse condJump ++ progPrev
                  , lenJumpTo + lenSkipJump + lenSkip + lenCondJump + lenPrev)
          skipping blockLength = [ "JUMP " ++ show (blockLength + 1) ] -- przeskakuje blok podanej dlugoski (+1, bo trzeba przeskoczyc tez aktulna instrukcje
          eqJump blockLength = [ "JZERO " ++ show (blockLength + 1) ]
          leJump blockLength = [ "JNEG " ++ show (blockLength + 1) ]
          leqJump blockLength = [ "DEC a" -- sprowadzam do przypadku leJump
                                , "JNEG " ++ show (blockLength + 1)
                                ]

-- symuluje if-endif (gdy w Ra jest juz obliczone wyrazenie warunkowe (roznica: pierwsza - druga wartosc))
-- pobiera informacje o rodzaju warunku, komendy dla True oraz poprzednie komendy (+ licznik instrukcji)
doIf :: CondType -> (Code, Int) -> (Code, Int) -> (Code, Int)
doIf c progT prog = doIfElse c progT ([],0) prog -- symuluje za pomoca if-else-endif

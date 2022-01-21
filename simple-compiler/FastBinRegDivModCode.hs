module FastBinRegDivModCode
( fastBinRegDivModCode
) where

import Code

-- FastBinRegMultCode.hs
-- Bartlomiej Krolikowski

-- komendy efektywnie dzielace Ra przez Rd
-- uwagi:
--    - korzysta z rejestrow od Ra do Rh
--    - kolejnosc komend taka jak w rzeczywistosci (trzeba odwrocic przed wlozeniem do funkcji)
--    - zapisuje wyniki: Ra <- div Rb <- mod
fastBinRegDivModCode :: Code
fastBinRegDivModCode = initDivisor ++ initDivMod ++ loopDivMod ++ endDivMod

-- obsluguje przypadki znaku dzielnika
-- zapisuje informacje o znaku Rd do Rc (0 dla +, Rd (< 0) dla -), a w Rd zapisuje abs(Rd)
initDivisor :: Code
initDivisor = [ Swap Rd
              , Jzero 2  -- skok dla Rd == 0
              , Jump 4   -- Rd nie jest 0 - skok do zwyklego dzielenia
              , Reset Ra -- vvv (tu: Rd == 0)
              , Reset Rb -- ^^^ wynik dzielenia przez 0 to (0,0)
              , Jump (8 + length initDivMod + length loopDivMod + length endDivMod) -- przeskakuje na koniec
              , Jneg 3   -- skok do przypadku Rd < 0
              , Reset Rc -- zapisanie do Rc, ze Rd > 0 (Rc <- 0)
              , Jump 4   -- skok do dzielenia
              , Swap Rc  -- vvv (Rd < 0)
              , Reset Ra
              , Sub Rc   -- ^^^ zapisanie do Rc, ze Rd < 0 (Rc <- Rd), a do Rd: -Rd (== abs(Rd)) (koszt: 12)
              , Swap Rd  -- przywrocenie Ra i Rd
              ]

-- instrukcje przed petla wykonujaca dzielenie (argumenty podane w Ra i Rd, Rd > 0)
-- najpierw mierzy dlugosc bitowa liczby w Ra
-- potem wykonuje przypadek brzegowy dzielenia (dla Ra == 0 lub -1)
-- uzywa rejestrow od Ra do Rh (wyniki beda w Rg i Rh)
initDivMod :: Code
initDivMod = [ Reset Rb  -- vvv
             , Swap Rb
             , Add Rb    -- ^^^ skopiowanie Ra do Rb (koszt: 12)
             , Reset Rf  -- vvv
             , Dec Rf    -- ^^^ zapisanie -1 do Rf (koszt: 2)
             , Reset Re  -- zapisuje 0 do Re (to bedzie akumulator dlugosci)
             , Jzero 7   -- vvv (petla) przeskok dla Ra == 0
             , Inc Ra
             , Jzero 11  --             przeskok dla Ra == -1
             , Dec Ra    --     Ra nie bylo 0 ani -1
             , Shift Rf  --     Ra /= 2
             , Inc Re    --     dlugosc++
             , Jump (-6) -- ^^^ petla mierzaca dlugosc Ra (koszt: 11)
             , Reset Rg  -- vvv (tu: Ra == 0)
             , Swap Rh
             , Reset Ra
             , Sub Rd
             , Swap Rh   -- ^^^ przypadek brzegowy dla 0: wynik = (0,0) (Rh == mod - Rd) (koszt: 14)
             , Jump 6
             , Dec Ra    -- vvv (tu: Ra == -1 <=> Ra+1 == 0)
             , Reset Rg
             , Dec Rg
             , Reset Rh
             , Dec Rh    -- ^^^ przypadek brzegowy dla -1: wynik = (-1, Rd-1) (Rh == mod - Rd) (koszt: 5)
             , Swap Rf   -- vvv
             , Reset Ra
             , Sub Re
             , Swap Rf   -- ^^^ zapisanie -Re do Rf (koszt: 13)
             ]

-- idea: przechodze po kolejnych bitach Ra od konca,
--       licze korzystajac ze wzorow: x = d*y + m, 2*x = 2*d * y + 2*m, 2*x+1 = 2*d * y + 2*m+1
--                                    (+ odpowiednie modyfikacje d i m obcinajace m do [0,y))
--       dla: d = (x div y)
--            m = (x mod y)
--       (uwaga: tylko dla Rd > 0)
loopDivMod :: Code
loopDivMod = [ Swap Re    -- koniec jesli przetworzylem juz najnizszy bit
             , Jzero 28
             , Swap Re    -- vvv (w Ra jest poprzednio przetwarzany bit)
             , Shift Re
             , Swap Rb
             , Sub Rb
             , Swap Rb    -- ^^^ usuwam ten bit z Rb (koszt: 18) (uwaga: tu juz jest Rb >= 0)
             , Reset Ra   -- vvv
             , Inc Ra
             , Swap Rg
             , Shift Rg
             , Swap Rg
             , Swap Rh
             , Shift Rh
             , Add Rd
             , Swap Rh    -- ^^^ Rg <- 2*Rg, Rh <- 2*Rh + Rd (== 2*mod - Rd) (koszt: 26)
             , Reset Ra   -- vvv
             , Add Rb     -- ^^^ kopiuje wynik do Ra (koszt: 11)
             , Dec Re     -- vvv
             , Inc Rf     -- ^^^ update dlugosci liczby (koszt: 2)
             , Shift Rf   -- pobieram nastepny bit (koszt: 5)
             , Jzero 2
             , Inc Rh     -- Rh++ (w Rh jest: mod - Rd)
             , Swap Rh
             , Jneg 3
             , Sub Rd     -- vvv (Rh >= 0)
             , Inc Rg     -- ^^^ mod przekroczyl Rd-1 (mod - Rd == Rh >= 0) - obcinam do [0,Rd) (koszt: 11)
             , Swap Rh    -- tu: w Ra jest wlasnie przetworzony bit
             , Jump (-28)
             ]

-- dostosowuje wynik w zaleznosci od znaku Rd i przenosi wyniki: Ra <- div = Rg, Rb <- mod = Rh + Rd
endDivMod :: Code
endDivMod = [ Swap Rh  -- vvv
            , Add Rd   -- ^^^ wstawienie mod do Ra (koszt: 11)
            , Swap Rc  -- odczytanie oryginalnego znaku Rd
            , Jzero 9
            , Swap Rc  -- tu: Rd < 0
            , Jzero 3
            , Inc Rg   -- vvv -div <- -div+1 (tu: mod nie jest 0, czyli bylo zaokraglone w dol == zaokraglenie w gore dla ujemnego Rd)
            , Add Rc   -- ^^^ mod <- mod + Rd (oryginalne)   (koszt: 11)
            , Swap Rb  -- vvv
            , Reset Ra
            , Sub Rg   -- ^^^ zapisanie mod (z Ra) do Rb i div (w Rg jest -div) do Ra (koszt: 12)
            , Jump 4   -- skok koncowy
            , Swap Rc  -- vvv
            , Swap Rb
            , Swap Rg  -- ^^^ zapisanie mod (z Ra) do Rb i div (z Rg) do Ra (koszt: 3)
            ]

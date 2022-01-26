module Translation.FastBinRegMultCode
( fastBinRegMultCode
) where

import Translation.Code

-- FastBinRegMultCode.hs
-- Bartlomiej Krolikowski

-- komendy efektywnie mnozace Ra przez Rd
-- uwagi:
--    - korzysta z rejestrow od Ra do Rf
--    - kolejnosc komend taka jak w rzeczywistosci (trzeba odwrocic przed wlozeniem do funkcji)
fastBinRegMultCode :: Code
fastBinRegMultCode = putMinInRa ++ initMult ++ loopMult ++ endMult

-- uklada argumenty (podane w Rd i Ra) tak, aby ten w Ra mial mniejsza wartosc bezwzgledna
-- idea: jesli abs(x) < abs(y) to y < -x i y < x lub -x < y i x < y (i na odwrot)
--       (nierownosc nie zmienia sie gdy zmieniamy znak x)
putMinInRa :: Code
putMinInRa = [ Reset Rb -- vvv
             , Swap Rb
             , Add Rb   -- ^^^ skopiowanie Ra do Rb (koszt: 12)
             , Sub Rd   -- porownanie Ra z Rd (koszt: 10)
             , Jzero 14 -- przeskok do wyjscia (sa rowne)
             , Jneg 6   -- przeskok dla Ra < Rd
             , Reset Ra -- vvv tu: Ra > Rd
             , Sub Rb   -- ^^^ zapisanie -Rb do Ra (koszt: 11)
             , Sub Rd   -- porownanie Ra z Rd (koszt: 10)
             , Jneg 6   -- przeskok dla Ra > Rd, -Ra < Rd (abs(Ra) > abs(Rd))
             , Jump 8   -- przeskok dla Ra > Rd, -Ra > Rd (abs(Ra) < abs(Rd))
             , Reset Ra -- vvv tu: Ra < Rd
             , Sub Rb   -- ^^^ zapisanie -Rb do Ra (koszt: 11)
             , Sub Rd   -- porownanie Ra z Rd (koszt: 10)
             , Jneg 4   -- przeskok dla Ra < Rd, -Ra < Rd (abs(Ra) < abs(Rd))
             , Swap Rb  -- odtworzenie Ra (tu: abs(Ra) > abs(Rd))
             , Swap Rd  -- abs(Rd) jest mniejsze
             , Jump 2   -- przeskok do wyjscia
             , Swap Rb  -- odtworzenie Ra (tu: abs(Ra) < abs(Rd))
             ]

-- rozpoczyna petle wykonujaca mnozenie (argumenty podane w Ra i Rd)
initMult :: Code
initMult = [ Reset Rb -- vvv
           , Swap Rb
           , Add Rb   -- ^^^ skopiowanie Ra do Rb (koszt: 12)
           , Reset Rc -- zapisuje 0 do Rc (to bedzie akumulator)
           , Reset Re -- vvv
           , Inc Re   -- ^^^ zapisanie 1 do Re (koszt: 2)
           , Reset Rf -- vvv
           , Dec Rf   -- ^^^ zapisanie -1 do Rf (koszt: 2)
           ]

-- tresc mnozacej petli
-- rejestry:
--    Ra - czynnik (mniejsze abs)
--    Rb - kopia Ra
--    Rc - akumulator
--    Rd - czynnik (wieksze abs)
--    Re - 1
--    Rf - -1
loopMult :: Code
loopMult = [ Jzero 22   -- wyjscie z petli (warunek: Ra == 0 - w Rc jest wynik)
           , Inc Ra
           , Jzero 17   -- wyjscie z petli (warunek: Ra == -1 - trzeba od Rc odjac Rd: -1*Rd = -Rd)
           , Dec Ra     -- tu: Ra nie jest -1 ani 0
           , Shift Rf   -- vvv Ra /= 2
           , Shift Re   --     Ra *= 2
           , Sub Rb     -- ^^^ obliczenie Ra mod 2 (-1 dla nieparzystych, 0 dla parzystych) (koszt: 20)
           , Jzero 4    -- jesli Ra jest parzyste to pomijam zmiane akumulatora
           , Swap Rc    -- vvv (tu Ra jest nieparzyste)
           , Add Rd
           , Swap Rc    -- ^^^ dodanie do wyniku Rd * (Ra mod 2) == 1 (koszt: 12)
           , Swap Rd    -- vvv
           , Shift Re
           , Swap Rd    -- ^^^ Rd *= 2 (koszt: 7)
           , Add Rb     -- vvv (odtwarzam Ra - poprzednio bylo Sub Rb)
           , Shift Rf
           , Swap Rb
           , Shift Rf   -- ^^^ Ra,Rb /= 2 (koszt: 21)
           , Jump (-18) -- powrot na poczatek petli
           , Swap Rc    -- vvv (tu: Ra == -1)
           , Sub Rd
           , Swap Rc    -- ^^ ostatnie dodawanie: Rc += Ra * Rd <=> Rc += -Rd <=> Rc -= Rd (koszt: 12)
           ]

-- zapisuje wynik do Ra (wynik jest w Rc)
endMult :: Code
endMult = [ Swap Rc ]

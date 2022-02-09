autor: Bartłomiej Królikowski

pliki:
Haskell:
 Code.hs - definicje typów reprezentujących instrukcje maszyny
 Expressions.hs - funkcje tłumczące wyrażenia i warunki na ciągi instrukcji
 FastBinRegDivModCode - kod wykonujący dzielenie (czas: O(liczba bitów))
 FastBinRegMultCode - kod wykonujący mnozenie (czas: O(liczba bitów))
 GrammarTree.hs - definicje typów reprezentujących drzewo wyprowadzenia
 Instructions.hs - funkcje tłumczące komendy na ciągi instrukcji
 kompilator.hs - główny plik (zawiera definicję main) - wczytuje dane i zapisuje wynik
 Translation.hs - funckje tłumaczące program na kod wynikowy

Happy:
 Grammar.y - plik wejsciowy dla generatora parsera (happy)

Alex:
 Tokens.x - plik wejsciowy dla generatora leksera (alex)

wymagania:
 ghc (w wersji co najmniej 8.6.5 (chociaż może działać też dla niektórych starszych))
 happy (w wersji co najmniej 1.19.11)
 alex (w wersji co najmniej 3.2.4)

w razie problemów wszystkie wymagane programy można zainstalować na systemie Ubuntu przez: apt install haskell-platform
(jednak instalacja tylko odpowiedniej wersji ghc powinna wystarczyć)

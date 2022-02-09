import Tokens
import Grammar.Data
import Grammar.Parse
import Variables.Correctness
import Intermediate.Data
import Intermediate.Translation
--
import Translation.Code
import Translation.Translation
import System.IO
import System.Environment
--import qualified Data.Map as Map

main = do
--         args <- getArgs
--         handleSource <- openFile (args !! 0) ReadMode
--         handleMachCode <- openFile (args !! 1) WriteMode
         hSetEncoding stdin utf8
         hSetEncoding stdout utf8
         contents <- getContents
         printOrError $ compile $ parse $ alexScanTokens contents

-- zwraca albo blad albo skompilowany program
compile :: Program -> Either String [InterInstr]
compile prog = do
                 checkCorrectness prog
                 return $ getInterProg prog

-- wypisuje wiadomosc o bledzie lub zapisuje program do pliku
printOrError :: Either String [InterInstr] -> IO ()
printOrError (Left msg) = putStrLn ("Błąd: " ++ msg)
printOrError (Right inter) = putStrLn $ unlines $ map show inter

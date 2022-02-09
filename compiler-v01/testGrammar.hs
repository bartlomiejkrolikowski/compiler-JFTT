import Tokens
import Grammar.Data
import Grammar.Parse
import Translation.Translation
import Variables.Correctness
import System.IO
import qualified Data.Map as Map

main = do
         hSetEncoding stdin utf8
         hSetEncoding stdout utf8
         contents <- getContents
         let prog = parse $ alexScanTokens contents
         printOrError (checkCorrectness prog) prog
--         let code@(machCode,_) = translateProgram prog
--         putStrLn $ show code
--         putStr $ unlines $ map show machCode

-- zwraca albo blad albo skompilowany program
--compile :: Program -> Either String Code
--compile prog = do
--                 checkCorrectness prog
--                 return $ fst $ translateProgram prog

-- wypisuje wiadomosc o bledzie lub program
printOrError :: Either String () -> Program -> IO ()
printOrError (Left msg) _ = putStrLn ("Błąd: " ++ msg)
printOrError (Right ()) prog = putStrLn $ show prog

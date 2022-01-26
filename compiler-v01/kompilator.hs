import Tokens
import Grammar.Data
import Grammar.Parse
import Translation.Code
import Translation.Translation
import System.IO
import System.Environment
--import qualified Data.Map as Map

main = do
         args <- getArgs
         handleSource <- openFile (args !! 0) ReadMode
         handleMachCode <- openFile (args !! 1) WriteMode
         hSetEncoding handleSource utf8
         hSetEncoding handleMachCode utf8
         contents <- hGetContents handleSource
         saveOrError handleMachCode $ compile $ parse $ alexScanTokens contents
         hClose handleSource
         hClose handleMachCode

checkCorrectness = undefined

-- zwraca albo blad albo skompilowany program
compile :: Program -> Either String Code
compile prog = do
                 checkCorrectness prog
                 return $ fst $ translateProgram prog

-- wypisuje wiadomosc o bledzie lub zapisuje program do pliku
saveOrError :: Handle -> Either String Code -> IO ()
saveOrError _ (Left msg) = putStrLn ("Błąd: " ++ msg)
saveOrError h (Right machCode) = hPutStr h $ unlines $ map show machCode

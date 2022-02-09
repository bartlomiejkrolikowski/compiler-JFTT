import Tokens
import Grammar.Parse
import Translation.Translation
import System.IO
import qualified Data.Map as Map

main = do
         hSetEncoding stdin utf8
         hSetEncoding stdout utf8
         contents <- getContents
         let prog = parse $ alexScanTokens contents
         putStrLn $ show prog
         let code@(machCode,_) = translateProgram prog
         putStrLn $ show code
         putStr $ unlines $ map show machCode

import Tokens
import GrammarTree
import Grammar
import Translation
import System.IO
import System.Environment
import qualified Data.Map as Map

main = do
         args <- getArgs
         handleSource <- openFile (args !! 0) ReadMode
         handleMachCode <- openFile (args !! 1) WriteMode
         hSetEncoding handleSource utf8
         hSetEncoding handleMachCode utf8
         contents <- hGetContents handleSource
         let (machCode,_) = translateProgram $ parse $ alexScanTokens contents
         hPutStr handleMachCode $ unlines $ map show machCode
         hClose handleSource
         hClose handleMachCode

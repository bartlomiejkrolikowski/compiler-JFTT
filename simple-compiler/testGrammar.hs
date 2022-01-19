import Tokens
import GrammarTree
import Grammar
import System.IO
import qualified Data.Map as Map

main = do
         hSetEncoding stdin utf8
         hSetEncoding stdout utf8
         getContents >>= putStrLn . show . parse . alexScanTokens

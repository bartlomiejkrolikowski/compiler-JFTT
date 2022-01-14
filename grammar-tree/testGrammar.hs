import Tokens
import GrammarTree
import Grammar
import System.IO

main = getContents >>= putStrLn . show . parse . alexScanTokens

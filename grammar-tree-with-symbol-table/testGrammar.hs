import Tokens
import GrammarTree
import Grammar
import System.IO
import qualified Data.Map as Map

main = getContents >>= putStrLn . show . parse . alexScanTokens

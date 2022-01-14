import Tokens
import Grammar
import System.IO

main = getContents >>= putStrLn . show . parse . alexScanTokens

module Main (main) where

import Lib
import Lexer
import Parser
import System.Environment
import System.IO

main :: IO ()
main = do
  (filename : _) <- getArgs
  text <- readFile filename
  let tree = parseTTL $ alexScanTokens text
  print tree

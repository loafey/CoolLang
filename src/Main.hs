module Main where

import           Lang.Par   (myLexer, pExpr)
import           Lang.Print (printTree)
import           System.IO  (IOMode (ReadMode), hGetContents, openFile)

main :: IO ()
main = do
    file <- openFile "test/test.cl" ReadMode
    fileContent <- hGetContents file
    case pExpr . myLexer $ fileContent of
        Left ok -> putStrLn $ printTree ok
        Right e -> print e

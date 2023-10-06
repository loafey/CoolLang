module Main where

import           Control.Monad      (when)
import           Lang.Par           (myLexer, pExpr)
import           Lang.Print         (printTree)
import           System.Environment (getArgs)
import           System.IO          (IOMode (ReadMode), hGetContents, openFile)

main :: IO ()
main = do
    args <- getArgs
    when (null args) (error "No file supplied!")
    let filePath = head args

    file <- openFile filePath ReadMode
    fileContent <- hGetContents file
    case pExpr . myLexer $ fileContent of
        Left ok -> putStrLn $ printTree ok
        Right e -> print e

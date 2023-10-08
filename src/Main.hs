module Main where

import           Control.Monad           (when)
import           Error
import           Interpreter.Interpreter (interpret)
import           Lang.Par                (myLexer, pProgram)
import           Lang.Print              (printTree)
import           Renamer.Renamer         (rename)
import           System.Environment      (getArgs)
import           System.IO               (IOMode (ReadMode), hGetContents,
                                          openFile)

main :: IO ()
main = do
    args <- getArgs
    when (null args) (error "No file supplied!")
    let filePath = head args

    file <- openFile filePath ReadMode
    fileContent <- hGetContents file
    case pProgram . myLexer $ fileContent of
        Left e   -> print e
        Right ok -> do
            putStrLn " -- RAW -- "
            print ok
            putStrLn " -- PRETTY -- "
            putStrLn (printTree ok)
            case rename ok of
                Left e -> putStrLn $ showErr e
                Right ok -> do
                    putStrLn " -- RENAMED -- "
                    putStrLn (printTree ok)
                    putStrLn " -- OUTPUT -- "
                    interpret ok

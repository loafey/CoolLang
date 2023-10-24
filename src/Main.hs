{-# OPTIONS_GHC -Wno-name-shadowing #-}
module Main where

import           Control.Monad           (when)
import           Error
import           Lang.Par                (myLexer, pProgram)
import           Lang.Print              (printTree)
import           Renamer.Renamer
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
            case rename mempty Fresh ok of
                Left e -> putStrLn $ showErr e
                Right ok -> do
                    putStrLn " -- RENAMED -- "
                    putStrLn (printTree ok)

module Interpreter.Interpreter where

import           Lang.Abs

interpret :: Program -> IO ()
interpret p = do
    print ()

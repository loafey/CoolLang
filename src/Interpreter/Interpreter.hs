{-# LANGUAGE OverloadedStrings #-}

module Interpreter.Interpreter where

import           Control.Monad.Except     (ExceptT, MonadError (throwError),
                                           MonadIO (liftIO), runExceptT)
import           Control.Monad.State.Lazy (StateT, evalStateT, gets)
import           Data.Map                 (Map, fromList, lookup)
import           Data.Map.Lazy            ((!))
import           Util                     (whenErr)
import           Lang.Abs                 (BNFC'Position, Bind, Bind' (Bind),
                                           Def, Def' (DBind), Expr, Expr' (..),
                                           Ident, Lit, Program,
                                           Program' (Program))

interpret :: Program -> IO ()
interpret p = do
    let state = InterpreterState $ getBinds p
    res <- runExceptT $ evalStateT actuallyInterpret state
    whenErr res putStrLn

getBinds :: Program -> Map Ident Expr
getBinds (Program _ program) = fromList $ map (\(Bind _ y z) -> (y, z)) $ filterBinds program

filterBinds :: [Def] -> [Bind]
filterBinds binds = [a | DBind _ a <- binds]

newtype InterpreterState = InterpreterState {
        binds :: Map Ident Expr
    }

type CompilerState a = StateT InterpreterState (ExceptT String IO) a

data Value
    = VAbsoluteUnit
    | VLit Lit
    | VIdent Ident deriving Show


actuallyInterpret :: CompilerState Value
actuallyInterpret = do
    binds <- gets binds
    case Data.Map.lookup "main" binds of
        Just main -> evalExpr main
        Nothing   -> throwError "No main!"

evalExpr :: Expr -> CompilerState Value
evalExpr e = do
    case e of
        EPat _ i     -> return $ VIdent i
        ELit p l     -> return $ VLit l
        EApp p l r   -> apply p l r
        ELet p i e s -> undefined
        ELam p e s   -> undefined
        ECase p e s  -> undefined

apply :: BNFC'Position -> Expr -> Expr -> CompilerState Value
apply p e1 e2 = do
    func <- evalExpr e1
    case func of
        VIdent "print" -> do
            arg <- evalExpr e2
            liftIO $ print arg
            return  VAbsoluteUnit

        VIdent i -> do
            binds <- gets binds
            let e = binds ! i
            arg <- evalExpr e2
            evalExpr e
        _ -> throwError ("Tried to apply on a non-ident/lambda at " ++ show p)

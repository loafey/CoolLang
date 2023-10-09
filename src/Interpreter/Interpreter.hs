{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Interpreter.Interpreter where
import           Control.Monad.Except     (ExceptT, MonadError (throwError),
                                           runExceptT)
import           Control.Monad.IO.Class   (liftIO)
import           Control.Monad.State.Lazy (StateT, evalStateT, gets)
import           Data.Map                 (Map, (!))
import qualified Data.Map                 as Map
import           Data.Maybe               (fromMaybe)
import           Lang.Abs                 (BNFC'Position, Bind, Bind' (Bind),
                                           Def, Def' (DBind), Expr, Expr' (..),
                                           Ident (Ident), Lit,
                                           Lit' (LChar, LInt, LString), Program,
                                           Program' (Program))
import           Prelude                  hiding (lookup)
import           Util                     (todo, whenErr, whenOk)

interpret :: Program -> IO ()
interpret p = do
    let state = InterpreterState $ getBinds p
    res <- runExceptT $ evalStateT actuallyInterpret state
    whenErr res putStrLn
    whenOk res print

getBinds :: Program -> Map Ident Expr
getBinds (Program _ program) = Map.fromList $ map (\(Bind _ y z) -> (y, z)) $ filterBinds program

filterBinds :: [Def] -> [Bind]
filterBinds binds = [a | DBind _ a <- binds]

newtype InterpreterState = InterpreterState {
        binds :: Map Ident Expr
    }

type CompilerState a = StateT InterpreterState (ExceptT String IO) a

newtype Context = Context {
        named :: Map Ident Value
    } deriving Show

emptyContext :: Context
emptyContext = Context Map.empty

lookup :: Ident -> Context -> Maybe Value
lookup i (Context n) = Map.lookup i n

insert :: Ident -> Value -> Context -> Context
insert k v (Context n) = Context (Map.insert k v n)

contains :: Ident -> Context -> Bool
contains k (Context n) = Map.member k n

data Value
    = VAbsoluteUnit
    | VLit Lit
    | VIdent Ident
    | VLam Context Ident Expr
instance Show Value where
    show :: Value -> String
    show VAbsoluteUnit            = "[]"
    show (VLit (LString _ s))     = s
    show (VLit (LChar _ c))       = show c
    show (VLit (LInt _ i))        = show i
    show (VIdent (Ident ident))   = ident
    show (VLam _ (Ident ident) e) = "\\" <> ident <> " -> " <> show e


actuallyInterpret :: CompilerState Value
actuallyInterpret = do
    binds <- gets binds
    case Map.lookup "main" binds of
        Just main -> evalExpr emptyContext main
        Nothing   -> throwError "No main!"

evalExpr :: Context -> Expr -> CompilerState Value
evalExpr c = \case
    EPat _ i        -> return $ fromMaybe (VIdent i) (lookup i c)
    ELit _ l        -> return $ VLit l
    EApp p l r      -> evalApply c p l r
    ELet _ i e s    -> evalLet c i e s
    ELam _ i s      -> return $ VLam c i s
    ECase  {}       -> todo
    EAppExplicit {} -> error "Should not exist"

evalLet :: Context -> Ident -> Expr -> Expr -> CompilerState Value
evalLet c i e s = do
    val <- evalExpr c e
    let c' = insert i val c
    evalExpr c' s

evalApply :: Context -> BNFC'Position -> Expr -> Expr -> CompilerState Value
evalApply c p e1 e2 = evalExpr c e1 >>= \case
    VIdent "print" -> do
        arg <- evalExpr c e2
        liftIO $ print arg
        return VAbsoluteUnit

    VIdent i -> do
        binds <- gets binds
        let e = binds ! i
        evalApply c p e e2

    VLam c i e -> do
        arg <- evalExpr c e2
        let c' = insert i arg c
        evalExpr c' e

    _ -> throwError ("Tried to apply on a non-ident/lambda at " ++ show p)

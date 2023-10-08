{-# LANGUAGE OverloadedStrings #-}

module Interpreter.Interpreter where
import           Control.Monad.Except     (ExceptT, MonadError (throwError),
                                           runExceptT)
import           Control.Monad.IO.Class   (liftIO)
import           Control.Monad.State.Lazy (StateT, evalStateT, gets)
import           Data.Map                 (Map, (!))
import qualified Data.Map                 as Map
import           Lang.Abs                 (BNFC'Position, Bind, Bind' (Bind),
                                           Def, Def' (DBind), Expr, Expr' (..),
                                           Ident (Ident), Lit,
                                           Lit' (LChar, LInt, LString), Program,
                                           Program' (Program))
import           Prelude                  hiding (lookup)
import           Util                     (whenErr, whenOk)

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
data Context = Context {
        named :: Map Ident Value,
        stack :: [Value]
    }
emptyContext :: Context
emptyContext = Context Map.empty []
insert :: Value -> Context -> Context
insert v (Context n s) =  Context n (v:s)
pop :: Context -> Maybe (Value, Context)
pop (Context n (s:ss)) = Just (s, Context n ss)
pop _                  = Nothing
lookup :: Ident -> Context -> Maybe Value
lookup i (Context n _) = Map.lookup i n
insertNamed :: Ident -> Value -> Context -> Context
insertNamed k v (Context n s) = Context (Map.insert k v n) s

data Value
    = VAbsoluteUnit
    | VLit Lit
    | VIdent Ident
    | VLam Ident Expr
instance Show Value where
  show :: Value -> String
  show VAbsoluteUnit          = "[]"
  show (VLit (LString _ s))   = s
  show (VLit (LChar _ c))     = show c
  show (VLit (LInt _ i))      = show i
  show (VIdent (Ident ident)) = ident
  show (VLam (Ident ident) e) = "\\" <> ident <> " -> " <> show e


actuallyInterpret :: CompilerState Value
actuallyInterpret = do
    binds <- gets binds
    case Map.lookup "main" binds of
        Just main -> evalExpr emptyContext main
        Nothing   -> throwError "No main!"

evalExpr :: Context -> Expr -> CompilerState Value
evalExpr c e = do
    case e of
        EPat _ i        -> return $ case lookup i c of
            Just v -> v
            _      ->  VIdent i
        ELit p l        -> return $ VLit l
        EApp p l r      -> apply c p l r
        ELet _ i e s    -> evalLet c i e s
        ELam _ e s      -> return $ VLam e s
        ECase p e s     -> undefined
        EAppExplicit {} -> error "Should not exist"

evalLet :: Context -> Ident -> Expr -> Expr -> CompilerState Value
evalLet c i e s = do
    val <- evalExpr c e
    let c' = insertNamed i val c
    evalExpr c' s

apply :: Context -> BNFC'Position -> Expr -> Expr -> CompilerState Value
apply c p e1 e2 = do
    func <- evalExpr c e1
    case func of
        VIdent "print" -> do
            arg <- evalExpr c e2
            liftIO $ print arg
            return  VAbsoluteUnit

        VIdent i -> do
            binds <- gets binds
            let e = binds ! i
            arg <- evalExpr c e2
            evalExpr (insert arg c) e

        VLam i e -> do
            arg <- evalExpr c e2
            let c' = insertNamed i arg c
            evalExpr c' e

        _ -> throwError ("Tried to apply on a non-ident/lambda at " ++ show p)

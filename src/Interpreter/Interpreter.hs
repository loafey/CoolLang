{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Interpreter.Interpreter where
import           Control.Monad            (filterM, void, zipWithM)
import           Control.Monad.Except     (ExceptT, MonadError (throwError),
                                           runExceptT)
import           Control.Monad.IO.Class   (liftIO)
import           Control.Monad.State.Lazy (StateT, evalStateT, gets)
import           Data.Map                 (Map, (!))
import qualified Data.Map                 as Map
import           Lang.Abs                 (BNFC'Position, Bind' (Bind), Branch,
                                           Branch' (Branch), Data' (Data),
                                           Def' (DBind, DData), Expr,
                                           Expr' (..), Ident (Ident),
                                           Inj' (Inj), Lit,
                                           Lit' (LChar, LInt, LString), Pattern,
                                           Pattern' (PCatch, PInj, PLit, PVar),
                                           Program, Program' (Program), Type)
import           Lang.Print               (printTree)
import           Prelude                  hiding (lookup)
import           Util                     (whenErr, whenOk)

interpret :: Program -> IO ()
interpret p = do
    let (binds, dtypes) = getBinds p
    print dtypes
    let state = InterpreterState binds dtypes
    putStrLn " -- Expanded tree --"
    mapM_ ((putStrLn . printTree) . uncurry (Bind Nothing)) (Map.toList binds)
    -- putStrLn " -- Expanded tree raw --"
    -- mapM_ (print . uncurry (Bind Nothing)) (Map.toList binds)
    res <- runExceptT $ evalStateT actuallyInterpret state
    whenErr res putStrLn
    whenOk res print

getBinds :: Program -> (Map Ident Expr, Map Ident (Int, [Type]))
getBinds (Program _ program) = (filterBinds, filterData)
    where
        filterBinds :: Map Ident Expr
        filterBinds = Map.fromList
            [(\(Bind _ y z) -> (y, z)) a | DBind _ a <- program]

        filterData :: Map Ident (Int, [Type])
        filterData = Map.fromList
            $ map (\(ind, Inj _ i t) -> (i, (ind, t)))
            $ concat
            $ [zip [0..] a | DData _ (Data _ _ a) <- program]

data InterpreterState = InterpreterState {
        binds :: Map Ident Expr,
        types :: Map Ident (Int, [Type])
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
    | VData Int [Value]
instance Show Value where
    show :: Value -> String
    show VAbsoluteUnit            = "[]"
    show (VLit (LString _ s))     = s
    show (VLit (LChar _ c))       = show c
    show (VLit (LInt _ i))        = show i
    show (VIdent (Ident ident))   = ident
    show (VLam _ (Ident ident) e) = "\\" <> ident <> " -> " <> show e
    show (VData i xs)             = "(" <> show i <> ": " <> show xs <> ")"


actuallyInterpret :: CompilerState Value
actuallyInterpret = do
    binds <- gets binds
    case Map.lookup "main" binds of
        Just main -> evalExpr emptyContext main
        Nothing   -> throwError "No main!"

evalExpr :: Context -> Expr -> CompilerState Value
evalExpr c = \case
    ELit _ l        -> return $ VLit l
    EApp p l r      -> evalApply c p l r
    ELet _ i e s    -> evalLet c i e s
    ELam _ i s      -> return $ VLam c i s
    ECase p e b     -> evalCase c p e b
    EAppExplicit {} -> error "Should not exist"
    -- This is horrible, I know
    EPat _ i        -> case lookup i c of
        Just v  -> return v
        Nothing -> do
            types <- gets types
            return $ case Map.lookup i types of
                Just (ind, x) -> if null x then VData ind [] else VIdent i
                Nothing       -> VIdent i

valPattern :: Pattern -> Value -> CompilerState Bool
valPattern (PLit _ l) v = case v of
    VLit l' -> return $ void l == void l'
    _       -> error "whoopsie"
valPattern (PCatch _) _     = return True
valPattern (PVar _ _) _     = undefined
valPattern (PInj _ i p) v = case v of
    VData ind p' -> do
        types <- gets types
        let (ind',_) = types ! i
        if ind /= ind'
            then return False
        else
            or <$> zipWithM valPattern p p'
    _ -> error "whoopsie"

evalCase :: Context -> BNFC'Position -> Expr -> [Branch] -> CompilerState Value
evalCase c p e br = do
    e <- evalExpr c e
    bools <- filterM (\(Branch _ p _)  -> valPattern p e) br
    case bools of
        ((Branch _ _ res):_) -> evalExpr c res
        _     -> throwError $ "Non-exhaustive case at: " <> show p

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
        case Map.lookup i binds of
            Just e -> evalApply c p e e2
            Nothing -> do
                types <- gets types
                e2 <- evalExpr c e2
                let (ind, _) = types ! i
                return $ VData ind [e2]


    VLam c i e -> do
        arg <- evalExpr c e2
        let c' = insert i arg c
        evalExpr c' e

    _ -> throwError ("Tried to apply on a non-ident/lambda at " ++ show p)

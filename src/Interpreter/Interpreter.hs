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
                                           Data' (Data), Def,
                                           Def' (DBind, DData), Expr,
                                           Expr' (..), Ident (Ident),
                                           Inj' (Inj), Lit,
                                           Lit' (LChar, LInt, LString), Program,
                                           Program' (Program), Type)
import           Lang.Print               (printTree)
import           Prelude                  hiding (lookup)
import           Util                     (todo, whenErr, whenOk)

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

getBinds :: Program -> (Map Ident Expr, Map Ident [Type])
getBinds (Program _ program) = (filterBinds, filterData)
    where
        filterBinds :: Map Ident Expr
        filterBinds = Map.fromList
            [(\(Bind _ y z) -> (y, z)) a | DBind _ a <- program]

        filterData :: Map Ident [Type]
        filterData = Map.fromList
            $ map (\(Inj _ i t) -> (i, t))
            $ concat [(\ (Data _ _ z) -> z) a | DData _ a <- program]

data InterpreterState = InterpreterState {
        binds :: Map Ident Expr,
        types :: Map Ident [Type]
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
    | VData [Value]
instance Show Value where
    show :: Value -> String
    show VAbsoluteUnit            = "[]"
    show (VLit (LString _ s))     = s
    show (VLit (LChar _ c))       = show c
    show (VLit (LInt _ i))        = show i
    show (VIdent (Ident ident))   = ident
    show (VLam _ (Ident ident) e) = "\\" <> ident <> " -> " <> show e
    show (VData xs)               = show xs


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
    ECase  {}       -> todo
    EAppExplicit {} -> error "Should not exist"
    -- This is horrible, I know
    EPat _ i        -> case lookup i c of
        Just v  -> return v
        Nothing -> do
            types <- gets types
            return $ case Map.lookup i types of
                Just x  -> if null x then VData [] else VIdent i
                Nothing -> VIdent i

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
                -- Crash if the constructor does not exist, but I am
                -- not sure if we need it?
                seq (types ! i) $
                    return $ VData [e2]


    VLam c i e -> do
        arg <- evalExpr c e2
        let c' = insert i arg c
        evalExpr c' e

    _ -> throwError ("Tried to apply on a non-ident/lambda at " ++ show p)

{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE LambdaCase               #-}
{-# LANGUAGE OverloadedStrings        #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Interpreter.Interpreter where
import           Control.Monad            (void)
import           Control.Monad.Except     (ExceptT, MonadError (throwError),
                                           runExceptT)
import           Control.Monad.Extra      (foldM)
import           Control.Monad.IO.Class   (liftIO)
import           Control.Monad.State.Lazy (StateT, evalStateT, gets)
import qualified Data.Bifunctor
import           Data.Int                 (Int64)
import           Data.IORef
import           Data.Map                 (Map, (!))
import qualified Data.Map                 as Map
import           Data.Void                (Void)
import           Foreign                  (Ptr, peek)
import           Foreign.C                (CString, newCString)
import           GHC.Ptr                  (FunPtr (FunPtr))
import           Lang.Abs                 (BNFC'Position, Bind' (Bind), Branch,
                                           Branch' (Branch), Data' (Data),
                                           Def' (DBind, DData), Expr,
                                           Expr' (..), Ident (Ident),
                                           Inj' (Inj), Lit,
                                           Lit' (LChar, LInt, LString), Pattern,
                                           Pattern' (PCatch, PInj, PLit, PVar),
                                           Program, Program' (Program), Type)
import           Prelude                  hiding (lookup)
import           Unsafe.Coerce            (unsafeCoerce)
import           Util                     (whenErr, whenOk)

foreign import ccall "dlopen" c_dlopen :: CString -> Int -> Ptr Void
foreign import ccall "dlsym" c_dlsym :: Ptr Void -> CString -> FunPtr (Ptr Void -> Ptr Void)
foreign import ccall "dynamic"
  mkFun :: FunPtr (Ptr Void -> Ptr Void) -> (Ptr Void -> Ptr Void)

interpret :: Program -> IO ()
interpret p = do
    let state = uncurry InterpreterState $ getBinds p
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
            $ concat [zip [0..] a | DData _ (Data _ _ a) <- program]

data InterpreterState = InterpreterState {
        binds :: Map Ident Expr,
        types :: Map Ident (Int, [Type])
    }

type IS a = StateT InterpreterState (ExceptT String IO) a

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


actuallyInterpret :: IS Value
actuallyInterpret = do
    binds <- gets binds
    case Map.lookup "main" binds of
        Just main -> evalExpr emptyContext main
        Nothing   -> throwError "No main!"

evalExpr :: Context -> Expr -> IS Value
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

valPattern :: Context -> Pattern -> Value -> IS (Context, Bool)
valPattern c (PLit _ l) v = case v of
    VLit l' -> return (c, void l == void l')
    _       -> throwError "Type error TODO fill out"
valPattern c (PCatch _) _     = return (c, True)
valPattern c (PVar _ i) v     = do
    let c' = insert i v c
    return (c', True)
valPattern c (PInj _ i p) v = case v of
    VData ind p' -> do
        types <- gets types
        let (ind',_) = types ! i
        if ind /= ind' then
            return (c, False)
        else foldM (\(c',r) (x,y) ->
                    Data.Bifunctor.second (r ||) <$> valPattern c' x y
                ) (c, False) (zip p p')
    _ -> throwError "Type error TODO fill out"

evalCase :: Context -> BNFC'Position -> Expr -> [Branch] -> IS Value
evalCase c p e br = do
    e <- evalExpr c e
    cases <- mapM (\(Branch _ p r)  -> (r,) <$> valPattern c p e) br
    let f = filter (\(_,(_,x)) -> x) cases
    case f of
        ((res, (c,_)):_) -> evalExpr c res
        _                -> throwError $ "Non-exhaustive case at: " <> show p

evalLet :: Context -> Ident -> Expr -> Expr -> IS Value
evalLet c i e s = do
    val <- evalExpr c e
    let c' = insert i val c
    evalExpr c' s

libc :: IO (IORef (Ptr Void))
{-# NOINLINE libc #-}
libc = do
    lib <- liftIO $ newCString "libc"
    newIORef $ c_dlopen lib 0
getLibC :: IO (Ptr Void)
getLibC = do
    libc <- libc
    readIORef libc
getFunc :: String -> IO a
getFunc s = do
    libc <- getLibC
    funcName <- liftIO $ newCString s
    let func = mkFun $ c_dlsym libc funcName
    pure (unsafeCoerce func :: a)



evalApply :: Context -> BNFC'Position -> Expr -> Expr -> IS Value
evalApply c p e1 e2 = evalExpr c e1 >>= \case
    VIdent "c_call" -> do
        puts <- (liftIO $ getFunc "puts") :: StateT InterpreterState (ExceptT String IO) (CString -> IO ())
        hello <- liftIO $ newCString "crongus"
        liftIO $ puts hello
        difftime <- (liftIO $ getFunc "difftime") :: StateT InterpreterState (ExceptT String IO) (Int64 -> Int64 -> Double)
        liftIO . print $ difftime 452141 54212

        return VAbsoluteUnit

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

    -- This should not type check!
    VData ind r -> do
        arg <- evalExpr c e2
        return $ VData ind (r ++ [arg])

    x -> throwError $ concat
        ["Tried to apply on a non-ident/lambda at "
        , show p
        , "["
        , show x
        , "]"]

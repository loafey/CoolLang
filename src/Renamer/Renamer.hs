{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}

module Renamer.Renamer (rename) where

import           Lang.Abs

import           Control.Monad.Except
import           Control.Monad.State
import           Data.Map             (Map)
import qualified Data.Map             as M
import           Data.Set             (Set)
import qualified Data.Set             as S
import           Error
import Debug.Trace (traceShow, trace)

-- TODO: Add data types to base types before continuing.
-- Renamer is currently completely unusble

debug :: Bool
debug = True

newtype Rn a = Rn { runRn :: ExceptT (RenameError Pos) (State Ctx) a }
    deriving (Functor, Applicative, Monad, MonadState Ctx, MonadError (RenameError Pos))

data Ctx = Ctx
    { counter    :: Int
    , varNames   :: Map Ident Ident
    , tyNames    :: Map Ident Ident -- Can't have this TVar as it also stores position
    , baseTypes  :: Set Ident
    , injections :: Set Ident
    } deriving Show

emptyCtx :: Ctx
emptyCtx = Ctx 0 mempty mempty mempty mempty

type Pos = BNFC'Position


rename :: Program -> Either (RenameError Pos) Program
rename prg
  | debug     = traceShow context prg'
  | otherwise = prg'
  where
    (prg', context) = flip runState emptyCtx
                    . runExceptT
                    . runRn
                    . rnProgram $ prg

rnProgram :: Program -> Rn Program
rnProgram (Program pos defs)
    = addDecls defs
    >> get >>= flip trace (pure ()) . ("Context before renaming: " ++) . ((++"\n") . show)
    >> Program pos <$> mapM rnDef defs

addDecls :: [Def] -> Rn ()
addDecls [] = pure ()
addDecls (x:xs) = case x of
    DBind _ bind -> addBind bind >> addDecls xs
    DData _ dat -> addData dat >> addDecls xs
    DSig _ sig -> addDecls xs -- Perhaps add signatures too

addData :: Data -> Rn ()
addData d = do
    name' <- getDataTypeName d
    modify (\ctx -> ctx { baseTypes = S.insert name' ctx.baseTypes })

addBind :: Bind -> Rn ()
addBind (Bind pos name expr)
    = modify (\ctx -> ctx { varNames = M.insert name name ctx.varNames })

rnDef :: Def -> Rn Def
rnDef = \case
    DSig pos signature -> DSig pos <$> rnSig signature
    DBind pos bind -> DBind pos <$> rnBind bind
    DData pos dat -> DData pos <$> rnData dat

rnSig :: Sig -> Rn Sig
rnSig (Sig pos name typ) = Sig pos name <$> rnType typ

rnBind :: Bind -> Rn Bind
rnBind (Bind pos name expr) = Bind pos name <$> rnExpr expr

rnData :: Data -> Rn Data
rnData (Data pos ty injections) = addTyVars ty >> Data pos ty <$> mapM rnInjection injections

rnInjection :: Inj -> Rn Inj
rnInjection (Inj pos name tys) = modify (\ctx -> ctx { injections = S.insert name ctx.injections }) >> Inj pos name <$> mapM rnType tys

rnExpr :: Expr -> Rn Expr
rnExpr = \case
    EPat pos ident -> EPat pos <$> getVarName pos ident
    ELit pos lit -> pure $ ELit pos lit
    EApp pos e1 e2 -> EApp pos <$> rnExpr e1 <*> rnExpr e2
    EAppExplicit pos e1 tyApp e2 -> EAppExplicit pos <$> rnExpr e1 <*> rnType tyApp <*> rnExpr e2
    ELet pos ident e1 e2 -> do
        name' <- newVarName ident
        ELet pos name' <$> rnExpr e1 <*> rnExpr e2
    ELam pos ident e -> do
        ident' <- newVarName ident
        e' <- rnExpr e
        pure (ELam pos ident' e')
    ECase pos e1 branches -> do
        e1' <- rnExpr e1
        branches' <- mapM rnBranch branches
        pure $ ECase pos e1' branches'

rnBranch :: Branch -> Rn Branch
rnBranch (Branch pos pat expr) = Branch pos <$> rnPattern mempty pat <*> rnExpr expr

-- Exlude vars that are constructors
rnPattern :: Set Ident -> Pattern -> Rn Pattern
rnPattern seen = \case
    PLit pos lit -> pure $ PLit pos lit
    PCatch pos   -> pure $ PCatch pos
    PVar pos name -> do
        -- TODO: Add both definitions for better error message
        when (name `S.member` seen) (throwError $ RnMultiplePatternVar pos name)
        injs <- gets injections
        if name `S.member` injs
        then pure $ PVar pos name
        else PVar pos <$> newVarName name
    PInj pos name pats -> do
        -- TODO: Add both definitions for better error message
        when (name `S.member` seen) (throwError $ RnMultiplePatternVar pos name)
        injs <- gets injections
        if name `S.member` injs
        then PInj pos name <$> mapM (rnPattern (S.insert name seen)) pats
        else PInj pos <$> newVarName name <*> mapM (rnPattern (S.insert name seen)) pats

rnType :: Type -> Rn Type
rnType = \case
    TVar pos name     -> TVar pos <$> getTVarTyName name
    TApp pos ty1 ty2  -> TApp pos <$> rnType ty1 <*> rnType ty2
    TFun pos ty1 ty2  -> TFun pos <$> rnType ty1 <*> rnType ty2
    TAll pos tvars ty -> TAll pos <$> mapM newTVarTyName tvars <*> rnType ty

newTVarTyName :: TVar -> Rn TVar
newTVarTyName (MkTVar pos ident) = MkTVar pos <$> newTyName ident

getTVarTyName :: TVar -> Rn TVar
getTVarTyName (MkTVar pos ident) = MkTVar pos <$> getTyName pos ident

newVarName :: Ident -> Rn Ident
newVarName (Ident name) = do
    i <- gets counter
    let name' = Ident $ "$" ++ show i ++ name
    modify $ \ctx -> ctx { varNames = M.insert (Ident name) name' ctx.varNames, counter = succ ctx.counter }
    pure name'

getVarName :: Pos -> Ident -> Rn Ident
getVarName pos name = gets (M.lookup name . varNames) >>= \case
    Nothing -> throwError $ RnUnboundVar pos name
    Just name' -> pure name'

newTyName :: Ident -> Rn Ident
newTyName (Ident name) = do
    i <- gets counter
    let name' = Ident $ "$" ++ show i ++ name
    modify $ \ctx -> ctx { tyNames = M.insert (Ident name) name' ctx.tyNames, counter = succ ctx.counter }
    pure name'

getTyName :: Pos -> Ident -> Rn Ident
getTyName pos name = gets (M.lookup name . tyNames) >>= \case
    Nothing -> gets (S.member name . baseTypes) >>= \case
        False -> throwError $ RnUnboundVar pos name
        True -> pure name
    Just name' -> pure name'

addTyVars :: Type -> Rn ()
addTyVars = \case
  TVar _ name -> void $ newTVarTyName name
  TApp _ ty1 ty2 -> addTyVars ty1 >> addTyVars ty2
  TAll _ tvars ty -> mapM_ newTVarTyName tvars >> addTyVars ty
  TFun _ ty1 ty2 -> addTyVars ty1 >> addTyVars ty2

getDataTypeName :: MonadError (RenameError Pos) m => Data -> m Ident
getDataTypeName (Data _ identType _) = case identType of
    TVar _ (MkTVar _ name)            -> pure name
    TApp _ (TVar _ (MkTVar _ name)) _ -> pure name
    TApp pos _ _ -> throwError $ RnBadDataType pos identType
    TAll pos _ _ -> throwError $ RnBadDataType pos identType
    TFun pos _ _ -> throwError $ RnBadDataType pos identType

rmPos :: Type -> Type
rmPos = fmap (const Nothing)

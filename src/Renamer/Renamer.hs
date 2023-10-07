{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}

module Renamer.Renamer (rename) where

import Lang.Abs

import Control.Monad.Except
import Control.Monad.State
import Data.Map (Map)
import Data.Map qualified as M
import Data.Set (Set)
import qualified Data.Set as S
import Error
import Debug.Trace (trace, traceShow)

newtype Rn a = Rn { runRn :: ExceptT (RenameError Pos) (State Ctx) a }
    deriving (Functor, Applicative, Monad, MonadState Ctx, MonadError (RenameError Pos))

data Ctx = Ctx
    { counter :: Int
    , varNames :: Map Ident Ident
    , tyNames :: Map Ident Ident -- Can't have this TVar as it also stores position
    , baseTypes :: Set Type  -- Make sure to set the position of the type to Nothing
    , injections :: Set Ident
    } deriving Show

emptyCtx :: Ctx
emptyCtx = Ctx 0 mempty mempty mempty mempty

type Pos = BNFC'Position

rename :: Program -> Either (RenameError Pos) Program
rename program = let (prg, ctx) = (runState . runExceptT . runRn . rnProgram) program emptyCtx
         in trace (show ctx) prg

rnProgram :: Program -> Rn Program
rnProgram (Program pos defs) = Program pos <$> mapM rnDef defs

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
rnData (Data pos ty injections) = Data pos <$> rnBaseType ty <*> mapM rnInjection injections

rnInjection :: Inj -> Rn Inj
rnInjection (Inj pos name tys) = modify (\ctx -> ctx { injections = S.insert name ctx.injections }) >> Inj pos name <$> mapM rnType tys

-- | Checks for a valid declaration of the data type as well
rnBaseType :: Type -> Rn Type
rnBaseType t = do
    t' <- go t
    trace ("TRACE: " ++ show t') pure ()
    modify (\ctx -> ctx { baseTypes = S.insert (rmPos t') ctx.baseTypes })
    pure t'
  where
    go :: Type -> Rn Type
    go = \case
        TVar pos name     -> TVar pos <$> newTVarTyName name
        TApp pos ty1 ty2@(TVar _ name)  -> do
            ty1' <- go ty1
            ty2' <- go ty2
            pure $ TApp pos ty1' ty2'
        -- TODO: Add the data type name that is being defined for better error message
        TApp _ _ ty2 -> throwError $ RnBadDataType (hasPosition ty2) ty2
        ty           -> throwError $ RnBadDataType (hasPosition ty) ty

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
    Nothing -> throwError $ RnUnboundVar pos name
    Just name' -> pure name'

rmPos :: Type -> Type
rmPos = fmap (const Nothing)

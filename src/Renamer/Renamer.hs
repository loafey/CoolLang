{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Renamer.Renamer (rename) where

import           Lang.Abs

import           Control.Monad.Except
import           Control.Monad.State
import           Data.Map             (Map)
import qualified Data.Map             as M
import           Data.Set             (Set)
import qualified Data.Set             as S
import           Error
import           Util                 (sortBindsLast)

type Pos = BNFC'Position

newtype Rn a = Rn { runRn :: ExceptT (RenameError Pos) (State Ctx) a }
    deriving (Functor, Applicative, Monad, MonadState Ctx, MonadError (RenameError Pos))

data Ctx = Ctx
    { counter      :: Int
    , variables    :: Map Ident Ident
    , binds        :: Set Ident
    , tyVariables  :: Map TVar TVar
    , baseTypes    :: Set TVar
    , constructors :: Set Inj
    } deriving Show

emptyCtx :: Ctx
emptyCtx = Ctx 0 mempty mempty mempty mempty mempty

run :: Rn a -> Either (RenameError Pos) a
run = flip evalState emptyCtx . runExceptT . runRn

rename :: Program -> Either (RenameError Pos) Program
rename = run . rnProgram

rnProgram :: Program -> Rn Program
rnProgram (Program pos defs) = Program pos <$> mapM rnDef (sortBindsLast defs)

rnDef :: Def -> Rn Def
rnDef = \case
    DSig pos signature -> DSig pos <$> rnSig signature
    DBind pos bind -> DBind pos <$> rnBind bind
    DData pos dat -> DData pos <$> rnData dat

rnSig :: Sig -> Rn Sig
rnSig (Sig pos name domain) = do
    cleanContext
    addBind name
    Sig pos name <$> rnDomain domain

rnBind :: Bind -> Rn Bind
rnBind (Bind pos name expr) = do
    cleanContext
    getBindName pos name
    Bind pos name <$> rnExpr expr

rnData :: Data -> Rn Data
rnData d@(Data pos domain injections) = do
    cleanContext
    name <- getDataTypeName d
    addBaseType name
    isValidData domain
    domain <- rnDomain domain
    Data pos domain <$> mapM rnInjection injections

rnInjection :: Inj -> Rn Inj
rnInjection inj@(Inj pos name tys)
    = addInjection inj
    >> Inj pos name <$> mapM rnType tys

rnDomain :: Domain -> Rn Domain
rnDomain (TEmpty pos ty) = TEmpty pos <$> rnType ty
rnDomain (TAll pos tvars ty) = do
    tvars <- mapM newTypeName tvars
    TAll pos tvars <$> rnType ty

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

rnPattern :: Set Ident -> Pattern -> Rn Pattern
rnPattern seen = \case
    PLit pos lit -> pure $ PLit pos lit
    PCatch pos   -> pure $ PCatch pos
    PVar pos name -> do
        -- TODO: Add both definitions for better error message
        when (name `S.member` seen) (throwError $ RnMultiplePatternVar pos name)
        b <- isConstructor name
        if b
        then pure $ PVar pos name
        else PVar pos <$> newVarName name
    PInj pos name pats -> do
        -- TODO: Add both definitions for better error message
        when (name `S.member` seen) (throwError $ RnMultiplePatternVar pos name)
        b <- isConstructor name
        if b
        then PInj pos name <$> mapM (rnPattern (S.insert name seen)) pats
        else PInj pos <$> newVarName name <*> mapM (rnPattern (S.insert name seen)) pats

rnType :: Type -> Rn Type
rnType ty = case ty of
    TVar pos1 name -> do
        let name' = rmPosTVar name
        atoms <- gets baseTypes
        tyvars <- gets tyVariables
        if S.member name' atoms
        then pure ty
        else case M.lookup name' tyvars of
            Nothing   -> throwError $ RnUnboundTVar pos1 name
            Just name -> pure $ TVar pos1 name
    TApp pos ty1 ty2  -> TApp pos <$> rnType ty1 <*> rnType ty2
    TFun pos ty1 ty2  -> TFun pos <$> rnType ty1 <*> rnType ty2

addBind :: MonadState Ctx m => Ident -> m ()
addBind name = modify (\ctx -> ctx { binds = S.insert name ctx.binds })

getBindName :: (MonadError (RenameError Pos) m, MonadState Ctx m) => Pos -> Ident -> m ()
getBindName pos name = gets (S.member name . binds) >>= \case
    False -> throwError $ RnUnboundVar pos name
    True -> pure ()

addVariable :: MonadState Ctx m => Ident -> Ident -> m ()
addVariable old new = modify (\ctx -> ctx { variables = M.insert old new ctx.variables })

addTypeVariable :: MonadState Ctx m => TVar -> TVar -> m ()
addTypeVariable old new = modify (\ctx -> ctx { tyVariables = M.insert old new ctx.tyVariables })

newVarName :: MonadState Ctx m => Ident -> m Ident
newVarName (Ident name) = do
    i <- gets counter
    let name' = Ident $ "$" ++ show i ++ name
    modify (\ctx -> ctx { counter = succ ctx.counter
                        , variables = M.insert (Ident name) name' ctx.variables })
    pure name'

getVarName :: Pos -> Ident -> Rn Ident
getVarName pos name = gets (M.lookup name . variables) >>= \case
    Nothing -> throwError $ RnUnboundVar pos name
    Just name' -> pure name'

newTypeName :: (MonadState Ctx m) => TVar -> m TVar
newTypeName tvar@(MkTVar _ (Ident name)) = do
    i <- gets counter
    let name' = MkTVar Nothing $ Ident $ "$" ++ show i ++ name
    modify (\ctx -> ctx { counter = succ ctx.counter
                        , tyVariables = M.insert (rmPosTVar tvar) name' ctx.tyVariables })
    pure name'

getTypeName :: TVar -> Rn TVar
getTypeName name =
    let name' = rmPosTVar name
    in gets (S.member name' . baseTypes) >>= \case
           False -> gets (M.lookup name' . tyVariables) >>= \case
               Nothing -> throwError $ RnUnboundTVar (hasPosition name) name
               Just name -> pure name
           True -> pure name

addBaseType :: MonadState Ctx m => TVar -> m ()
addBaseType name = modify (\ctx -> ctx { baseTypes = S.insert (rmPosTVar name) ctx.baseTypes })

getDataTypeName :: MonadError (RenameError Pos) m => Data -> m TVar
getDataTypeName (Data _ domain _) = case domain of
    TEmpty _ ty -> go ty
    TAll _ _ ty -> go ty
  where
    go ty = case ty of
        TVar _ name            -> pure name
        TApp _ (TVar _ name) _ -> pure name
        TApp pos _ _           -> throwError $ RnBadDataType pos ty
        TFun pos _ _           -> throwError $ RnBadDataType pos ty

getTVars :: Type -> [TVar]
getTVars t = case t of
    TVar _ tvar    -> [tvar]
    TApp _ ty1 ty2 -> getTVars ty1 ++ getTVars ty2
    TFun _ ty1 ty2 -> getTVars ty1 ++ getTVars ty2

addInjection :: MonadState Ctx m => Inj -> m ()
addInjection inj = modify (\ctx -> ctx { constructors = S.insert inj ctx.constructors })

isConstructor :: Ident -> Rn Bool
isConstructor ident = do
    injs <- gets constructors
    let names = S.map getName injs
    pure (ident `S.member` names)
  where
    getName (Inj _ name _) = name

isValidData :: Domain -> Rn ()
isValidData domain = case domain of
    TEmpty _ ty -> go ty
    TAll _ _ ty -> go ty
  where
    go :: Type -> Rn ()
    go ty = case ty of
        TVar _ _             -> pure ()
        TApp _ (TVar _ _) ty -> go ty
        _                    -> throwError $ RnBadDataType (hasPosition ty) ty

rmPosType :: Type -> Type
rmPosType = fmap (const Nothing)

rmPosTVar :: TVar -> TVar
rmPosTVar = fmap (const Nothing)

cleanContext :: Rn ()
cleanContext = modify (\ctx -> ctx { tyVariables = mempty })

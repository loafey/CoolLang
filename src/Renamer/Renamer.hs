{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings   #-}

module Renamer.Renamer (rename, renameGetNames, RenameDirection(..)) where

import           Lang.Abs

import           Control.Monad.Except
import           Control.Monad.State
import           Data.Bifunctor       (second)
import           Data.Composition
import           Data.Map             (Map)
import qualified Data.Map             as M
import           Data.Set             (Set)
import qualified Data.Set             as S
import           Error
import           Util                 (sequenceFst, sortBindsLast)

type Pos = BNFC'Position

newtype Rn a = Rn { runRn :: ExceptT (RenameError Pos) (State Ctx) a }
    deriving ( Functor
             , Applicative
             , Monad
             , MonadState Ctx
             , MonadError (RenameError Pos))

data Ctx = Ctx
    { counter       :: Int
    , variables     :: Map Ident Ident
    , binds         :: Set Ident
    , tyVariables   :: Map TVar TVar
    , baseTypes     :: Set TVar
    , constructors  :: Set Ident
    , originalNames :: Map Ident Ident
    , renameDir     :: RenameDirection
    } deriving Show

createEmptyCtx :: Map Ident Ident -> RenameDirection -> Ctx
createEmptyCtx = Ctx 0 mempty mempty mempty mempty mempty

data RenameDirection = Fresh | Old
    deriving Show

renameGetNames :: Map Ident Ident
               -> RenameDirection
               -> Program
               -> Either (RenameError Pos) (Program, Map Ident Ident)
renameGetNames names rd prg
    = sequenceFst
    . second originalNames
    . flip runState (createEmptyCtx names rd)
    . runExceptT
    . runRn
    $ rnProgram prg

rename :: Map Ident Ident
       -> RenameDirection
       -> Program
       -> Either (RenameError Pos) Program
rename = fmap fst .:. renameGetNames

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
rnData d@(Data pos domain0 injections) = do
    cleanContext
    (name, tvars) <- isValidData domain0
    -- Find which two names clash and add both to error constructor
    noOverlappingVars tvars
    when (rmPosTVar name `elem` map rmPosTVar tvars) $ do
         throwError $ RnNameTypeVariableClash (hasPosition name) d name
    addBaseType name
    domain1 <- rnDomain domain0
    Data pos domain1 <$> mapM rnInjection injections

rnInjection :: Inj -> Rn Inj
rnInjection inj@(Inj pos name tys)
    = addInjection inj
    >> Inj pos name <$> mapM rnType tys

rnDomain :: Domain -> Rn Domain
rnDomain (TEmpty pos ty) = TEmpty pos <$> rnType ty
rnDomain (TAll pos tvars0 ty) = do
    tvars1 <- mapM newTypeName tvars0
    TAll pos tvars1 <$> rnType ty

rnExpr :: Expr -> Rn Expr
rnExpr = \case
    EPat pos ident -> EPat pos <$> getVarName pos ident
    ELit pos lit -> pure $ ELit pos lit
    EApp pos e1 e2 -> EApp pos <$> rnExpr e1 <*> rnExpr e2
    EAppExplicit pos e1 tyApp e2
        -> EAppExplicit pos <$> rnExpr e1 <*> rnType tyApp <*> rnExpr e2
    ELet pos ident e1 e2 -> do
        name' <- newVarName ident
        ELet pos name' <$> rnExpr e1 <*> rnExpr e2
    ECase pos e1 branches -> do
        e1' <- rnExpr e1
        branches' <- mapM rnBranch branches
        pure $ ECase pos e1' branches'
    ELam pos ident e -> do
        ident' <- newVarName ident
        e' <- rnExpr e
        pure (ELam pos ident' e')
    EAnn pos expr ty -> EAnn pos <$> rnExpr expr <*> rnType ty

rnBranch :: Branch -> Rn Branch
rnBranch (Branch pos pat expr)
    = Branch pos <$> rnPattern mempty pat <*> rnExpr expr

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
        else PInj pos <$> newVarName name
                      <*> mapM (rnPattern (S.insert name seen)) pats

rnType :: Type -> Rn Type
rnType ty = case ty of
    TVar pos1 name0 -> do
        let name' = rmPosTVar name0
        isBaseType <- gets (S.member name' . baseTypes)
        tyvars <- gets tyVariables
        if isBaseType
        then pure ty
        else
            gets renameDir >>= \case
                Fresh -> case M.lookup name' tyvars of
                    Nothing    -> throwError $ RnUnboundTVar pos1 name0
                    Just name1 -> pure $ TVar pos1 name1
                Old -> do
                    ogs <- gets originalNames
                    let f (MkTVar _ x) = x
                    let g (Ident x) = x
                    case M.lookup (f name') ogs of
                        Nothing    -> (crash . g . f) name'
                        Just name1 -> pure $ TVar pos1 (MkTVar Nothing name1)
    TApp pos ty1 ty2  -> TApp pos <$> rnType ty1 <*> rnType ty2
    TFun pos ty1 ty2  -> TFun pos <$> rnType ty1 <*> rnType ty2

addBind :: MonadState Ctx m => Ident -> m ()
addBind name = modify (\ctx -> ctx {
    binds = S.insert name ctx.binds
})

getBindName :: (MonadError (RenameError Pos) m, MonadState Ctx m)
            => Pos -> Ident -> m ()
getBindName pos name = gets (S.member name . binds) >>= \case
    False -> throwError $ RnUnboundVar pos name
    True -> pure ()

newVarName :: MonadState Ctx m => Ident -> m Ident
newVarName (Ident name) = do
    i <- gets counter
    let name' = Ident $ "$" ++ show i
    modify (\ctx -> ctx {
        counter = succ ctx.counter,
        variables = M.insert (Ident name) name' ctx.variables,
        originalNames = M.insert name' (Ident name) ctx.originalNames
    })
    pure name'

getVarName :: Pos -> Ident -> Rn Ident
getVarName pos name = gets renameDir >>= go
  where
    go Fresh = gets (S.member name . constructors) >>= \case
        False -> gets (M.lookup name . variables) >>= \case
            Nothing -> throwError $ RnUnboundVar pos name
            Just name' -> pure name'
        True -> pure name
    go Old = gets (S.member name . constructors) >>= \case
        False -> gets (M.lookup name . originalNames) >>= \case
            Nothing -> crash $ unwrapIdent name
            Just name' -> pure name'
        True -> pure name


newTypeName :: (MonadState Ctx m) => TVar -> m TVar
newTypeName tvar@(MkTVar _ (Ident name)) = do
    i <- gets counter
    let name' = MkTVar Nothing $ Ident $ "$" ++ show i ++ name
    modify (\ctx -> ctx {
        counter = succ ctx.counter ,
        tyVariables = M.insert (rmPosTVar tvar) name' ctx.tyVariables
    })
    pure name'

addBaseType :: MonadState Ctx m => TVar -> m ()
addBaseType name = modify (\ctx -> ctx {
    baseTypes = S.insert (rmPosTVar name) ctx.baseTypes
})

addInjection :: MonadState Ctx m => Inj -> m ()
addInjection (Inj _ name _) = modify (\ctx -> ctx {
    constructors = S.insert name ctx.constructors
})

isConstructor :: Ident -> Rn Bool
isConstructor ident = gets (S.member ident . constructors)

isValidData :: (MonadError (RenameError Pos) m) => Domain -> m (TVar, [TVar])
isValidData domain = case domain of
    TEmpty _ ty -> go ty
    TAll _ _ ty -> go ty
  where
    go :: (MonadError (RenameError Pos) m) => Type -> m (TVar, [TVar])
    go ty0 = case ty0 of
        TVar _ name          -> pure (name, [])
        TApp _ (TVar _ name) ty1 -> do
            xs <- go' ty1
            pure (name, xs)
        TApp _ ty1 ty2 -> do
            (name, tvars) <- go ty1
            tvars' <- go' ty2
            pure (name, tvars ++ tvars')
        _                    -> throwError $ RnBadDataType (hasPosition ty0) ty0
    go' :: (MonadError (RenameError Pos) m) => Type -> m [TVar]
    go' ty = case ty of
        TVar _ tvar -> pure [tvar]
        TApp _ ty1 ty2 -> do
            xs <- go' ty1
            ys <- go' ty2
            pure (xs ++ ys)
        _                    -> throwError $ RnBadDataType (hasPosition ty) ty

noOverlappingVars :: MonadError (RenameError Pos) m => [TVar] -> m ()
noOverlappingVars [] = pure ()
noOverlappingVars (x:xs)
  | rmPosTVar x `elem` map rmPosTVar xs
    = throwError $ RnDuplicateParameterName (hasPosition x) x
  | otherwise = noOverlappingVars xs

rmPosTVar :: TVar -> TVar
rmPosTVar = fmap (const Nothing)

cleanContext :: Rn ()
cleanContext = modify (\ctx -> ctx { tyVariables = mempty, variables = mempty })

crash :: String -> a
crash name =
    error $ unwords [ "Renaming back failed as the name for '"
                    , name
                    , "' could not be found"]

unwrapIdent :: Ident -> String
unwrapIdent (Ident x) = x

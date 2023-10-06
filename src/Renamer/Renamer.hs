{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Renamer.Renamer (rename) where

import Lang.Abs

import Control.Monad.Except
import Control.Monad.State
import Data.Map (Map)
import Data.Map qualified as M
import Error

newtype Rn a = Rn { runRn :: ExceptT (RenameError Pos) (State Ctx) a }
    deriving (Functor, Applicative, Monad, MonadState Ctx, MonadError (RenameError Pos))

data Ctx = Ctx
    { counter :: Int
    , names :: Map Ident Ident
    }

type Pos = BNFC'Position

rename :: Program -> Rn Program
rename = undefined

rnBind :: Bind -> Rn Bind
rnBind (Bind pos name expr) = undefined

rnExpr :: Expr -> Rn Expr
rnExpr = \case
    EPat pos ident -> EPat pos <$> getName pos ident
    ELit pos lit -> pure $ ELit pos lit
    EApp pos e1 e2 -> EApp pos <$> rnExpr e1 <*> rnExpr e2
    ELet pos ident e1 e2 -> do
        name' <- newName ident
        ELet pos name' <$> rnExpr e1 <*> rnExpr e2
    ELam pos ident e -> do
        ident' <- newName ident
        e' <- rnExpr e
        pure (ELam pos ident' e')
    ECase pos e1 branches -> do
        e1' <- rnExpr e1
        branches' <- mapM rnBranch branches
        pure $ ECase pos e1' branches'

rnBranch :: Branch -> Rn Branch
rnBranch (Branch pos pat expr) = undefined

-- Exlude vars that are constructors
rnPattern :: Pattern -> Rn Pattern
rnPattern = \case
    PLit pos lit -> undefined
    PCatch pos   -> undefined
    PVar pos ident -> undefined
    PInj pos ident pats -> undefined

rnType :: Type -> Rn Type
rnType = \case
    TVar pos tvar     -> undefined
    TApp pos ty1 ty2  -> undefined
    TFun pos ty1 ty2  -> undefined
    TAll pos tvars ty -> undefined

newName :: Ident -> Rn Ident
newName (Ident name) = do
    i <- gets counter
    let name' = Ident $ "$" ++ show i ++ name
    modify $ \ctx -> ctx { names = M.insert (Ident name) name' ctx.names}
    pure name'

getName :: Pos -> Ident -> Rn Ident
getName pos name = do
    gets (M.lookup name . names) >>= \case
        Nothing -> throwError $ RnUnboundVar pos name
        Just name' -> return name'

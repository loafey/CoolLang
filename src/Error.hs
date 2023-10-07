{-# LANGUAGE LambdaCase #-}
module Error
    ( RenameError(..)
    , TypeCheckError(..)
    , RuntimeError(..)
    , showErr
    ) where

import Lang.Abs
import Lang.Print (printTree)

type Pos = BNFC'Position

data RenameError a = RnUnboundVar a Ident | RnBadDataType a Type | RnMultiplePatternVar a Ident

data TypeCheckError a = TcUnboundVar a Ident

data RuntimeError a = Error a String

class ShowError a where
    showErr :: a -> String

instance ShowError (RenameError Pos) where
    showErr = \case
        RnUnboundVar pos (Ident name) -> "Unbound variable '" ++ name ++ "' at '" ++ showErr pos ++ "'"
        RnBadDataType pos ty -> "Unexpected type '" ++ printTree ty ++ "' in data declaration at '" ++ showErr pos ++ "'"
        RnMultiplePatternVar pos (Ident name) -> "Conflicting defintions for '" ++ name ++ "' at '" ++ showErr pos ++ "'"

instance ShowError Pos where
    showErr = \case
        Nothing -> ""
        Just (x,y) -> "(" ++ show x ++ ", " ++ show y ++ ")"

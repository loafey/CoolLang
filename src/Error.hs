module Error
    ( RenameError(..)
    , TypeCheckError(..)
    , RuntimeError(..)
    ) where

import Lang.Abs

data RenameError a = RnUnboundVar a Ident

data TypeCheckError a = TcUnboundVar a Ident

data RuntimeError a = Error a String

module Util (
    whenErr,
    whenOk,
    todo,
    sortBindsLast
) where

import           Data.List.Extra (sort, sortBy)
import           Lang.Abs
import           Lang.ErrM       (Err)

whenErr :: Applicative f => Err a -> (String -> f ()) -> f ()
whenErr (Left o) f  = f o
whenErr (Right _) _ = pure ()

whenOk :: Applicative f => Err a -> (a -> f ()) -> f ()
whenOk (Left _) _  = pure ()
whenOk (Right o) f = f o

todo :: a
todo = error "TODO: Not yet implemented"

sortBindsLast :: [Def] -> [Def]
sortBindsLast = sortBy defOrdering

defOrdering :: Def -> Def -> Ordering
defOrdering d1 d2 = case (d1, d2) of
    (DBind _ _, DBind _ _) -> EQ
    (DSig _ _, DSig _ _) -> EQ
    (DData _ _, DData _ _) -> EQ
    (DBind _ _, _) -> GT
    (_, DBind _ _) -> LT
    (DSig _ _, DData _ _) -> GT
    (DData _ _, DSig _ _) -> LT

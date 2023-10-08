module Util (
    whenErr,
    whenOk,
    todo,
    traceShow
) where

import           Debug.Trace (trace, traceShowId)
import           Lang.ErrM   (Err)

whenErr :: Applicative f => Err a -> (String -> f ()) -> f ()
whenErr (Left o) f  = f o
whenErr (Right _) _ = pure ()

whenOk :: Applicative f => Err a -> (a -> f ()) -> f ()
whenOk (Left _) _  = pure ()
whenOk (Right o) f = f o

todo :: a
todo = error "TODO: Not yet implemented"

traceShow :: Show a => a -> a
traceShow = traceShowId

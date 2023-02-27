module Lib.Monad.Repeat
    ( MonadRepeat(..)
) where

{- A type is a MonadRepeat if it provides a way to concatenate a list of operations with Monad bind.
The default implementation of (>>*) uses Monad bind, but a different "operator" can be used, according
to the needs of the user. This class is useful to decrease the amount of code, if there's a long
chain of similar operations. For instance:

f x = do            =>          f x = x >>* [g, h, g, m, h]
    y <- g x
    z <- h y
    a <- g z
    b <- m a
    c <- h b
    return c

The syntax of >>* is also intuitive, because it executes >>= an arbitrary number of times (highlighted
by * in >>*) -}
class Monad m => MonadRepeat m where
    (>>*) :: a -> [(a -> m a)] -> m a
    (>>*) k [] = return k
    (>>*) k (a : t) = (a k) >>= (\x -> (>>*) x t)

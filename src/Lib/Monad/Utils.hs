module Lib.Monad.Utils
    ( doNothing
    , doNothing'
    , notM
    , ixMapM
    , concatMapM
    , concatIxMapM
    , partitionM
    , forAllM
    , local'
    , (>>*)
) where

import Control.Monad.State.Lazy

{- Operation that does literally nothing. -}
doNothing :: Applicative m => m ()
doNothing = pure ()

{- Same of doNothing, but exploiting Monad type-class instead of Applicative. -}
doNothing' :: Monad m => m ()
doNothing' = return ()

notM :: Applicative m => m Bool -> m Bool
notM cond = not <$> cond

rawIxMapM :: Monad m => [a] -> Int -> (Int -> a -> m b) -> m [b]
rawIxMapM [] _ _ = return []
rawIxMapM (x : t) ix f = do
    x' <- f ix x
    t' <- rawIxMapM t (ix + 1) f
    return $ x' : t'

ixMapM :: Monad m => (Int -> a -> m b) -> [a] -> m [b]
ixMapM f xs = rawIxMapM xs 0 f

concatMapM :: Monad m => (a -> m [b]) -> [a] -> m [b]
concatMapM f xs = do
    xss <- mapM f xs
    return $ concat xss

concatIxMapM :: Monad m => (Int -> a -> m [b]) -> [a] -> m [b]
concatIxMapM f xs = do
    xss <- rawIxMapM xs 0 f
    return $ concat xss

partitionM :: Monad m => (a -> m Bool) -> [a] -> m ([a], [a])
partitionM f l = parts l ([], [])
    where
        parts [] (matching, notMatching) = return (reverse matching, reverse notMatching)
        parts (x : t) (matching, notMatching) = do
            isMatching <- f x
            if isMatching
            then parts t (x : matching, notMatching)
            else parts t (matching, x : notMatching)

{- More fancy version of foldM -}
forAllM :: (Foldable t, Monad m) => t a -> (b -> a -> m b) -> b -> m b
forAllM t f x = foldM f x t

{- More fancy version of foldM -}
fromFstToLastM :: (Foldable t, Monad m) => t a -> (b -> a -> m b) -> b -> m b
fromFstToLastM t f x = foldM f x t

{- fromLastToFstM [x1, x2, x3] f a
        =
    do
        a3 <- f x3 a
        a2 <- f x2 a3
        f x1 a2
-}
fromLastToFstM :: (Foldable t, Monad m) => t a -> (a -> b -> m b) -> b -> m b
fromLastToFstM t f x = foldr f' (pure x) t
    where
        f' e yM = do
            y <- yM
            f e y

{- The same of `local` of MonadReader, but with MonadState. -}
local' :: MonadState s m => (s -> s) -> m a -> m a
local' stUpd op = do
    st <- get
    let newSt = stUpd st
    put newSt
    res <- op
    put st
    return res

{- A way to concatenate a list of operations with Monad bind. Useful to decrease the amount of code, if there's a long
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
(>>*) :: Monad m => a -> [a -> m a] -> m a
(>>*) = foldM (\y fM -> fM y)

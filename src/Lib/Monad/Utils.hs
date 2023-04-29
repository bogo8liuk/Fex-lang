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

{- The same of `local` of MonadReader, but with MonadState. -}
local' :: MonadState s m => (s -> s) -> m a -> m a
local' stUpd op = do
    st <- get
    let newSt = stUpd st
    put newSt
    res <- op
    put st
    return res
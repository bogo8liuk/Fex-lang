{- |
Module : Compiler.Ast.Raw.Operations
Description : Operations on the ast
Copyright : (c) Luca Borghi, 2022
License : GPL-3
Stability : experimental

Operations on the abstract syntax tree.
-}

module Compiler.Ast.Raw.Operations
    (
    -- * Operation monad
      AstOpT
    , AstOp
    , AstOpEither
    -- ** Running operations
    , runAstOpT
    , runAstOp
    , runAstOpEither
    , execAstOpT
    , execAstOp
    , execAstOpEither
    -- ** Functions on operation monad
    , astOpErr
    , fallible
) where

import Utils.Data.Foldable (splitMap)
import qualified Compiler.Ast.Raw.Tree as Tree (Program (..), Declaration, HasDeclarations (..))
import Control.Monad.State (StateT, MonadState (..), runStateT, execStateT, gets)
import Data.Functor.Identity (Identity, runIdentity)
import Control.Monad.Trans (lift)

{- |
An operation on a `Tree.Program`. Given @AstOpT s m a@:

@s@ is the actual state associated to the program.

@m@ is the monad which wraps the result of the operation.

@a@ is the result type of the monad.

If you have to write a new module of the compiler, then, generally, you can avoid using this since `AstOp` and
`AstOpEither` should suffice.
-}
type AstOpT s m = StateT (Tree.Program s) m

{- |
Infallible ast operation.
-}
type AstOp s = AstOpT s Identity

{- |
Ast operation which can fail. It's parametric on the error type.
-}
type AstOpEither s err = AstOpT s (Either err)

{- |
Running a parametric ast operation.
-}
runAstOpT :: Tree.Program s -> AstOpT s m a -> m (a, Tree.Program s)
runAstOpT = flip runStateT

{- |
Running an infallible ast operation.
-}
runAstOp :: Tree.Program s -> AstOp s a -> (a, Tree.Program s)
runAstOp p op = runIdentity $ runAstOpT p op

{- |
Running a fallible ast operation.
-}
runAstOpEither :: Tree.Program s -> AstOpEither s err a -> Either err (a, Tree.Program s)
runAstOpEither = runAstOpT

{- |
Executing a parametric ast operation.
-}
execAstOpT :: Monad m => Tree.Program s -> AstOpT s m a -> m (Tree.Program s)
execAstOpT = flip execStateT

{- |
Executing an infallible ast operation.
-}
execAstOp :: Tree.Program s -> AstOp s a -> Tree.Program s
execAstOp p op = runIdentity $ execAstOpT p op

{- |
Executing a fallible ast operation.
-}
execAstOpEither :: Tree.Program s -> AstOpEither s err a -> Either err (Tree.Program s)
execAstOpEither = execAstOpT

{- |
Throwing an error with a fallible ast operation.
-}
astOpErr :: err -> AstOpEither s err a
astOpErr = lift . Left

{- |
It takes an infallible ast operation and it makes it fallible.
-}
fallible :: AstOp s a -> AstOpEither s err a
fallible op = do
    p <- get
    let (x, p') = runAstOp p op
    put p'
    return x

getDeclarations :: Monad m => AstOpT s m [Tree.Declaration s]
getDeclarations = gets Tree.declarations

{- |
Given a list of `Declaration`s, it replaces the current program with it.
-}
replaceProgram :: Monad m => [Tree.Declaration s] -> AstOpT s m ()
replaceProgram = put . Tree.Program

{- |
It removes a selected set of `Declaration`s. A mapping function is passed to select which declarations have to be
removed: if the function returns @Nothing@, then the declaration is not removed, else, the mapped value (unwrapped
from @Just@ constructor) is returned in the final list and the declaration is removed.
-}
removeDeclarationsWith :: Monad m => (Tree.Declaration s -> Maybe d) -> AstOpT s m [d]
removeDeclarationsWith select = do
    decls <- getDeclarations
    let (selected, decls') = splitMap select decls
    replaceProgram decls'
    return selected

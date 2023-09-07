{- |
Module : Compiler.Ast.Raw.Tree
Description : Abstract syntax tree
Copyright : (c) Luca Borghi, 2022
License : GPL-3
Stability : experimental

API for the abstract syntax tree. Some notation: we will refer to nodes of the ast as "tokens", for example,
a type definition (with presumably a name and list of constructors) is token; we will refer to opaque objects
denoting some tokens as "items". An item should keep no information about a token, including its structure.
-}

{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module Compiler.Ast.Raw.Tree
    (
    -- * Nodes
    --
    -- | The nodes of abstract syntax tree. The entry-point is `Program`.
      Program(..)
    , Declaration(..)
    , TypeDefinition(..)
    , AliasDefinition(..)
    , TypeClassDefinition(..)
    , InstanceDefinition(..)
    , Signature(..)
    , Binding(..)
    , TypeDefHeader(..)
    , TypeClassDefHeader(..)
    , InstanceDefHeader(..)
    , SymbolDefHeader(..)
    , DataConDefinition(..)
    , Type(..)
    , QualifiedType(..)
    , Constraint(..)
    , TypeHinting(..)
    , TypeConName(..)
    , DataConName(..)
    , ConstraintName(..)
    , SymbolName(..)
    , TypeVarName(..)
    , Literal(..)
    , TyHintExpression(..)
    , Expression(..)
    , Lambda(..)
    , PatternExpression(..)
    , Case(..)
    , MultiplePatternCase(..)
    , PatternMatch(..)
    , MultiplePatternMatch(..)
    -- ** Tokens with expressions
    , DefinitionWithExpression(..)
    -- ** Filters
    , DeclarationItem(..)
    , DeclarationFilter
    -- * Functions on ast
    -- ** Building
    -- ** Getters
    , HasDeclarations(..)
) where

import Data.Text (Text)
import Data.List.NonEmpty (NonEmpty)
import Utils.Data.Filter (Filter)
import Compiler.Ast.Common (Item (..))
import Compiler.Lib.Types (AutoApplication, RightApplication, LeftApplication)

{- |
Parametric token along with an expression. There are two cases:
-}
data DefinitionWithExpression head a
    {- |
    it takes a parametric head and attaches an expression.
    -}
    = SimpleDef head (TyHintExpression a) a
    {- |
    it consists in a `MultiplePatternMatch` construct. No headers.
    -}
    | PatternDef (MultiplePatternMatch a) a

{- |
Entry point of a program.
-}
newtype Program a = Program [Declaration a]

data Declaration a
    = TyDef (TypeDefinition a)
    | AliasDef (AliasDefinition a)
    | TyClDef (TypeClassDefinition a)
    | InstDef (InstanceDefinition a)
    | Sig (Signature a)
    | Bind (Binding a)

data TypeDefinition a = TypeDefinition (TypeDefHeader a) [DataConDefinition a] a

data AliasDefinition a = AliasDefinition (TypeDefHeader a) (Type a) a

data TypeClassDefinition a = TypeClassDefinition (TypeClassDefHeader a) [Signature a] a

data InstanceDefinition a = InstanceDefinition (InstanceDefHeader a) [Binding a] a

data Signature a = Signature (SymbolName a) (QualifiedType a) a

data Binding a = Binding (DefinitionWithExpression (SymbolDefHeader a) a) a

data TypeDefHeader a = TyDefHeader (TypeConName a) [TypeVarName a] a
data TypeClassDefHeader a = TyClDefHeader (ConstraintName a) [TypeVarName a] [Constraint a] a
data InstanceDefHeader a = InstDefHeader (ConstraintName a) [Type a] [Constraint a] a
data SymbolDefHeader a = SymDefHeader (SymbolName a) [SymbolName a] a

data DataConDefinition a = DataCon (DataConName a) [Type a] a

data Type a
    = SingleTyCon (TypeConName a) a
    | SingleTyVar (TypeVarName a) a
    | AppType !(AutoApplication Type a) a
data QualifiedType a = QualType !(RightApplication Constraint Type a) a
data Constraint a = Constraint !(LeftApplication ConstraintName Type a) a
data TypeHinting a = TyHint (QualifiedType a) a

data TypeConName a = TyConName !Text !a
data DataConName a = DataConName !Text !a
data ConstraintName a = ConstrName !Text !a
data SymbolName a = SymName !Text !a
data TypeVarName a = TyVarName !Text !a

data Literal a
    = IntLit Integer a
    | FloatLit Double a
    | CharLit Char a
    | StringLit String a

data TyHintExpression a = Expr (Expression a) (Maybe (TypeHinting a)) a
data Expression a
    = ExprVar (SymbolName a) a
    | ExprDataCon (DataConName a) a
    | ExprLit (Literal a) a
    | ExprLam (Lambda a) a
    | ExprMatch (PatternMatch a) a
    | ExprLet (Binding a) (TyHintExpression a) a
    | ExprLetRec (NonEmpty (Binding a)) (TyHintExpression a) a
    | ExprApp (AutoApplication TyHintExpression a) a

data Lambda a = Lambda (DefinitionWithExpression [SymbolName a] a) a

data PatternExpression a
    = PatternVar (SymbolName a) a
    | PatternLit (Literal a) a
    | PatternDataCon (DataConName a) a
    | PatternDefault a
    | PatternDataConApp (LeftApplication DataConName PatternExpression a) a

data Case a = Case (PatternExpression a) (TyHintExpression a) a
data MultiplePatternCase a = MultiCase (NonEmpty (PatternExpression a)) (TyHintExpression a) a

data PatternMatch a = PatternMatch (TyHintExpression a) [Case a] a
data MultiplePatternMatch a = MultiMatch [MultiplePatternCase a] a

{- |
Tokens which encapsulate a list of `Declaration`.
-}
class HasDeclarations t where
    declarations :: t a -> [Declaration a]

instance HasDeclarations Program where
    declarations (Program decls) = decls

{- |
Data constructors corresponding to the different types of declarations.
-}
data DeclarationItem
    = TypeDefItem
    | AliasDefItem
    | TyClDefItem
    | InstDefItem
    | SigItem
    | BindItem

type DeclarationFilter = Filter DeclarationItem

instance Item DeclarationItem DeclarationItem where
    itemOf = id

instance Item (Declaration a) DeclarationItem where
    itemOf (TyDef tyDef) = itemOf tyDef
    itemOf (AliasDef aliasDef) = itemOf aliasDef
    itemOf (TyClDef tyClDef) = itemOf tyClDef
    itemOf (InstDef instDef) = itemOf instDef
    itemOf (Sig sig) = itemOf sig
    itemOf (Bind bind) = itemOf bind

instance Item (TypeDefinition a) DeclarationItem where
    itemOf _ = TypeDefItem

instance Item (AliasDefinition a) DeclarationItem where
    itemOf _ = AliasDefItem

instance Item (TypeClassDefinition a) DeclarationItem where
    itemOf _ = TyClDefItem

instance Item (InstanceDefinition a) DeclarationItem where
    itemOf _ = InstDefItem

instance Item (Signature a) DeclarationItem where
    itemOf _ = SigItem

instance Item (Binding a) DeclarationItem where
    itemOf _ = BindItem

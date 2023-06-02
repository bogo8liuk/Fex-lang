{- Rewriting of Ast.Tree module. A new file is created since Ast.Tree.hs is a huge file. -}

module Compiler.Ast.Tree
    ( Program(..)
) where

import Data.Text(Text)
import Data.List.NonEmpty(NonEmpty)

{- There are various ast tokens built upon the application of other tokens. These tokens are built with recursive
application of tokens which build them. Other tokens, instead, use list of tokens since the semantics is not the
the tokens applications. -}
data TokenLeftApplication applier applied a =
      LeftHead (applier a)
    | LeftApp (TokenLeftApplication applier applied a) (applied a)

data TokenLeftApplication' app a =
      LeftHead' (app a)
    | LeftApp' (TokenLeftApplication' app a) (app a)

data TokenRightApplication applied applier a =
      RightHead (applier a)
    | RightApp (applied a) (TokenRightApplication applied applier a)

data TokenAutoApplication app a = AutoApp (app a) (app a)

data ExpressionDefinition head a =
      SimpleDef head (Expression a) a
    | PatternDef (PatternMatch a) a

newtype Program a = Program [Declaration a]

data Declaration a =
      TyDef (TypeDefinition a)
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

data Binding a = Binding (ExpressionDefinition (SymbolDefHeader a) a) a

data TypeDefHeader a = TyDefHeader (TypeConName a) [TypeVarName a] a
data TypeClassDefHeader a = TyClDefHeader (ConstraintName a) [TypeVarName a] [Constraint a] a
data InstanceDefHeader a = InstDefHeader (ConstraintName a) [Type a] [Constraint a] a
data SymbolDefHeader a = SymDefHeader (SymbolName a) [SymbolName a] a

data DataConDefinition a = DataCon (DataConName a) [Type a] a

data Type a =
      SingleTyCon (TypeConName a) a
    | SingleTyVar (TypeVarName a) a
    | AppType !(TokenAutoApplication Type a) a
data QualifiedType a = QualType !(TokenRightApplication Constraint Type a) a
data Constraint a = Constraint !(TokenLeftApplication ConstraintName Type a) a
data TypeHinting a = TyHint (QualifiedType a) a

data TypeConName a = TyConName !Text !a
data DataConName a = DataConName !Text !a
data ConstraintName a = ConstrName !Text !a
data SymbolName a = SymName !Text !a
data TypeVarName a = TyVarName !Text !a

data Literal a

data TyHintExpression a = Expr (Expression a) (TypeHinting a) a
data Expression a =
      ExprVar (SymbolName a) a
    | ExprDataCon (DataConName a) a
    | ExprLit (Literal a) a
    | ExprLam (Lambda a) a
    | ExprMatch (PatternMatch a) a
    | ExprLet (Binding a) (TyHintExpression a) a
    | ExprLetRec (NonEmpty (Binding a)) (TyHintExpression a) a
    | ExprApp (TokenAutoApplication TyHintExpression a) a

data Lambda a = Lambda (ExpressionDefinition [SymbolName a] a) a

data PatternMatch a

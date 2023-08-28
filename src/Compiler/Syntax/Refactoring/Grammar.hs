{- |
Module : Compiler.Syntax.Refactoring.Grammar
Description : Parsing of language constructs
Copyright : (c) Luca Borghi, 2023
License : GPL-3
Stability : experimental

Parsing of language constructs.
-}
module Compiler.Syntax.Refactoring.Grammar
    (
) where
import Compiler.Syntax.Refactoring.Types (LangParser)
import qualified Compiler.Ast.Raw.Tree as Raw

declaration :: LangParser (Raw.Declaration a)
declaration = undefined

typeDefinition :: LangParser (Raw.TypeDefinition a)
typeDefinition = undefined

aliasDefinition :: LangParser (Raw.AliasDefinition a)
aliasDefinition = undefined

typeClassDefinition :: LangParser (Raw.TypeClassDefinition a)
typeClassDefinition = undefined

instanceDefinition :: LangParser (Raw.InstanceDefinition a)
instanceDefinition = undefined

signatureDeclaration :: LangParser (Raw.Signature a)
signatureDeclaration = undefined

bindingDeclaration :: LangParser (Raw.Binding a)
bindingDeclaration = undefined

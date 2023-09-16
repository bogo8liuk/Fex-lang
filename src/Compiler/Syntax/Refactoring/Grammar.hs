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
import Lib.Pos as With (ProgramPos)
import Compiler.Syntax.Refactoring.Lib (track)
import Compiler.Syntax.Refactoring.Language (lowerIdentifier, upperIdentifier)
import Data.Text (Text)

declaration :: LangParser (Raw.Declaration With.ProgramPos)
declaration = undefined

typeDefinition :: LangParser (Raw.TypeDefinition With.ProgramPos)
typeDefinition = undefined

aliasDefinition :: LangParser (Raw.AliasDefinition With.ProgramPos)
aliasDefinition = undefined

typeClassDefinition :: LangParser (Raw.TypeClassDefinition With.ProgramPos)
typeClassDefinition = undefined

instanceDefinition :: LangParser (Raw.InstanceDefinition With.ProgramPos)
instanceDefinition = undefined

signatureDeclaration :: LangParser (Raw.Signature With.ProgramPos)
signatureDeclaration = undefined

bindingDeclaration :: LangParser (Raw.Binding With.ProgramPos)
bindingDeclaration = undefined

symbolName :: LangParser (Raw.SymbolName With.ProgramPos)
symbolName = parseName Lower Raw.SymName

typeConName :: LangParser (Raw.TypeConName With.ProgramPos)
typeConName = parseName Upper Raw.TyConName

dataConName :: LangParser (Raw.DataConName With.ProgramPos)
dataConName = parseName Upper Raw.DataConName

constraintName :: LangParser (Raw.ConstraintName With.ProgramPos)
constraintName = parseName Upper Raw.ConstrName

typeVarName :: LangParser (Raw.TypeVarName With.ProgramPos)
typeVarName = parseName Lower Raw.TyVarName

data LetterCase = Lower | Upper

parseName
    :: Functor name
    => LetterCase
    -> (Text -> () -> name ())
    -> LangParser (name With.ProgramPos)
parseName letterCase build = track $ do
    textName <-
        case letterCase of
            Lower -> lowerIdentifier
            Upper -> upperIdentifier
    return $ build textName ()

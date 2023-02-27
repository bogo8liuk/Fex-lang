module Compiler.Syntax.Lib.Info
    ( CustomParsec
    , CustomOperator
    , CustomOpTable
    , Status
    , Component(..)
    , makeInitialStatus
    , setObjectToParse
    , getObjectToParse
) where

import Data.Functor.Identity
import Text.Parsec
import Text.Parsec.Expr

 --TODO: Change the name of CustomParsec in <name of the compiler>Parsec, it's better
type CustomParsec = Parsec String Status
type CustomOperator = Operator String Status Identity
type CustomOpTable a = OperatorTable String Status Identity a

--TODO: change docs of this adt
data Component =
    Program                 | -- A Program is made by a sequence of import/export and a CommandSequence TODO
    {- TODO: import/export -}
    OperatorsCategory       |
    OpsCategoryField String |
    OperatorsCategoryName   |
    CommandSequence         | -- A CommandSequence is made by a sequence of Command
    Command                 | -- A Command can be a Declaration or an Expression
    Declaration             | -- A Declaration can be an ADT, an Interface, an Instance, a Binding or a Note
    CaseSequence            | -- A CaseSequence is everything that is separated by pipes |
    ThenSequence            |
    MappingSequence         |
    Application             | -- An Application is everything that is separated by space (where a space has a semantic)
    ADT                     | -- An ADT is Algebraic Data Type declaration
    Alias                   | -- An Alias is an alias declaration
    ADTConstructor          | -- An ADTConstructor is a constructor for an ADT
    RecordConstructor       | -- A RecordConstructor is a constructor for a record case (ADT)
    Interface               | -- An Interface is an interface declaration TODO
    Instance                | -- An Instance is an instance statement TODO
    SymbolDeclaration       | -- A Binding is a declaration of a symbol with `let` followed by `=` Keyword and an Expression
    ConstraintStatement     |
    SignatureStatement      | -- A Note is a type note over Bindings TODO
    Type                    | -- A Type is the name of a type
    Enclosing               | -- An Enclosing is something (whatever?) enclosed by ()
    Expression              | -- An Expression (should be a returning-value statement) can be a Primitive, Symbol, Operation,
                              -- BoundExpression, LambdaExpression, EnclosedExpression, PatternMatch
    Literal                 | -- A Primitive can be a NumberVal, a CharacterVal, a FloatVal, a StringVal (TODO: something else)
    NumberLiteral           | -- A NumberVal can be an IntegerVal or a FloatVal
    CharacterLiteral        | -- A CharacterVal is a single character enclosed by ''
    StringLiteral           | -- A StringVal is a sequence of characters enclosed by ""
    IntegerLiteral          | -- An IntegerVal is a number not followed by a point and other digits and potentially preceded by a -
    NaturalLiteral          |
    DoubleLiteral           | -- A FloatVal is like an Integer but it has to be followed by a point . and some digits
    TupleCon                |
    TupleTyCon              |
    Symbol                  | -- A Symbol is a name of a variable
    Keyword String          | -- A Keyword is a language-defined symbol (not Symbol)
    Operator                | -- An Operator is a Symbol that has also operators features
    Operation               | -- An Operation is an Expression followed by an Operator followed by another Expression
    BoundExpression         | -- A BoundExpression is a Binding followed by a `in` Keyword followed by an Expression
    LambdaExpression        | -- A LambdaExpression is an anonymous function
    EnclosedExpression      | -- An EnclosedExpression is an expression enclosed by parenthesis
    MatchExpression         | -- A MatchExpression is a pattern match expression
    MatchCase               | -- | x -> f x
    TypeExplicitation       | -- A TypeExplicitation is the explicitation of the type of an expression
    Then                    |
    And                     |
    Void deriving Eq          -- A Void should be everything like comments or white spaces (They are not semantic-less, e.g. in f application)

instance Show Component where
    show Program = "program"
    show OperatorsCategory = "operators category"
    show (OpsCategoryField s) = "operators category field " ++ s
    show OperatorsCategoryName = "operators category name"
    show CommandSequence = "program"
    show Command = "command"
    show Declaration = "declaration"
    show CaseSequence = "sequence of cases"
    show ThenSequence = "sequence of relations"
    show Application = "application"
    show ADT = "ADT declaration"
    show Alias = "alias declaration"
    show ADTConstructor = "ADT constructor"
    show RecordConstructor = "ADT record constructor"
    show Interface = "interface declaration"
    show Instance = "instance declaration"
    show SymbolDeclaration = "variable declaration"
    show ConstraintStatement = "constraint statement"
    show SignatureStatement = "signature statement"
    show Type = "type name"
    show Enclosing = "statement enclosed between ()"
    show Expression = "expression"
    show Literal = "literal constructor"
    show NumberLiteral = "literal number"
    show CharacterLiteral = "literal character"
    show StringLiteral = "literal string"
    show IntegerLiteral = "literal integer"
    show NaturalLiteral = "literal natural"
    show DoubleLiteral = "literal floating point number"
    show TupleCon = "tuple constructor"
    show TupleTyCon = "tuple type constructor"
    show Symbol = "variable"
    show (Keyword s) = "keyword " ++ s
    show Operator = "operator"
    show Operation = "two expressions between an operator"
    show BoundExpression = "expression with bindings"
    show LambdaExpression = "lambda"
    show EnclosedExpression = "expression enclosed between ()"
    show MatchExpression = "match expression"
    show MatchCase = "match case"
    show TypeExplicitation = "type explicitation"
    show Then = "then statement"
    show And = "and separated statements"
    show Void = "non-code text"

-- The reason why the type of operatorsTable resides in Text.Parsec.Expr.buildExpressionParser
newtype Status = ParsingStatus { objectToParse :: Component
                               }

instance Show Status where
    show s = show (objectToParse s)

makeInitialStatus :: Status
makeInitialStatus = ParsingStatus { objectToParse = Program }

{-The first argument is taken for future changes: if I add a field in Status record, then it will be necessary to
have a Status to change the object to parse, because the other fields of the status remain unchanged -}
setObjectToParse :: Status -> Component -> Status
setObjectToParse _ comp = ParsingStatus { objectToParse = comp }

getObjectToParse :: Status -> Component
getObjectToParse ParsingStatus { objectToParse = obj } = obj

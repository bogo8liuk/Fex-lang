# Fex
Fex is a "pure" functional programming language. It's experimental and it's strongly inspired by Haskell. The actual state
of the project is "work-in-progress". This project is the work made for my bachelor thesis.

### TODO
- Parsec TokenParser exposes two methods (`reserved` and `reservedOp`): it is not portable to have both (if I change the lex analysis of a keyword, the method to parse the keyword could change as well, `reserved` or `reservedOp`), so create the abstraction to have only one method

- change "interface" (and all related identifiers like "intf") with "property"
- rewrite Names module with above tasks and with new abstractions in Ast.Tree, namely using AstOp

- problem: in kind-inference, when a token with a variable as a kind is promoted, it keeps the old state, instead of taking the one of the position where the new kind has been discovered.
- refactor all imports (use namespaces, import just what you need, etc.)

- turn Lib into a stand-alone library
- rewrite adt defs in codebase with application instead of tuples (e.g. data A = A (Int, Char) ==> data A = A Int Char)

- implement Eq and Ord for Ast.Tree names tokens and change the codebase where strings are used instead of the tokens directly

- can Names module incorporate Constraints module???
- fix warnings all around the codebase

# Fex
Fex is a "pure" functional programming language. It's experimental and it's strongly inspired by Haskell. The actual state
of the project is "work-in-progress". This project is the work made for my bachelor thesis. If you understand the italian
tongue, a starting point for the documentation can be found at https://github.com/bogo8liuk/thesis . Official documentation
will come in the future (I hope as soon as possible). Anyway, this is an experiment and everyone are encouraged to contribute,
thus feel free to start a pull request or open an issue!

### main TODOs
- Rewrite old code in terms of monad trasformers.
- Rewrite kind-inference and the definition of kind.
- Fix and terminate code generation. The situation is like this: it generates the assembly code, but I didn't find the way
to generate an executable. The actual state of art is that the compiler makes an interface file, but even with it and the
assembly code, ghc can't create an executable. A possible way is to leave Core as target and creating the backend from scratch.
This is a very drastic solution, but making my own backend can be really interesting and even more flexible (and
backward-compatible, since ghc API are very likely to change).
- Write the documentation on two sides: 1) the compiler code itself, in order to ease contributions; 2) the Fex language,
but this hasn't the priority now, since the language can change radically.

#### technical TODOs
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

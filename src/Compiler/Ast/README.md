# Type equality
In this section, the type equality is described in order to keep track of it, since it is quite complex.
First of all, we have to introduce our type-system:

### Type-system
The language type system is quite simple and consists in:
- function type (we can represent it as `(->)`) which have kind `(* -> * -> *)` ;
- the rest of types; this set is variable from program to program, because user can define as many new types as he want;
  some types of this set are builtin, but this is not important;

To define type equality test we need extra information like a function which, given a type value, returns its constraints.
We will refer to such function as "context" or `CXT`. We need also to introduce a new concept which is the satisfiability
between two constraints.

### Constraints satisfiability
Let c, d two constraints, we say c satisfies d if all the following conditions are true: TODO

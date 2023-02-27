# Rules on constraints
Taking inspiration from Paterson conditions (see FlexibleInstances haskell extension or the paper "Understanding Functional 
Dependecies via Constraint Handling Rules"), the rules on constraints are:
- in each constraint, non-type variables can appear, but at least one type variable has to appear;
- no type variable has more (a greater or equal number of) occurrences in each constraint of an instance (but checked one
by one, not counted together) than the associated "head";
- type variables in the constraints of an instance must be wrapped in less constructors (counting for all type variables)
than the type variables of the head.

The first one refers to all constraints, the second and the third ones just to instances constraints.

TODO: look at constraints which matches instances (warning in haskell)

# Rules on names
With "names", I will refer to all that strings which can be written by a user in a program and which represent a new
definition or concept at some point of a program, so keywords are not included in this set because they do not represent
a new definition or concept in a program; also builtin constructors or types (or whatever builtin) are not included
(even if they are not keywords), because they do not introduce something new in a certain point of program, but they
are compiled without the user define them.

The following rules are valid:
- there cannot exist two or more global symbol names with the same name;
- each global symbol name cannot not have the same name of any symbol name in any property;
- if we consider property symbol names as global, the second rule is just a consequence of the first rule;
- there cannot exist two or more property names with the same name;
- there cannot exist two or more type names with the same name;
- there cannot exist two or more constructor names with the same name;
- each type variable in an adt declaration must be bound.

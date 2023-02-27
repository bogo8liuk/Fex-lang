# Tuple type
How is Tuple type handled? Just for now, it is not an infinite type (from which an arbitrary number of types can be generated), 
and it is based on the Haskell tuple type. In order to do this, there is max size a tuple value cannot overcomes which is
`maxTupleSize` defined in `GHC.Exts`.

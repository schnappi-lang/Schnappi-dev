# Schnappi-dev
Yet another purely functional programming language with first class types


## Core

Schnappi Core is based on a weird and untested type theory, which is inspired by Idris2, Agda, Pie and other dependently typed languages.

In practice, this means that every type has attributes associated with it.

Attributes mainly apply to values, but there is a exception.

All attributes:

+ level
+ size
+ usage
+ usage of the type itself
+ assumptions
+ diverge or not

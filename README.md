# Schnappi-dev

Yet another purely functional programming language with first class types

Note: Schnappi is a research language that is currently under heavy development. Schnappi consists of a number of very poorly studied language features and is not designed for production use.

Schnappi originally wants to confuse algebraic effects with something also called effects ([div of Koka](https://koka-lang.github.io/koka/doc/std_core_types.html#type_space_div))

But it does not sound like a good idea.

## Core

Schnappi is based on a weird and untested type theory, which is inspired by Idris2, Agda, Pie, Koka and other programming languages.

In practice, this means that every type has attributes associated with it.

Attributes mainly apply to values, but there is a exception.

All attributes:

+ level (universe hierarchy)
+ size (sized types in agda)
+ usage (quantity in Idris2)
+ usage of the type itself - Yes, you can have type information available at run time and maybe doing pattern matching on it? (not yet implemented)
+ assumptions (Axiom in Coq) - only erased values can be assumed
+ diverge or not (inspired by Koka)
+ (todo) no-check - not fully checked - equals to the assumption `t: Type, x: t, u: Type -> x: u` - now then we have yet another typescript?

## Full

Todo:

+ complex implicits - maybe it is easier to write the elaborator in miniKanren
+ effects

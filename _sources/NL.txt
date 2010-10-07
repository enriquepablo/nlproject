
NL, the first order theory behind nl
====================================

The language
------------

I will call NLL the first order language for NL. NLL is a FOL with equality (as a transitive, simmetrical and reflaxive relation) with the following non-logical terms:

binary relations: is, isa.

n-ary functor: proposition.

atomic terms: verb, fact, expression.

Axiomatization
--------------

forall x, y, z: (is(x, y) -> is(x, z)) -> isa(y, z)

forall x, y: isa(x, y) & isa(y, x) -> equals(x, y)

forall x, y, z: is(x, y) & equals(y, z) -> is(x, z)

These are taken from Paul Bernays, and provide us with the form of a simple set theory without "constructive" axioms. We only add to these:

forall x: is(x, expression)

forall x, y, z: is(x, y) & is(y, z) -> isa(z, expression)

isa(verb, expression)

isa(fact, expression)

forall x, y1,...yn: is(proposition(x, y1,...yn), fact) -> is(x, verb)

Interpretation.
---------------

We interpret extensions of NL in natural language texts. We take these texts as sets of expressions, that are related among them by the uses of the copular verb within the text. We interpret our predicates as different uses of the copular verb, and the construction "is(proposition(...), fact)" as the sentences of the text that are not made by copular verbs.

Example
-------

The text:

People are things. Men and woman are people. John is a man, and Yoko is a woman. People can love other people. XXX TODO.

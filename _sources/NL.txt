
APPENDIX: NL, the first order theory behind nl
====================================

The interpretation (informal)
-----------------------------

We are going to examine the natural language. For that, we are going to talk about *expressions*. We are going to take that an expression is (almost) anything that might be said by a normal human being and that has meaning. Independently of whether it has truth value. So: "jonh", "that man", "a man", "Jonh thinks about Mary", are expressions. I say almost because we exclude from the set of expressions the copular sentences. So: "John is a man", "Men are animals", or "it is a fact that John is thinking about Mary" are not expressions.

We can say a few things about the set of expressions. First, that they are composed of finitely many atomic expressions, and that there are limits to the amount of composite expressions that it is possible to form with them. The number is not only finite but fairly small, and perhaps not too different with the number of entries in the wikipedia.

Second, that there seem to be two kinds of expressions in our set: those with truth value, and those without it. However, we are going to take an alternative but equivalent view here. We shall imagine that no expression has truth value, and that those that seem to have it, are in fact borrowing it from an underlying but implicit copular sentence: "it is a fact that...".

What we do now is to think about copular sentences as relations among expressions. So, Copular sentences, from this point of view, impose certain structures over sets of expressions. These structures are similar to the structures of set or class systems. We may distinguish 3 uses of the copular verb, that can be likened to the basic predicates of class systems: "x belongs to class y" (such as in "John is a man"), "x is a subclass of y" (such as in "a man is an animal"), and "x is the same as y" (such as in "that man is John").

These structures of expressions, which we might call discourses, have of course informal interpretations, they have semantics, and we use them to communicate truths. However, we are going to disregard those semantics, which are nebulous and reside in a strange universe, and we are going to try to take those structures of expressions themselves as semantics, as the standard interpretation of some language to be developed. If we disregard their interpretations, they are really simple structures.

And the development of this language is very simple. We know that it is similar to some class or set system, and we know that there are a limited amount of of individuals in its domain of interpretation. We have 2 kinds of individuals: those that can have elements, and those that cannot. This language can be a really simple, consistent and decidable first order theory. It would not need of any productive axioms, since we don't want it to contain all truth in a few axioms: we want it to extend it in an ad hoc manner to model different discourses.

The language
------------

I will call NLL the first order language for NL. NLL is a fist order language with equality with the following non-logical terms:

binary relations: is, isa.

functor: proposition. Note that we don't impose an arity on this functor, though we might as well do so.

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


The advantage
-------------

proposition is a functor. So, any function constructed upon proposition can be used as a term within another such function.

The interpretation (formal)
---------------------------

TODO XXX

Example
-------

The text:

People are things. Men and woman are people. John is a man, and Yoko is a woman. People can love other people. XXX TODO.

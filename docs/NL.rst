
APPENDIX: NL, the first order theory behind nl
====================================

The intended interpretation (informal)
--------------------------------------

We are going to examine the natural language. For that, we are going to talk about *expressions*. We are going to take that an expression is (almost) anything that might be said by a normal human being and that has meaning. Independently of whether he assigns a truth value to them. So: "jonh", "that man", "a man", "Jonh loves Mary", are expressions. I say "almost" because we exclude from the set of expressions the copular sentences. So: "John is a man", "Men are animals", or "it is a fact that John loves Mary" are not to be considered expressions.

We can say a couple of things about expressions. First, that there are atomic and composite expressions. In principle we won't distinguish among them, and will just talk about expressions.

Second, that there are finitely many atomic expressions, and that there are limits to the amount of composite expressions that it is possible to form with them. The number is not only finite but fairly small, and perhaps not too different from the number of entries in the wikipedia.

And third, that there seem to be two kinds of expressions: those with truth value (such as "John loves Mary"), and those without it (such as "John"). In this respect, we are going to take an alternative but equivalent view. We shall imagine that no expression has truth value, and that those that seem to have it, are in fact borrowing it from an underlying but implicit copular sentence: "it is a fact that <expression>".

What we do now is to think about copular sentences as relations among expressions. So, Copular sentences, from this point of view, impose certain structures over the set of expressions. These structures are similar to set or class structures. We may distinguish 3 uses of the copular verb, that can be likened to the basic predicates of class systems: "x belongs to class y" (such as in "John is a man"), "x is a subclass of y" (such as in "a man is an animal"), and "x is the same as y" (such as in "that man is John").

These structures of expressions, which we might call discourses, have of course, and by definition, meaning, they have semantics, and we use them to communicate truths. However, we are going to disregard those semantics, which are nebulous and reside in a strange universe, and we are going to try to take those structures of expressions themselves as the standard interpretation of some language to be developed. If we disregard their meaning, they are really simple structures.

The development of this language is quite easy. We know that its interpretation is similar to some class or set system, and we know that there are a limited amount of of individuals in its domain of interpretation. We have 2 kinds of individuals: those that can have elements, and those that cannot. This language can be a really simple, finite domain, consistent and decidable first order theory. It would not need of any "productive" axioms, since we don't want it to contain all truth in a few axioms: we want it to be able to extend it in an ad hoc manner to model different discourses.

Finally, we can talk about the composition of expressions. This can be done through the use of function terms. This of course includes non-copular sentences such as "John loves Mary". And I think that it is not necessary to point out that it is much easier to mirror complex grammars, such are those developed to analyze the natural languages, with function symbols than it is with predicate symbols. It might be worth pointing out 2 advantages.

One. "Natural" sentences -excluding copular ones- map to formal functions. This allows us to nest them, in contrast to what happens if we map them to formal predicates. For example, "thinks that wants to eat an apple" might map to something like "fact(thinks, fact(wants, fact(eat, apple)))".

Second. Since natural sentences map to functions, the antinomies that unrestricted comprehension produces are excluded when we use them to produce anonymous "classes". We only have one kind of individual, expression, and we may quantify over it with no restriction.

In principle, I will develop the language with recourse to a single function term, that will allow us to produce non-copular sentences out of other terms, though there is no limit to the number of possible operators that we might use to, for example, mirror the adjectivation of nouns, etc.

The language
------------

The language for NL is a fist order language with equality with the following non-logical terms:

binary relations: is, isa.

functor: proposition. Note that we don't impose an arity on this functor. We might alternatively use a function term with a definite arity for each verb, but here, for simplicity, I will just provide this.

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


The interpretation (formal)
---------------------------

TODO XXX

Example
-------

The text:

People are things. Men and woman are people. John is a man, and Mary is a woman. People can love other people. XXX TODO.

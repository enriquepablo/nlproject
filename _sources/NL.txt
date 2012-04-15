
APPENDIX: NL, the first order theory behind nl
================================================

The intended interpretation
---------------------------

We are going to examine the natural language. For that, we are going to talk about *expressions*. We are going to take that an expression is (almost) anything that might be said by a normal human being and that has meaning. Independently of whether he assigns a truth value to them. So: "jonh", "that man", "a man", "Jonh loves Mary", are expressions. I say "almost" because we exclude from the set of expressions the copular sentences. So: "John is a man", "Men are animals", or "it is a fact that John loves Mary" are not to be considered expressions.

We can say a couple of things about expressions. First, that there are atomic and composite expressions. In principle we won't distinguish among them, and will just talk about expressions.

Second, that there are finitely many atomic expressions, and that there are limits to the amount of composite expressions that it is possible to form with them.

And third, that there seem to be two kinds of expressions: those with truth value (such as "John loves Mary"), and those without it (such as "John"). In this respect, we are going to take an alternative but equivalent view. We shall imagine that no expression has truth value, and that those that seem to have it, are in fact borrowing it from an underlying but implicit copular sentence: "it is a fact that <expression>".

What we do now is to think about copular sentences as relations among expressions. So, Copular sentences, from this point of view, impose certain structures over the set of expressions. These structures are similar to set or class structures. We may distinguish 3 uses of the copular verb, that can be likened to the basic predicates of class systems: "x belongs to class y" (such as in "John is a man"), "x is a subclass of y" (such as in "a man is an animal"), and "x is the same as y" (such as in "that man is John").

These structures of expressions, which we might call discourses, have of course, and by definition, meaning, they have semantics, and we use them to communicate truths. However, we are going to disregard those semantics, which are nebulous and reside in a strange universe, and we are going to try to take those structures of expressions themselves as the standard interpretation of some language to be developed. If we disregard their meaning, they are really simple structures.

The development of this language is quite simple. We know that its interpretation is similar to some class or set system, and we know that there are a limited amount of of individuals in its domain of interpretation. We have 2 kinds of individuals: those that can "have elements", and those that cannot. This language can be a finite domain, consistent and decidable first order theory. It would not need of any "productive" axioms, since we don't want it to contain all truth in a few axioms: we want it to be able to extend it in an ad hoc manner to model different discourses.

Finally, we can talk about the composition of expressions. This can be done through the use of operators. This of course includes non-copular sentences such as "John loves Mary".
In principle, I will develop the language with recourse to a single operator, that will allow us to produce non-copular sentences out of other symbols, though there is no limit to the number of possible operators that we might use.

The language
------------

The language for NL is a fist order language with equality with the following non-logical symbols:

binary relations: isa (belongs), is (subset).

ternary functor: f (fact).

atomic terms: verb, fact, expression.

As variables, we will use x, y, z.

Axiomatization
--------------

The first three are taken from Paul Bernays, and provide us with the form of a simple set theory without "constructive" axioms:

1) forall x, y, z: (isa(x, y) -> isa(x, z)) -> is(y, z)

2) forall x, y: is(x, y) & is(y, x) -> equals(x, y)

3) forall x, y, z: isa(x, y) & equals(y, z) -> isa(x, z)

The fourth axiom establishes that there is a universal expression, that we denote with "expression":

4) forall x: isa(x, expression)

The fifth axiom establishes that we only have three kinds of expressions with respect to the "isa" relation: one kind is formed by "expression" and its subexpressions, the second kind are those expressions that can "contain" (to use the class terminology) other expressions, and the third kind are those that cannot contain other expressions. In particular, those of the second kind can contain only those of the third kind.

5) forall x, y, z: isa(x, y) & isa(y, z) -> is(z, expression)

The sixth and seventh axioms just define a couple of second kind expressions to begin with:

6) isa(verb, expression)

7) isa(fact, expression)

The eighth axiom simply provides us with the basic form to express non copular sentences.

8) forall x, y, z: isa(f(x, y, z), fact) -> isa(y, verb)


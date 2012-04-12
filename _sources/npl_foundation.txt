
Motivation
==========

The **npl** language may be viewed
as a proof of concept for the solution of a problem
that has been present in logics
from Gottlob Frege to the modern logic programming languages.
Here I will try to introduce this problem
and my proposed solution.

Historical introduction
-----------------------

Gottlob Frege was the first
to develop predicate logic.
And he developed it
in the belief that he was unraveling
the foundations of
the natural logic behind scientific theories
(including the whole of mathematics).
He wanted a mathematical formalism
capable of expressing
any informal scientific theory.
To that end,
he developed a formal theory
in which he had individuals (or values),
and he had classes (or value-ranges),
and he had predicates (what he called concepts).
His theory was second order,
but for our purposes, to simplify,
it can be taken to be equivalent to
a first order axiomatic 'naïve' set theory
with an axiom schema of unrestricted comprehension
(I call it naïve for carrying UC).

In short, our formal naïve set theory is a first order theory
with 2 basic predicates (not counting equality),
those of "belonging to a set", and "being a subset of another set".
These predicates get their form
through a small set of axioms,
basically extensionality,
and a definition of subset in terms of belonging.

Now, if we make a correspondence between these formal set predicates
and the copular use of the verb to be, we have a formal system
where we can express any scientific taxonomy, and reason about it.
Obviously this is not enough to express the logic of any
scientific theory; we need other predicates apart from copular ones,
to represent other relations apart from that of belonging to classes.

First order logic allows us to use more predicates, of course.
We can define new predicates and give them whatever form we like through
additional axioms. Nevertheless, there is still one problem to overcome
if we aspire to a mechanization of the scientific use of the natural
language. And that is the ability to have variables that range over
predicates, the ability to express predicates through constraints,
classes of predicates, etc. In first order predicate logic,
you cannot have variables ranging over predicates. In the natural language,
it is obvious that you can do the equivalent.

For Frege, this problem was solved with unrestriced comprehension,
(or rather, its equivalent in his system,)
that establishes a correspondence between predicates and sets.
Variables range over sets, and UC gives you a set for each predicate,
so you can have "predicate" variables that range over their corresponding sets.
Pehaps, nowadays we would say classes rather than sets,
since they involve UC;
but we could quantify variables over them,
so I think they are more properly called sets.
In any case, Bertrand Russell showed
that UC leads to contradiction in this context,
and cannot be used.

To be accurate, in his system, 
Frege could have variables ranging over predicates,
since his system was second order;
what Russell broke in Frege's system
was his Basic Law V, that corresponds to UC
and allowed him to kind of reduce the system
to first order.
Because the natural logic is obviously first order.

From then on, axiomatic set theory evolved, without UC,
to grow a range of esoteric axioms that quite estranged it from
the copular predicates of natural speech, but which served
logicists to provide a foundation for most of mathematics.
And logical empiricists pursued, without success, the adecuation
of formal logic to the informal logic of natural languages.
And Frege got famously depressed.

Manifestation of the problem
----------------------------

This problem I have described can be seen in many modern
logic programming systems, where any attempt to use
our natural logic as design model for software development is futile.

A paradigmatic example is the OWL language of the semantic web.
This language had two flavours: DL, and full. It is an ontology language
that can be processed by reasoners to extract consecuences.
It has basic class predicates, and you have UC: you can have
anonymous classes defined by predicates. But, in the DL flavour,
you cannot treat classes as individuals. In the full flavour,
you can, but you are not guaranteed that the system will be
consistent under a reasoner. And it is the full flavour that would
provide full (natural) expresivity.

A possible solution
-------------------

My proposition is to use the predicates of set theory
to express the natural copular verbs,
just as Frege (or OWL) did,
but then, instead of representing the rest of the natural verbs
as formal predicates, we represent them through individuals of the
theory. We limit our (first order) theory to only have the
basic predicates of set theory in their barest form, in something
like what follows.

We call this theory NPL.
In it, we only use implication "->"
and conjunction "&"
as logical connectives,
and the only production rule is modus ponens.
Variables are denoted by "x1", "x2"...
and are always universally quantified in their outernmost scope (sentence);
and individuals are denoted by any secuence of lower case letters.
The predicates are "isa" (for belongs to) and "are" (for is
a subset of),
are used in an infix form,
and we have that:

  x1 isa x2 & x2 are x3 -> x1 isa x3

  x1 are x2 & x2 are x3 -> x1 are x3

Now to the representation of natural verbs.
For simplicity, we will only consider natural verbs that represent
binary relations, so a natural sentence with such a verb would have
the form of a triplet subject-verb-object.
To represent this relation, we use a ternary operator "f"
(from fact). So, a non-copular sentence, in our system, would
have the form "f(s, v, o)" (where "s", "v", and "o" are just
individuals of the theory).
Since "f" is an operator, this
sentence represents just another individual of the theory, and has
no truth value.
We will call this sort of individuals "facts".
To attach truth value to facts, we use the set predicates,
to relate them with another individual of the theory,
"fact". So a complete non-copular sentence, in this theory,
would have the form (with prefix operators and infix predicates):

  f(s, v, o) isa fact

Since we only have 3 formal predicates, we do not need UC at all,
and yet we can have variables that range over the equivalents of
our natural verbs (and also over whole "facts").
The point is that we can model the forms of natural logic
with very few predicate and operator symbols,
and that any new term we may want to introduce,
when modelling any kind of discourse,
will be quantifiable by first order variables.
Those symbols that can not be quantified,
like "are" or "isa" or "f",
are so few that do not merit to be so.

We can be even more fine-grained. If we call "predication" to a
pair verb-object, we may want to have variables that range over
them. To do this, we can define a new operator "p", that produces
"predication" individuals, so that now the "f" operator takes 2 operands,
the subject and a "predication", to have something like:

  p(v, o) isa predication

  f(s, p(v, o)) isa fact

And, to show a little more of the power that we can obtain from
such a system, note that facts and predications are individuals
of the theory, so we can use them where we have used "s" or "o",
to build as complex a sentence as we may want (I think it wouldn't make
much sense to use them in place of "v").

An example developed on top of this theory might be (using a primitive
universal set "word"):

  person isa word

  man are person

  john isa man

  woman are person

  sue isa woman

  verb isa word

  loves isa verb

  x1 isa person &
  x2 isa verb &
  x3 isa person &
  f(x1, x2, x3) isa fact
  ->
  f(x3, x2, x1) isa fact

Now, "john loves sue" will imply that "sue loves john".


There is a semantics for this language `here <NL>`_.

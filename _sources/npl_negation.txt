Negation and counting sentences
===============================

We can use 2 forms of negation in **npl**. The first is classical negation,
and the second negation by failure. In both cases we can only talk about
negation of facts; neither predicates nor definitions can be negated.

Classical negation
------------------

::

    assertion : NOT fact DOT

    NOT : "not"

With classical negation, a negated fact is equivalent to a non-negated
fact. You add a negated fact "not P" to the kb in the same way you add
a non-negated fact "P". If you ask (in a query or in the condition of a
rule) for a negated fact, you will only succeed if the negated fact is
in the kb. The fundamental meaning of classical negation lies in
contradiction: you cannot have both a non-negated fact and its negation
in the kb. If you try to do so, the system will not accept it and warn you
of the contradiction. And if 2 rules produce contradictory facts, the
system will break.

Negation by failure
-------------------

Negation by failure relies on a closed world assumption, i.e., on the
assumption that all possible knowledge about the universe of discourse is
present in the kb. All true facts are present in the kb, and all
facts that are not present in the kb are false. So, if we ask a
negated fact "not P", the system need not look for it; it can look for
"P", and if it is not found, our query will succeed.

Counting facts
--------------

::

    arith : COUNT fact

    COUNT : "count"

So, negation by failure is basically a matter of counting facts. **npl**
allows us to count facts in rules. If you count a fact like
``count johnny [goes to hollywood] at 3``, you will obtain either 1 or 0.
So, to set a condition for a negated (by failure) fact, you would add an
arithmetic condition like ``{count johnny [goes to hollywood] at 3 = 0};``.

We can also use variables in fact counts. For example, to count how many
trips there have been to egipt, we would
``count Person1 [trips to egipt] at I1``.

We must stress again that counting facts only makes sense under a closed world
assumption. Under an open world assumption, the same set of facts must produce
the same set of consecuences under any circumstances, whatever the order they
are asserted. 
If we allow counting sentences in rules, the same set of facts
will produce different sets of consecuences depending on the order in
which they are added to the kb. However, if we (as we do in **npl** when we
use the present continuous) assume that the present and the past are closed,
the order in which we add the facts is part of the general truth, and the same
set of facts added in different order would represent different realities.

**NOTE**: Counting sentences is implemented but not yet exposed in **npl**,
because I'm not sure about the syntax.

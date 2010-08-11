
Special conditions and consecuences
===================================

As we have already seen with ``Arith`` in the section on arithmetics, there are some special predicates that we can only use as conditions in rules. Here we shall go over them, and will also mention a special consecuence already hinted at in the section dealing with time expressions, ``Finish``.

Time conditions
---------------

There are in nl a number of predicates that test certain relationships among *time expressions*. For example, whether an instant is whithin a duration, or whether 2 or more durations intersect.

**During**

We can test whether an instant lies between a given duration or list of durations. To do so, we import ``During`` from ``nl``.

  >>> from nl import During

To exemplify this feature, we will define a simple "modal" ontology, using the ``Wants`` verb (defined in the section on verbs and predicates) and the ``Can`` verb defined in the section on arithmetics. This ontology will be given by the additional rule, that translated into English, would assert that, if someone wants to do something at a given moment, and can do it in a duration that covers that moment, (s)he just does it (at that moment):

  >>> r6 = Rule([
  ...   Fact(HumanBeing('H1'), Wants(to=Exists('E1')), Instant('I1')),
  ...   Fact(HumanBeing('H1'), Can(what=Exists('E1')), Duration('D1')),
  ...   During('I1', 'D1'),
  ...   ],[
  ...   Fact(HumanBeing('H1'), Exists('E1'), Instant('I1'))])
  >>> kb.tell(r6)

  >>> bob = HumanBeing('bob')
  >>> kb.tell(bob)

Now, let us further complicate this example. We define a verb "smoke" and a verb "has", a noun "smokable substance", and a couple of substances:

  >>> class SmokableSubstance(Thing): pass
  >>> tobacco = SmokableSubstance('tobacco')
  >>> grass = SmokableSubstance('grass')
  >>> kb.tell(grass, tobacco)

  >>> class Smoke(Exists):
  ...     subject = HumanBeing
  ...     mods = {'what': SmokableSubstance}

  >>> class Has(Exists):
  ...     subject = HumanBeing
  ...     mods = {'what': Thing}

Now, we can define a rule that states that if someone has some smokable substance at some instant, (s)he can smoke it:

  >>> r7 = Rule([
  ...   Fact(HumanBeing('H1'), Has(what=SmokableSubstance('S1')), Instant('I1')),
  ...   ],[
  ...   Fact(HumanBeing('H1'), Can(what=Smoke(what=SmokableSubstance('S1'))), Duration(start='I1'))])
  >>> kb.tell(r7)

With all this in place, we can now assert the following:

  >>> kb.tell(Fact(john, Has(what=tobacco), 5))
  >>> kb.tell(Fact(bob, Has(what=grass), 3))

Furthermore, we assert that they wish:

  >>> kb.tell(Fact(john, Wants(what=Smoke(what=tobacco)), 3))
  >>> kb.tell(Fact(bob, Wants(what=Smoke(what=grass)), 3))

Let us not forget to extend our kb:

  >>> kb.extend()
  3

Now we are ready to ask a few things.

 - Does John smoke tobacco at 3?

  >>> kb.ask(Fact(john, Smoke(what=tobacco), 3))
  False

  No: he wanted, but he did not have tobacco until 5

 - Does John smoke tobacco at 5?

  >>> kb.ask(Fact(john, Smoke(what=tobacco), 3))
  False

  No: he already had tobacco, but he did not want to smoke at that time.

 - Does Bob smoke grass at 3?

  >>> kb.ask(Fact(bob, Smoke(what=grass), 3))
  True

  Yes, at that time he had and wanted.

 - Can John smoke tobacco at 6?

  >>> kb.ask(Fact(john, Can(what=Smoke(what=tobacco)), 6))
  True

  Yes, he had since 5.

 - Can Bob smoke tobacco at 6?

  >>> kb.ask(Fact(bob, Can(what=Smoke(what=tobacco)), 6))
  False

  No, he never had.

 - Can Bob smoke grass at 6?

  >>> kb.ask(Fact(bob, Can(what=Smoke(what=grass)), 6))
  True

  Yes, he had since 3.

Etc. etc. Of course, this is not a very satisfying ontology; we would want to take into account "amounts" had and smoked, consumption of existences, and so on. Defining acceptable ontologies requires some work and iterations until you get it right. But our aim here is not to define acceptable ontologies, only to demonstrate how to use ``nl``.

**Coincide (and Intersection)**

We can test whether a set of durations have an intersection. To do this, we import ``Coincide`` from nl, and use it as a condition in rules giving it any number of durations.

  >>> from nl import Duration, Intersection

With this, we may, for example, build a rule that states that, if two people live in the same place at the same time, they can meet each other. For this, we need a ``Place`` noun, a ``Lives`` verb, and a ``Meets`` verb.

  >>> class Place(Thing): pass

  >>> class Lives(Exists):
  ...     subject = HumanBeing
  ...     instantaneous = False
  ...     mods = {'where': Place}

  >>> class Meets(Exists):
  ...     subject = HumanBeing
  ...     mods = {'who': HumanBeing}

  >>> kb.tell(Rule([
  ...      Fact(HumanBeing('H1'), Lives(where=Place('P1')), Duration('D1')),
  ...      Fact(HumanBeing('H2'), Lives(where=Place('P1')), Duration('D2')),
  ...      Coincide('D1', 'D2'),
  ... ],[
  ...      Fact(HumanBeing('H1'), Can(what=Meets(who=HumanBeing('H2'))),
  ...                Intersection('D1', 'D2')),
  ... ]))

Note the use of ``Intersection``. It is used where a ``Duration`` would be used, and does what might be expected, i.e., produces a duration that is the intersection of any number of durations.

Now we might say:

  >>> england = Place('england')
  >>> eeuu = Place('eeuu')
  >>> kb.tell(england, eeuu)

  >>> kb.tell(Fact(john, Lives(where=england), Duration(start=2, end=7)))
  >>> kb.tell(Fact(mary, Lives(where=england), Duration(start=4, end=9)))
  >>> kb.tell(Fact(bob, Lives(where=eeuu), Duration(start=2, end=9)))

  >>> kb.extend()
  2

And then ask:

  >>> kb.ask(Fact(john, Can(what=Meets(who=mary)), 5))
  True
  >>> kb.ask(Fact(mary, Can(what=Meets(who=john)), 6))
  True
  >>> kb.ask(Fact(mary, Can(what=Meets(who=john)), 8))
  False
  >>> kb.ask(Fact(bob, Can(what=Meets(who=mary)), 5))
  False
  >>> kb.ask(Fact(bob, Can(what=Meets(who=john)), 5))
  False


Negation by failure (Unknown)
-----------------------------

In a rule, you can ask whether something is or is not known. For this, you import ``Not`` from ``nl``:

  >>> from nl import Not

Note that ``Not`` is not strictly "unknown": it is just absence from the knowledge base. Thus, if the negation of the condition is actually known, the ``Not`` condition will be true, just as if it were really unknown. In the "TODO" list is a true ``Unknown`` condition, that will be the conjunction of ``Not`` true and ``Not`` false.

Another issue with ``Not`` is that all variables that appear in the condition must be already bound in previous conditions. It cannot check all the unknowns, since they may be infinite, specially if the time expression is unbound.

Because of these issues, I don't feel like building a rather contrived ontology to give an example of this condition, so for now I will leave this matter at this.


Conjunction and Disjunction
---------------------------

We may import ``And`` and ``Or`` to build conditions that are conjuntions and disjunctions. The conjuntion of 2 conditions is just the same as the 2 separate conditions by themselves; they only make sense when used nested within disjunctions.

**TODO**

Subwords
--------

We can use ``Subword`` to test a "subset relationship" between nouns or verbs. For example, we might have a condition like ``Subword(Noun('N1'), HumanBeing)`` to test whether a certain noun is a "subset" or subword of ``HumanBeing``. As I have been doing in previous sections dealing with ``Noun`` and ``Verb``, I will defer fully exemplifying this question until a later section.


Finalization of the continuous present tense
--------------------------------------------

We can use, as a consecuence in rules, an expression that will terminate the continuous present tense of facts whose duration time expressions end in "now". To do so, we import ``Finish`` from ``nl``:

  >>> from nl import Finish

Now, suppose that we want to assert that, if someone loves someone else, and the lover dies, he stops loving her.

  >>> class Dies(Exists):
  ...     subject = HumanBeing

  >>> kb.tell(Rule([
  ...     Fact(HumanBeing('H1'), Loves(who=HumanBeing('H2')), Duration('D1')),
  ...     Fact(HumanBeing('H1'), Dies(), Instant('I1')),
  ...     During('I1', 'D1'),
  ...     ],[
  ...     Finish('D1', 'I1'),
  ... ]))

Now, if we have that John loves Yoko from 3 onwards, and John dies now, John's love for Yoko terminates now:

  >>> kb.tell(Fact(john, Loves(who=yoko), Duration(start=3, end="now")))
  >>> kb.tell(Fact(john, Dies(), Instant("now")))
  >>> kb.extend()
  1

For now, to make time advace within nl's knowledge base, we have to execute the function ``now``. This is something which I wouldn't like ending up in the 1.0 API, though:

  >>> from nl import now
  >>> now()
  1281517957.0

  >>> kb.ask(Fact(john, Loves(who=yoko), Instant(5)))
  True

  >>> kb.ask(Fact(john, Loves(who=yoko), Instant("now")))
  False


.. _Python: http://www.python.org/

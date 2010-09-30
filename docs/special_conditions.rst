
Special conditions and consecuences
===================================

As we have already seen with ``Arith`` in the section on arithmetics, there are some special predicates that we can only use as conditions in rules. Here we shall go over them, and will also mention a special consecuence already hinted at in the section dealing with time expressions, ``Finish``.

Time conditions
---------------

There are in nl a number of predicates that test certain relationships among *time expressions*. The reason we don't just use arithmetic conditions to test these relations is the special treatment of the present time when using time.

For example, we can test whether an instant is whithin a duration, or whether 2 or more durations intersect.

**During**

We can test whether an instant lies between a given duration or list of durations. To do so, we import ``During`` from ``nl``.

  >>> from nl import During

To exemplify this feature, we will define a simple "modal" ontology, using the ``Wants`` verb (defined in the section on verbs and predicates) and the ``Can`` verb defined in the section on arithmetics. This ontology will be given by the following rule, that translated into English, would assert that, if someone wants to do something at a given moment, and can do it in a duration that covers that moment, (s)he just does it (at that moment):

  >>> r6 = Rule([
  ...   Fact(HumanBeing('H1'), Wants(to=Exists('E1')), Instant('I1')),
  ...   Fact(HumanBeing('H1'), Can(what=Exists('E1')), Duration('D1')),
  ...   During('I1', 'D1'),
  ...   ],[
  ...   Fact(HumanBeing('H1'), Exists('E1'), Instant('I1'))])
  >>> kb.tell(r6)

  >>> bob = HumanBeing('bob')
  >>> kb.tell(bob)

Now, let us further complicate this example. We define a verb "drinks" and a verb "has", a noun "drinkable substance", and a couple of substances:

  >>> class DrinkableSubstance(Thing): pass
  >>> beer = DrinkableSubstance('beer')
  >>> coffee = DrinkableSubstance('coffee')
  >>> kb.tell(beer, coffee)

  >>> class Drinks(Exists):
  ...     subject = HumanBeing
  ...     mods = {'what': DrinkableSubstance}

  >>> class Has(Exists):
  ...     subject = HumanBeing
  ...     mods = {'what': Thing}

Now, we can define a rule that states that if someone has some drinkable substance at some instant, (s)he can drink it:

  >>> r7 = Rule([
  ...   Fact(HumanBeing('H1'), Has(what=DrinkableSubstance('S1')), Instant('I1')),
  ...   ],[
  ...   Fact(HumanBeing('H1'), Can(what=Drinks(what=DrinkableSubstance('S1'))), Duration(start='I1'))])
  >>> kb.tell(r7)

With all this in place, we can now assert the following:

  >>> kb.tell(Fact(john, Has(what=beer), 5))
  >>> kb.tell(Fact(bob, Has(what=coffee), 3))

Furthermore, we assert that they wish:

  >>> kb.tell(Fact(john, Wants(what=Drinks(what=beer)), 3))
  >>> kb.tell(Fact(bob, Wants(what=Drinks(what=coffee)), 3))

Let us not forget to extend our kb:

  >>> kb.extend()
  3

Now we are ready to ask a few things.

 - Does John drink beer at 3?

  >>> kb.ask(Fact(john, Drinks(what=beer), 3))
  False

  No: he wanted, but he did not have beer until 5

 - Does John drink beer at 5?

  >>> kb.ask(Fact(john, Drinks(what=beer), 3))
  False

  No: he already had beer, but he did not want to drink at that time.

 - Does Bob drink coffee at 3?

  >>> kb.ask(Fact(bob, Drinks(what=coffee), 3))
  True

  Yes, at that time he had and wanted.

 - Can John drink beer at 6?

  >>> kb.ask(Fact(john, Can(what=Drinks(what=beer)), 6))
  True

  Yes, he had since 5.

 - Can Bob drink beer at 6?

  >>> kb.ask(Fact(bob, Can(what=Drinks(what=beer)), 6))
  False

  No, he never had.

 - Can Bob drink coffee at 6?

  >>> kb.ask(Fact(bob, Can(what=Drinks(what=coffee)), 6))
  True

  Yes, he had since 3.

Etc. etc. Of course, this is not a very satisfying ontology; we would want to take into account "amounts" had and drunk, consumption of existences, and so on. Defining acceptable ontologies requires some work and iterations until you get it right. But our aim here is not to define acceptable ontologies, only to demonstrate how to use ``nl``.

**Coincide (and Intersection)**

We can test whether a set of durations have an intersection. To do this, we import ``Coincide`` from nl, and use it as a condition in rules giving it any number of durations.

  >>> from nl import Duration, Intersection

With this, we may, for example, build a rule that states that, if two people live in the same place at the same time, they can meet each other. For this, we need a ``Place`` noun, a ``Lives`` verb, and a ``Meets`` verb.

  >>> class Place(Thing): pass

  >>> class Lives(Exists):
  ...     subject = HumanBeing
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


.. _Python: http://www.python.org/


Arithmetics
===========

We have already seen, in the section speaking about predicates, that we can use ``Number`` to use numbers. But we are not restricted to simple integers when using ``Number``: we can also use arithmetic operations (with "parenthesized" polish notation, rooted in the internal use of CLIPS_). For example, we might have:

  >>> n = Number('(+ 2 3)')
  >>> n2 = Number('(+ 2 (* 5 6))')

We can't use this as a calculator, though; after all, this is Python_, and Python_ is in itself enough of a calculator. So, if we resolve ``n`` to a string, we will get the actual operation:

  >>> str(n)
  '(+ 2.0 3.0)'

As you can see, I am at the moment using floats in CLIPS_, which I tend to think is a mistake, so for the moment I am leaving floats out of the official API.

Arithmetics in nl is intended to be used whithin rules. So, for example, using the ``Runs`` verb defined in the section on verbs and predicates, and defining a ``Displaces`` verb defined as follows, we might have a rule such as the following:

  >>> class Displaces(Exists):
  ...     subject = HumanBeing
  ...     mods = {'d': Number}

  >>> r4 = Rule([Fact(HumanBeing('H1'), Runs(v='V1'), Duration(start='T1', end='T2')),
  ...      ],[
  ...      Fact(HumanBeing('H1'), Displaces(d='(* V1 (- T2 T1))'), Instant('T2'))])
  >>> kb.tell(r4)

Now we can do something like this:

  >>> kb.tell(Fact(mary, Runs(v=8), Duration(start=2, end=4)))
  >>> kb.extend()
  1
  >>> kb.ask(Fact(mary, Displaced(d=16), Instant(4)))
  True

At the moment, all arithmetic operations are binary, and we can only use the following operators (more to come, it's in the TODO list to allow all arithmetics provided by CLIPS_):

 - addition: ``+``;
 - substraction: ``-``;
 - multiplication: ``*``;
 - division: ``/``;
 - power: ``**``.

*NOTE*
As we have seen, we can mix instants and numbers in arithmetic operations. There is a caveat here, though: if we use "now" as the end instant of a duration, and use it in rules that use that duration arithmetically, we shall certainly get unexpected results. This is an unsolved problem at the moment, (that would have to be solved at the CLIPS_ level) and the only current solution is not to mix those features.

Arithmetic conditions
---------------------

We can also use arithmetics to establish conditions in rules. To do so, we must import ``Arith`` from ``nl``. Again, all arithmetic predicates are binary, using polish notation, and the allowed predicates are:

 - equality: ``=``;
 - inequality: ``!=``;
 - less than: ``<``;
 - less or equal than: ``<=``;
 - greater than: ``>``;
 - greater or equal than: ``>=``;

Let's give an example, such as "a person can only enter the 'Momentos' club if older than 18". For this, we will define 3 verbs: "aged", "enter", and "can", a noun "club", and a club "momentos".

  >>> from nl import Arith
  
  >>> class Aged(Exists):
  ...     subject = HumanBeing
  ...     mods = {'years': Number}

  >>> class Club(Thing): pass
  >>> class Enter(Exists):
  ...     subject = HumanBeing
  ...     mods = {'where': Club}

  >>> momentos = Club('momentos')
  >>> kb.tell(momentos)

  >>> class Can(Exists):
  ...     subject = HumanBeing
  ...     mods = {'what': Exists}
  ...     instantaneous = False

Now we define our rule:

  >>> r5 = Rule([
  ...      Fact(HumanBeing('H1'), Aged(years='X1'), 'I1'),
  ...      Arith('(> X1 18)')
  ...           ],[
  ...      Fact(HumanBeing('H1'), Can(what=Enter(where=momentos)), Duration(start='I1', end='now'))])
  >>> kb.tell(r5)

As can be expected from our discussion on *time  expressions*, the duration given to the consecuence of the previous rule could be tranlated to English as "from I1 onwards".

If we now assert these couple of facts:

  >>> sean = HumanBeing('sean')
  >>> kb.tell(sean)
  >>> kb.tell(Fact(john, Aged(years=40), 'now'))
  >>> kb.tell(Fact(sean, Aged(years=4), 'now'))
  >>> kb.extend()
  1

We have that

  >>> kb.ask(Fact(john, Can(what=Enter(where=momentos)), 'now'))
  True

  >>> kb.ask(Fact(sean, Can(what=Enter(where=momentos)), 'now'))
  False

Note that, in queries, we can provide an instant in a fact that has a non-instantaneous verb. nl will answer taking into account whether that instant is whithin the interval that it has stored in its kb.


.. _CLIPS: http://clipsrules.sourceforge.net/
.. _Python: http://www.python.org/

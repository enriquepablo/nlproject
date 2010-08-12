
Facts
=====

To build facts, that can then be entered into the knowledge base, we must use the class ``Fact``, imported from ``nl``:

  >>> from nl import Fact

Facts are then built by instantiating ``Fact`` with a subject, a predicate, a time expression, and a negation bit. For example:

  >>> john = HumanBeing('john')
  >>> yoko = Woman('yoko')
  >>> kb.tell(john, yoko)
  >>> f1 = Fact(john, Loves(who=yoko), Duration(start=0,end='now'), truth=1)

We cannot omit the subject or the predicate. If we don't provide the *time expression*, it will default to ``Instant('now')``, and if we omit the negation bit, it will default to true, or ``1``.

Facts have to be "told" to the knowledge base; we must use ``tell`` to enter them, so that later rules and queries may be aware of them.

  >>> kb.tell(f1)

Facts are really a special kind of predicate. In fact, we might define a predicate that has the same form as a fact in its given modifiers, e.g.:

  >>> class Says(Exists):
  ...     mods = {'subject': HumanBeing,
  ...             'predicate': Exists,
  ...             'start': Number,
  ...             'end': Number,
  ...             'truth': Number}

They are special and differentiated for two reasons.

 #. The first reason is their truth value, that can give rise to contradictions when two facts, identical except for their negation bit, are present in the same knowledge base.
 #. The second reason is the special treatment of time in facts. In particular, as we have seen in the previous section (on time expressions), the treatment of "now" as a continuous present when given as the end of a duration.

There is not really much more to be said about facts. They are composed of the kinds of constructs that we have already seen, and any combination of them will be enough to build a fact.

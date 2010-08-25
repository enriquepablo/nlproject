
Verbs and predicates
====================

Definition of verbs
-------------------

nl provides a primitive verb, ``Exists``, that we extend to define other verbs. So, to define our verbs, the first thing we need to do is to import ``Exists`` from ``nl``:

  >>> from nl import Exists

Defining verbs, like defining nouns, affects the knowledge base, through the metaclasss of ``Exists``, i.e. ``Verb``:

  >>> from nl import Verb
  >>> type(Exists) is Verb
  True

The definition of verbs is a little more complex than the definition of common nouns. There are three class attributes we may want to define for our verbs (all three are optional). These attributes set limits to the subjects and time expressions of facts in which instances of our verbs take part (as predicates), and to the modifiers that they can take.

 #. The first attribute is ``subject``. This is assigned to a ``Noun``, i.e., to ``Thing`` or to a class derived from it. So, for example, if we define a verb ``Loves`` and give it as subject ``HumanBeing``, any fact that has ``Loves`` as a verb in its predicate, must have a ``HumanBeing`` as subject. If we don't set the ``subject`` of a verb, it will default to ``Thing``, so the subject in a fact with that verb could be anything.
 #. Then we can provide our class with an attribute called ``instantaneous``, and assign to it a boolean value. If ``True``, facts constructed upon the defined verb will have an instant as time expression, and if ``False``, they will have an interval. If not assigned, ``instantaneous`` will default to ``True``.
 #. Finally, we define the modifiers that the verb can take when used in a predicate. To do this, we assign a dictionary to a ``mods`` attribute on the class. The keys of this dictionary must be strings, and will correspond to the names of those modifiers. The values of this predicate must be classes, so that the corresponding named modifiers of the verb in a predicate will be objects of that class. If the ``mods`` attribute is not given, it will default to an empty dictionary.

  >>> class Loves(Exists):
  ...     subject = HumanBeing
  ...     instantaneous = False
  ...     mods = {'who': HumanBeing}

Predicates
----------

Now, we might instantiate ``Loves`` to produce a predicate:

  >>> loves_mary = Loves(who=mary)

As is the case with things, this does not alter the knowledge base. And, contrary to the case with things, in the normal use of nl we do not ``tell`` predicates. We use predicates to build facts, which are then entered into the knowledge base through ``tell``. There is no theoretical or technical reason for that, it is simply not implemented because I don't see any practical use for it.

A verb can have more than one item in the ``mods`` dictionary. For example, we might have defined a noun "amount" and given a few objects of the class, and we might have given an item "how_much" to the ``mods`` dictionary of ``Loves``:

  >>> class Amount(Thing): pass
  >>> a_lot = Amount('a_lot')
  >>> a_little = Amount('a_little')
  >>> very_little = Amount('very_little')
  >>> kb.tell(a_lot, a_little, very_little)
  
  >>> class Loves2(Exists):
  ...     subject = HumanBeing
  ...     instantaneous = False
  ...     mods = {'who': HumanBeing,
  ...             'how_much': Amount}

Then, we might have a predicate like:

  >>> loves_mary_a_lot = Loves2(who=mary, how_much=a_lot)

However, we do not have to provide all the modifiers suggested in the definition of the verb, and might also have predicates like:

  >>> loves2_mary = Loves2(who=mary)
  >>> loves_a_lot = Loves2(how_much=a_lot)

As is also the case with things, we are not forced to extend ``Exists`` directly, and can extend from any other verb, so we might have:

  >>> class Loves3(Loves):
  ...     mods = {'how_much': Amount}

In this case, we would have a verb, ``Loves3``, that has virtually the same form as ``Loves2``, and with it we might build predicates exactly with the same form as the above ``loves2_anne`` or ``loves_a_lot``. Thus, the modifiers that we may give to a verb to produce a predicate are not just given by the ``mods`` dictionary of its class, but by the union of all the ``mods`` dictionaries of its class, and all classes in its class' ``__bases__``.

Classes of modifiers
--------------------

In the previous sections we have defined verbs that have, as modifiers, ``Thing`` instances. We do not have to limit ourselves to this kind of objects, though. We can, for example, have a verb class with a modifier that extends ``Exists``, so that the predicates that instantiate it have other predicates as modifiers. For example:

  >>> class Wants(Exists):
  ...     subject = HumanBeing
  ...     mods = {'what': Exists}

With this ``Wants`` verb, we might build predicates such as:

  >>> wants_to_luv_mary = Wants(what=Loves(who=mary))

Another important class of modifier we may provide a verb with are numbers. Instead of the rather absurd ``Amount`` class we defined earlier, we might have imported ``Number`` from ``nl``, and provided it as value for the key ``'how_much'``. For example:

  >>> from nl import Number
  >>> class Runs(Exists):
  ...     subject = HumanBeing
  ...     instantaneous = False
  ...     mods = {'v': Number}

  >>> r1 = Runs(v=1)

or, also, more inconveniently:

  >>> r2 = Runs(v=Number(2))

To finish this section, we will add that we can also provide ``Verb`` or ``Noun`` as values in the ``mods`` dictionary of a verb definition, so that in predicates, the modifiers of such verbs will be actual subclasses of ``Thing`` or ``Exists`` rather than instances of them.

I will defer giving examples of this feature until a later section, in which I will provide a complete "real world example" of the usage of nl.


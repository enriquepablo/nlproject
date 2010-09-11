
Rules - basic
=============

Rules have 2 parts: the conditions, and the consecuences. Both are given mainly by lists of sentences (copulas or facts), though some other constructs can be used in those lists. For example, we can provide arithmetic conditions, or we can give "present tense terminating expressions" as consecuences.

To define rules, first thing is to import ``Rule`` from ``nl``.

  >>> from nl import Rule

  >>> r1 = Rule([
  ...   Fact(john, Loves(who=yoko)),
  ...   ],[
  ...   Fact(yoko, Loves(who=john))])

As always, we have to ``tell`` our rule into the kb:

  >>> kb.tell(r1)

Having the condition from the previous section on facts, we can ask the kb about the consecuence:

  >>> kb.ask(Fact(yoko, Loves(who=john)))
  False

But we get a ``False`` answer. Why? Because we have not extended the knowledge base. Whenever we want to have consecuences in our knowledge base, we have to extend it. This is done through the ``extend`` function in ``kb``:

  >>> kb.extend()
  1

``extend`` gives back the number of consecuences added to the knowledge base. And it is now that we can ask our question:

  >>> kb.ask(Fact(yoko, Loves(who=john)))
  True

Variables
---------

The main difference between standalone sentences and sentences in rules is that we can use variables in sentences within rules. These variables are typical universally quantified logical variables (except in certain "sentence counting" conditions where they can appear as free variables, as we shall see in a later section). Their role in conditions is pattern matching; their role in consecuences is their substitution for the objects that have matched with them in the conditions; and their scope is the rule in which they appear.

Variables are given by a string, and are recognized by their form: A variable starts with an upper case alphabetical character, followed by any number of word characters, and ends in one or more digits. The regular expression pattern that identifies them is ``r'^[A-Z]\w*\d+$'``.

Next, we shall see different types of variables: thing variables, predicate variables, noun variables, verb variables. However, it is important to note that they all are actually first order logical variables. They are only different in Python_; in CLIPS_, they are all the same, i.e. CLIPS_ variables, and the classification available in Python_ translates into the underlying CLIPS_ as a certain binding. Thus, nl's typification of variables is just synctacic sugar. For example, if we have a thing variable such as ``Woman('W1')``, the underlying CLIPS_ construct would be something with the form "for all ?W1, where ?W1 is a woman...".

**Thing variables.**

We may provide thing variables by instantiating ``Thing`` (or one of its subclasses) with a string with the form indicated above. An example might be ``Thing('X1')``. Another example might be ``Woman('Woman1')``. A thing variable can appear in any place in a sentence whithin a rule where a concrete thing might appear in a standalone sentence. So, if we want to generalize the rule given above to state, not just that if John loves Yoko then it follows that Yoko loves John, but that if John loves any woman, that woman is bound to also love him, we might say:

  >>> r2 = Rule([
  ...   Fact(john, Loves(who=Woman('W1'))),
  ...   ],[
  ...   Fact(Woman('W1'), Loves(who=john))])
  >>> kb.tell(r2)

With that rule, we will have exactly the same consecuence as with rule ``r1``, once we enter the fact that John loves Yoko; but we will also get a consecuence if we say that John loves Linda, i.e., that Linda loves John:

  >>> kb.tell(Woman('linda'))
  >>> kb.tell(Fact(john, Loves(who=Woman('linda'))))
  >>> kb.extend()
  1
  >>> kb.ask(Fact(Woman('linda'), Loves(who=HumanBeing('john'))))
  True

We can of course use more than one variable withn a rule. So, further generalizing, we might express a rule that simply states that love for a woman is always corresponded:

  >>> r3 = Rule([
  ...   Fact(HumanBeing('Hb1'), Loves(who=Woman('W1'))),
  ...   ],[
  ...   Fact(Woman('W1'), Loves(who=HumanBeing('Hb1')))])
  >>> kb.tell(r3)

  >>> kb.tell(HumanBeing('paul'))
  >>> kb.tell(Fact(HumanBeing('paul'), Loves(who=Woman('yoko'))))
  >>> kb.extend()
  1
  >>> kb.ask(Fact(yoko, Loves(who=HumanBeing('paul'))))
  True

We may use aritmetic and time variables in just the same way we use thing variables, and have constructs such as ``Number('N1')``, ``Instant('I1')``, or ``Duration('D1')``.

**Predicate variables**

We have seen that we can use thing variables as verb modifiers within predicates. We can also have predicate variables. To do so, we instantiate a verb class with an unnamed variable string. In the most general case, we might use ``Exists('E1')``. Note the difference with the common use of verbs: we instantiate the verb not with named modifiers (nl objects), but with an unnamed string matching the *variable* regular expression given above.

In this sense, suppose we want to assert that John does whatever he wants to do:

  >>> r4 = Rule([
  ...   Fact(john, Wants(to=Exists('E1'))),
  ...   ],[
  ...   Fact(john, Exists('E1'))])
  >>> kb.tell(r4)

With this rule in place, we would have, for example:

  >>> kb.tell(Fact(john, Wants(to=Loves(who=yoko))))
  >>> kb.extend()
  1
  >>> kb.ask(Fact(john, Loves(who=yoko)))
  True

**Word (Noun and Verb) variables**

In the section dealing with predicates, we saw that we can use, as modifiers for verbs in predicates, not just things and and predicates, but also nouns and verbs. In this sense, we can use in rules variables that range over nouns and verbs, and place them, not just as modifiers for verbs in predicates, but also as proper name constructors or as verbs in predicates. So, for example, we might have variables such as ``Noun('N1')``, to range over any noun, ``Noun('N1', HumanBeing)`` to range over nouns derived from ``HumanBeing``, and ``Noun('N1', HumanBeing)('H1')`` to range over actual human beings.

I defer giving example rules using these kinds of variables until a later section in which I will provide a single complete real world ontology built with nl.


.. _PyCLIPS: http://pyclips.sourceforge.net/
.. _CLIPS: http://clipsrules.sourceforge.net/
.. _Python: http://www.python.org/

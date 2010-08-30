
Counting Sentences
==================

Let's suppose we are talking about a number of people, like:

  >>> sue = Woman('sue')
  >>> joanna = Woman('joanna')
  >>> dawn = Woman('dawn')
  >>> eva = Woman('eva')
  >>> sarah = Woman('sarah')
  >>> kb.tell(sue, joanna, dawn, eva, sarah)

  >>> kb.tell(Fact(john, Loves(who=yoko), Duration(start=1 ,end=2)))
  >>> kb.tell(Fact(john, Loves(who=sue), Duration(start=2 ,end=3)))
  >>> kb.tell(Fact(john, Loves(who=joanna), Duration(start=3 ,end=4)))
  >>> kb.tell(Fact(john, Loves(who=dawn), Duration(start=4 ,end=5)))
  >>> kb.tell(Fact(john, Loves(who=eva), Duration(start=5 ,end=6)))
  >>> kb.tell(Fact(bob, Loves(who=sarah), Duration(start=1 ,end=4)))
  >>> kb.tell(Fact(bob, Loves(who=dawn), Duration(start=4 ,end=6)))

Now, we might want to make rules about people who have loved 4 women, or about the person who has loved the most women, or about the number of women that has loved the person who has loved the least women. To do so, we can only count sentences. We might of course count them by hand and add redundant new sentences to state what we have found, but the information was already there. So nl allows us constructs to count sentences that are in the knowledge base and confom to a given pattern.

For this bussiness of counting sentences, there are 4 classes that we can use in the conditions of rules, in any place where a number might have been used: ``Count``, ``UniqueCount``, ``MaxCount``, and ``MinCount``. In all cases, we provide as arguments to the constructor any number of sentences. The variables that take part in these sentences can be of 2 types: bound, and free. Bound variables are those that appear in previous conditions, and free variables those that do not.

Count
-----

Constructs built with ``Count`` will count all (sets of) sentences that comply with the provided pattern. For example, ``Count(Fact(HumanBeing('X1'), Loves(who=Woman('W1')), Duration('D1')))`` will give ``7`` (the number of sentences where it is stated that someone loves someone else), and ``Count(Fact(john, Loves(who=Woman('W1')), Duration('D1')))`` will give ``5`` (the number of sentences where it is stated that John loves someone).

UniqueCount
-----------

Constructs built with ``UniqueCount`` require, appart from the sentence patterns to be counted, a list of variables that are required to be unique (that are free in the given sentences), and will count the number of sentences that match the given pattern and have distinct values for the variables that are required to be unique. For example, ``UniqueCount(('X1',), Fact(HumanBeing('X1'), Loves(who=Woman('W1')), Duration('D1')))`` will give ``2`` (the number of different subjects of sentences where it is stated that someone loves someone else), and ``UniqueCount(('W1',), Fact(HumanBeing('X1'), Loves(who=Woman('W1')), Duration('D1')))`` will give ``6`` (the number of women that are stated to be loved).

MaxCount and MinCount
---------------------

Constructs made with ``MaxCount`` or ``MinCount`` require a single variable as first argument, that is free in the sentences to be counted, and will count the maximum (or minimum) number of sentences that match the given pattern and have the same value for the given variable. For example, ``MaxCount('X1', Fact(HumanBeing('X1'), Loves(who=Woman('W1')), Duration('D1')))`` will give ``5`` (the number of women that john is stated to love, john being the maximum lover), and ``MinCount('W1', Fact(HumanBeing('X1'), Loves(who=Woman('W1')), Duration('D1')))`` will give ``1`` (the number of times the least loved women are loved).


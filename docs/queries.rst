
Queries
=======

Queries to the knowledge base are made in nl, as we have already seen in previous sections, through the ``ask`` function in the ``kb`` module.

There are two ways of performing queries with ``ask``. The fisrt and simplest is to give it as an argument a fact with no variables. This will return ``True`` or ``False`` depending on whether the given fact is within the knowledge base or not, and we have seen plenty of examples in previous sections.

The second way is using variables in the asked facts. In this usage, we have to provide ``ask`` with a few unnamed arguments of 2 kinds. The first kind are the variables we want to extract, and the second kind are the facts in which these variables are used. In this case, ``ask`` will return a dictionaries with the names of the variables as keys and the matched objects as values. Let's see a few examples.

We may want to ask when does John loves Yoko:

  >>> kb.ask(Duration('D1'), Fact(john, Loves(who=yoko), Duration('D1')))
  [{'D1': 'from 3.0 till 1281517624.0'}]

Or who ever loves Yoko:

  >>> kb.ask(HumanBeing('H1'), Fact(HumanBeing('H1'), Loves(who=yoko), Duration('D1')))
  [{'H1': 'john', 'D1': 'from 3.0 till 1281517624.0'}]

Also, who ever loves Yoko and when:

  >>> kb.ask(Duration('D1'), HumanBeing('H1'), Fact(HumanBeing('H1'), Loves(who=yoko), Duration('D1')))
  [{'H1': 'john', 'D1': 'from 3.0 till 1281517624.0'}]

If there are more people who love Yoko, the answer will be a list of dictionaries:

  >>> kb.tell(Fact(sean, Loves(who=yoko), Duration(start=2, end=5)))

  >>> kb.ask(Duration('D1'), HumanBeing('H1'), Fact(HumanBeing('H1'), Loves(who=yoko), Duration('D1')))
  [{'H1': 'john', 'D1': 'from 3.0 till 1281517624.0'}, {'H1': 'sean', 'D1': 'from 2.0 till 5.0'}]

Finally, we can provide more facts in the query, possibly using some of the already given variables, and ``ask`` will "and" the facts to provide the answers for which all facts are true:

  >>> kb.ask(Duration('D1'), HumanBeing('H1'), Fact(HumanBeing('H1'), Loves(who=yoko), Duration('D1')), Fact(HumanBeing('H1'), Lives(where=england), Duration(start=2, end=7)))
  [{'H1': 'john', 'D1': 'from 3.0 till 1281517624.0'}]

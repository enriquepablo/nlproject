
Common nouns and copulative sentences
=====================================

Definition of common nouns
--------------------------

nl provides a primitive common noun, ``Thing``. To define further common nouns, you import ``Thing`` from ``nl`` and extend it:

  >>> from nl import Thing
  >>> class HumanBeing(Thing): pass

Such a class definition would be morphologically (and, thus, possibly semantically) equivalent to asserting, in English, that human beings are things. Note that it is being asserted, i.e., something is being pushed into nl's knowledge base. This is due to the fact that ``Thing`` has a metaclass (``Noun``) that, among other things, touches the knowledge base in its ``__init__`` method.

  >>> from nl import Noun
  >>> type(Thing) is Noun
  True

Thus, with this simple class definition, we are actually asserting a copulative sentence, "human beings are things". There are two kinds of copulas that we can build with nl: those in which the subject is a common noun, such as the one above, and those in which the subject is a proper name, which we shall see shortly.

The classes derived from ``Thing`` can be further subclassed to provide for all the common nouns that we may need for our *ontology*.

  >>> class Woman(HumanBeing): pass
  >>> class Girl(Woman): pass

to assert that a woman is a human being, and a girl is a woman.

Definition of proper names
--------------------------

Copulative sentences with proper names as subjects are built by instantiating ``Thing`` derived classes. So if you want to say, for example, that Mary is a woman, you would instantiate ``Woman`` with a string representing her proper name:

  >>> mary = Woman('mary')

The string has to be a legal Python_ name: its characters must belong to the ASCII set, it cannot contain spaces, dashes, slashes, etc.

Note that the instantiation and assignment above does not touch the knowledge base. It just gives us an object that has some internal (private in the loose pythonic sense of "private") methods that allow nl to manipulate its knowledge base. To actually assert that Mary is a woman, we first have to import ``kb`` from ``nl``:

  >>> from nl import kb

``kb`` is a simple module that provides a couple of functions to interface with nl's knowledge base. The function we want to use here is ``tell``. Of course, we might also import ``tell`` from ``nl.kb``. So, to finally assert our copula, we would:

  >>> kb.tell(mary)

We can also obviously (I say "obviously", and "of course" a little above, because this things are due to the nature of Python_, and not of nl) instantiate ``Woman`` in the call to ``tell``, like:

  >>> kb.tell(Woman('anne'))

However, we may want to get hold of the ``mary`` (or ``anne``) object to use it later in the construction of facts (and rules) that talk about Mary (or Anne). This is just a convenience, though: If we later instantiate ``Woman`` again with the same ``'mary'`` string, the resulting object will correspond to the same CLIPS_ construct within the knowledge base.

We may also add that the (non-) capitalization of proper names is optional, but, since conventionally classes in Python_ are capitalized, I have chosen to not capitalize my proper names.

Not all concrete or individual things that we talk about in English have a proper name. Most often, we use determinate articles combined with common nouns to refer to them: this cat, that cat, or even that other cat. In nl, we convert those expressions into convenience proper names, and would use as proper names, for example, ``'that_cat'``, ``'that_other_cat'``, or ``'cat1'`` and ``'cat2'``, or whatever may be convenient.

Plurals, that refer to sets, lists ordered or unordered, etc., are not built in in nl. Therefore we have to define them in our ontologies as we see fit, as we shall (probably) see in another section.

As a conclussion to this section, we may say that the English copular verb, explicitly given by "to be", is implicit in nl. In English, "to be" establishes a structure that could be modelled with a very simple set theoretic system [#]_. In Python_ (and in CLIPS_'s COOL), the class system (or object orientation) establishes a similarly simple set structure, which is what we correspondingly use. Our ``mary`` object is not just a woman, it is also a human being and a thing, and will thus be affected by rules that talk about human beings and things. Obviously:

  >>> isinstance(mary, HumanBeing)
  True
  >>> isinstance(mary, Thing)
  True


**FOOTNOTES:**


.. [#] I say simple in the sense that it implies no constructive axioms, such as unrestricted comprehension in formal naïve set theory (as given by `Gottlob Frege`_), or those added (Axiom of choice, etc) to the set theories developed after the fall of the naïve theory at the hands and genious of `Bertrand Russell`_. Thus, what I refer to with simple set theory may be given by just the axiom of extensionality and the definition of subset, and possibly the definition of some distinguished individuals such as the empty set or the universal set.

.. _`Bertrand Russell`: http://en.wikipedia.org/wiki/Bertrand_Russell
.. _`Gottlob Frege`: http://en.wikipedia.org/wiki/Gottlob_Frege
.. _CLIPS: http://clipsrules.sourceforge.net/
.. _Python: http://www.python.org/

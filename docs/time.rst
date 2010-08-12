
Time expressions
================

There are 2 kinds of time expressions in nl: instants, and durations (intervals of time). They just basically boil down to integers and pairs of integers, except for some special treatment of the "now" concept.

Instants
--------

An instant is really simply a natural number. To create an instant, you import ``Instant`` from ``nl``, and instantiate it with an non-negative integer. ``0`` will in principle correspond to the start of the 1rst of January of 1970 (though we may configure nl so that ``0`` corresponds to any other historical or non-historical time, as we shall see below **TODO**), and any other integer, to a time n seconds after that historical time.

  >>> from nl import Instant
  >>> i1 = Instant(1)

As is the case with predicates, you don't ``tell`` instants (or durations); that would be rather pointless. You use them to build durations, or facts that use verbs that are ``instantaneous``.

Now
----

Appart from instantiating them with integers, you can also instantiate instants with the special string ``"now"``, and that will be the same as instantiating them with the number of seconds since the starting of time, i.e., it will be get the value of the present time.

  >>> from time import time
  >>> now = Instant('now')
  >>> now2 = Instant(int(time()))

With this, ``now``, and ``now2`` will be approximately the same.

The present time (i.e., the number of seconds since the starting of time) is stored as an internal  Python_ variable, and to make time pass (to change that variable) we must at the moment call a function ``now``, either with an integer to explicitly set the present time, or with no arguments to set the present time according to the ``time`` function of the ``time`` module of Python_'s standard library:

  >>> from nl import now
  >>> now(2)
  2.0
  >>> now()
  1281605116.0

This allows us to control what sentences are asserted "at the same time". I'm not entirely sure about this, but I think that the need to explicitly call ``now`` will be configurable in the 1.0 API of nl.

Durations
---------

A duration is composed of 2 instants, where the first instant is a smaller integer than the second. To create a duration, you import ``Duration`` from ``nl``, and implement it with an instant named ``start`` and another instant named ``end``.

  >>> from nl import Duration
  >>> d1 = Duration(start=Instant(1), end=Instant(2))

Also:

  >>> d2 = Duration(start=2, end=3)

We have to name the start and end arguments, since, as we shall see later, we can instantiate ``Duration`` as a logical variable, and not naming the arguments would confuse nl.

There is a special treatment of "now" in durations. If the start of a duration is now, it will be assigned the integer corresponding to that particular instant in the historical time. However, if the end of a duration is now, it will adopt a special value, (actually, ``-1``), corresponding to a continuous present. This way, a fact that has a duration that ends in "now" will have a continuous present tense. There are ways, in rules, to terminate that continuous present, and so convert those sentences to a past tense. This is the non-monotonic trick we spoke about in the introduction to this documentation. The monotonicity of the whole system is however kept, since there is never a reduction in the number of sentences within the knowledge base. We shall delve more on this issue after we introduce the way we use rules in nl.

**FOOTNOTES**

[#]_ I say in principle, because that is just a convention, and we may interpret ``0`` as any particular time 


.. _Python: http://www.python.org/

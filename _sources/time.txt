
Time expressions
================

Before exposing how we speak about time in nl, we have to say that we don't necessarily have to take time into account when using nl. If we want to talk about geometry, for example, we may ignore time completely, and build facts without the time argument, so that our discourse wil be outside time (or, what is the same, within a single and unique instant).  That said, its time for time.

There are 2 kinds of time expressions in nl: instants, and durations (intervals of time). They just basically boil down to integers and pairs of integers, except for some special treatment of the "now" concept which we shall see in a later section. They are used to build facts, to convey the tense and aspect of the verbs.

Instants
--------

An instant is really simply a natural number. To create an instant, you import ``Instant`` from ``nl``, and instantiate it with an non-negative integer.

  >>> from nl import Instant
  >>> i1 = Instant(1)

As is the case with predicates, you don't ``tell`` instants (or durations); that would be rather pointless. You use them to build durations or facts.

Durations
---------

A duration is composed of 2 instants, where the first instant is a smaller integer than the second. To create a duration, you import ``Duration`` from ``nl``, and instantiate it with an ``Instant`` named ``start`` and another ``Instant`` named ``end``.

  >>> from nl import Duration
  >>> d1 = Duration(start=Instant(1), end=Instant(2))

We can also instantiate ``Duration`` with just integers:

  >>> d2 = Duration(start=2, end=3)

We have to name the start and end arguments, since, as we shall see later, we can instantiate ``Duration`` as a logical variable, and not naming the arguments would confuse nl.



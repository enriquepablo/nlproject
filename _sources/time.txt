
Time expressions
================

Before exposing how we speak about time in nl, we have to say that we don't necessarily have to take time into account when using nl. If we want to talk about geometry, for example, we may ignore time completely, and build facts without the time argument, so that our discourse wil be outside time (or, what is the same, within a single and unique instant).  That said, its time for time.

There are 2 kinds of time expressions in nl: instants, and durations (intervals of time). They just basically boil down to integers and pairs of integers, except for some special treatment of the "now" concept.

Instants
--------

An instant is really simply a natural number. To create an instant, you import ``Instant`` from ``nl``, and instantiate it with an non-negative integer.

  >>> from nl import Instant
  >>> i1 = Instant(1)

As is the case with predicates, you don't ``tell`` instants (or durations); that would be rather pointless. You use them to build durations, or facts that use verbs that are ``instantaneous``.

Durations
---------

A duration is composed of 2 instants, where the first instant is a smaller integer than the second. To create a duration, you import ``Duration`` from ``nl``, and implement it with an instant named ``start`` and another instant named ``end``.

  >>> from nl import Duration
  >>> d1 = Duration(start=Instant(1), end=Instant(2))

We can also implement ``Duration`` with just integers:

  >>> d2 = Duration(start=2, end=3)

We have to name the start and end arguments, since, as we shall see later, we can instantiate ``Duration`` as a logical variable, and not naming the arguments would confuse nl.

Time and language
-----------------

Time passes. Other dimensions, such as space or mass, don't. There is a distinguished and everchanging point in time, which we call "now". Every physical dimension is "enveloped" by time, and therefore, affected by that special instant (physical objects change their "position" in those dimensions over time). However, logic is not physical, and thus not affected by time. There is a difficulty here because we, the ones that use logic, are physical. We develop (or discover) and use logic within time. Therefore, we, within time, need logic to speak about time, to envelope time. But we are getting too philosophical, and that's not practical.

The practical things regarding this issue are the following:

 #. We want to be able to say "now", at the same time as we want to be able to name any particular instant of time. Obviously, "now" has to correspond to different instants when uttered at different times.

 #. We want to be able to say various things at the same "now". Many things happen presently, but we may only say one thing at a time, so, to describe what happens now, we need a trick to cram the same "now" into any number of sentences.

 #. We need a continuous present tense. The future is mostly unknown, so, if something is happening now, we need a duration that starts in the past and ends in a moving "now".

Discrete time
-------------

As we have seen when introducing ``Instant``, in nl we take time to be discrete: any point in time is represented by an integer. An instant in nl has no duration. At instant 1 we have a state of affairs, at instant 2 we have another, and in between, we have no state of affairs. This is not a philosophical or scientific stance; it is merely practical. We are free to interpret how long does it take to go from one instant to the next in any particular discourse developed with nl: it may be a milisecond, a second, or a year.

1. Saying "now"
---------------

So, we want to be able to say "now". That is easy; with python, we would just say ``import time; time.time()``. With nl, we instantiate ``Instant`` with, instead of an integer, the string ``"now"``: ``Instant("now")``. In general (though not always, as we shall see below), nl will instantiate that instant with whatever integer it thinks that corresponds to the present time. "It thinks" means that it has an internal variable where it stores an integer that is made to correspond with the present time.

2. Saying several things now
----------------------------

We want to be able to say several things at the same time. We want to be able to say "John wants to eat now. Can he (now)?", and both nows have to correspond, and cannot be different just because we take some time in saying one sentence after the other. In nl, we solve that by passing time explicitly, by calling a function ``now()``. The internal variable mentioned above, that stores the present time, only changes when we tell it to change by calling the function ``now()``. So:

  >>> now()
  ...
  >>> i1 = Instant("now")
  >>> i2 = Instant("now")
  >>> now()
  ...
  >>> i3 = Instant("now")
  >>> str(i1) == str(i2)
  True
  >>> str(i1) == str(i3)
  False

Thus, let's suppose that we say that, if at some point in time, someone has food, (s)he can eat. And then we call ``now()``. And say that John has food now. And then we ask whether John can eat now: We get a "true" answer. But if then we call again ``now()`` and ask again whether John can eat, we don't necessarily get a "true" answer. Things have, or may have, changed. Every ``Instant("now")`` between consecutive calls to ``now()`` coincide, have the same meaning, and correspond to the same internal CLIPS construct.

There are 2 ways of using ``now()``, one that establishes what we will call a "fictional time", and another that establishes "historical time".

 **Fictional time**

To establish fictional time, we call ``now()`` with an integer argument. That integer is what is stored by nl as "now" until a subsequent call to ``now()`` with another integer (not necessarily consecutive, but bigger). We are then free to give any interpretation to the difference between consecutive instants: seconds, years, whatever.

 **Historical time**

To establish historical time, we call ``now()`` with no arguments. In this case, nl makes a call to the ``time()`` function of the ``time`` module from the standard library, converts the result to an integer, and stores that as the present time. This means that, in principle, ``Instant(0)`` corresponds to the start of the year 1970, and the difference between 2 consecutive instants corresponds to a second. We may change this, however, by calling 2 functions at the beginning of the discourse: ``time_granularity(gr)`` and ``start_of_time(start)``.

``time_granularity(gr)`` is called with a ``float`` argument that multiplies the default granularity of historical time (seconds). So, to establish that the time elapsed between 2 consecutive instants is a milisecond, we call it with ``1000.0``, and to establish that it is a minute, we call it with ``1.0/60.0``.

``start_of_time(start)`` is called with an integer that is interpreted as the difference (in whatever units of time we have established with ``time_granularity(gr)``) between our intended start of time and the beginning of 1970. So, to measure time in milliseconds and establish the start of time one second after the beginning of 1970, we would do:

  >>> time_granularity(1000.0)
  >>> start_of_time(1000)

To measure time in hours and to start time at the beginning of the 31st of december of 1969, we would:

  >>> time_granularity(1.0/3600.0)
  >>> start_of_time(-24)

etc.

3. A continuous present time
----------------------------

Finally, we want a continuous present tense. We want to say "from now onwards", and have an open ended interval of time, that can be terminated according to as yet unknown conditions. To achieve that, there is a special treatment of "now" in durations. If the start of a duration is ``Instant("now")``, it will be assigned the integer that nl has internally stored as the present time. However, if the end of a duration is ``Instant("now")``, it will adopt a special value, (actually, ``-1``), corresponding to a continuous present. When comparing times, nl will interpret that this special value is bigger than whatever it has stored as the present time, but smaller than that plus 1. This way, a fact that has a duration that ends in "now" will have a continuous present tense.

There are ways, in rules, to terminate that continuous present, and so convert those sentences to a perfect tense. This is the non-monotonic trick we spoke about in the introduction to this documentation. The monotonicity of the whole system is however kept, since there is never a reduction in the number of sentences within the knowledge base. Further, this termination propagates to whatever consecuences we may have derived from our original sentence. We shall delve more on this issue after we introduce the way we use rules in nl.



.. _Python: http://www.python.org/

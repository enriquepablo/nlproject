Time and now
============

This part of nl is perhaps the most fishy, and one that is completely optional. It is perfectly possible to use nl without ever instantiating instants or durations with the string ``'now'``, and thus to totally ignore this part of the documentation.

Time and language
-----------------

Time passes. Other dimensions, such as space or mass, don't. There is a distinguished and everchanging point in time, which we call "now". Every physical dimension is "enveloped" by time, and therefore, affected by that special instant (physical objects change their "position" in those dimensions over a line of nows in time). However, logic is not physical, and thus not affected by the passage of time. There is a difficulty here because we, the ones that use logic, are physical. We develop (or discover) and use logic within time. Therefore, we, within time, need logic to speak about time, to envelope time. But we are getting too philosophical, and that's not practical.

The practical things regarding this issue are the following, and, as can be easily seen, they don't fit within classical logic:

 #. We want to be able to say "now", at the same time as we want to be able to name any particular instant of time. Obviously, "now" has to correspond to different instants when uttered at different times. And that, of course, easily leads to contradiction.

 #. We want to be able to say various things at the same "now". Many things happen presently, but we may only say one thing at a time, so, to describe what happens now, we need a trick to cram the same "now" into any number of sentences.

 #. We need a continuous present tense. The future is mostly unknown, so, if something is happening now, we need a duration that starts in the past and ends in a moving "now", and can be terminated.

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

Every ``Instant("now")`` between consecutive calls to ``now()`` coincide, have the same meaning, and correspond to the same internal CLIPS construct. This means that nl defaults gracefully if you choose to not use its "time facilities".

There are 2 ways of using ``now()``, one that establishes what we will call a "fictional time", and another that establishes "historical time".

 **Fictional time**

To establish fictional time, we call ``now(n)`` with an integer argument. That integer is what is stored by nl as "now" until a subsequent call to ``now(n)`` with another integer (not necessarily consecutive, but bigger). We are then free to give any interpretation to the difference between consecutive instants: seconds, years, whatever.

 **Historical time**

To establish historical time, we call ``now()`` with no arguments. In this case, nl makes a call to the ``time()`` function of the ``time`` module from the standard library, converts the result to an integer, and stores that as the present time. This means that, in principle, ``Instant(0)`` corresponds to the start of the year 1970, and the difference between 2 consecutive instants corresponds to a second. We may change this, however, by calling 2 functions at the beginning of the discourse: ``time_granularity(gr)`` and ``start_of_time(start)``.

``time_granularity(gr)`` is called with a ``float`` argument that divides the default granularity of historical time (seconds). So, to establish that the time elapsed between 2 consecutive instants is a milisecond, we call it with ``1000.0``, and to establish that it is a minute, we call it with ``1.0/60.0``.

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

Finalization of the continuous present tense
--------------------------------------------

There are ways, in rules, to terminate that continuous present, and so convert those sentences to a perfect tense. This is the non-monotonic trick we spoke about in the introduction to this documentation. The monotonicity of the whole system is however kept, since there is never a reduction in the number of sentences within the knowledge base. Further, this termination propagates to whatever consecuences we may have derived from our original sentence.

To do so, we can use, as a consecuence in rules, an expression built with ``Finish``, that accepts 2 arguments, a duration, and an instant within that duration, and truncates the duration so that it ends in the provided instant:

  >>> from nl import Finish

Now, suppose that we want to assert that, if someone loves someone else, and the lover dies, he stops loving her.

  >>> class Dies(Exists):
  ...     subject = HumanBeing

  >>> kb.tell(Rule([
  ...     Fact(HumanBeing('H1'), Loves(who=HumanBeing('H2')), Duration('D1')),
  ...     Fact(HumanBeing('H1'), Dies(), Instant('I1')),
  ...     During('I1', 'D1'),
  ...     ],[
  ...     Finish('D1', 'I1'),
  ... ]))

Now, if we have that John loves Yoko from 3 onwards, and John dies now, John's love for Yoko terminates now:

  >>> kb.tell(Fact(john, Loves(who=yoko), Duration(start=3, end="now")))
  >>> kb.tell(Fact(john, Dies(), Instant("now")))
  >>> kb.extend()
  1

Now, to make time advace within nl's knowledge base, we have to execute the function ``now``, and we can see that:

  >>> from nl import now
  >>> now()
  1281517957.0

  >>> kb.ask(Fact(john, Loves(who=yoko), Instant(5)))
  True

  >>> kb.ask(Fact(john, Loves(who=yoko), Instant("now")))
  False




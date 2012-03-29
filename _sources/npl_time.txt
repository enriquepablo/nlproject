Time
----

::

    fact : subject predicate time

We can specify a time as a distinguished part of a fact. This time
has the form of either an integer or a pair of
integers. An integer marks a fact whose interpretation is an
instantaneous happening, and a pair represents an interval of time,
a duration.

For examples of all this, you can look at
`this npl test <https://github.com/enriquepablo/nl/blob/master/nl/npl_tests/cms.npl>`_.

The reason we distinguish time (it would in principle suffice to represent
times as just another modifier in the predicate)
is because we want to allow for the
present continuous (this is, for facts that have a starting instant
but not an ending instant). To do this, we employ some non-monotonic
technique. Now, the logic we have drawn up to this moment is strictly
monotonic. And non-monotonicity scares the hell out of me. So, we isolate time
in a reserved place and treat it very carefully, and make it optional.

Time can thus be given as an instant or as a duration. To assert facts,
or to specify conditions in rules, we can only use the present tense.
We assume a closed world were everything is in known the instant it happens,
i.e., we know everything about the past and the present but nothing about the
future.

Instants
~~~~~~~~

::

    time : NOW

    order : NOW DOT

    NOW : "now"

The time can be specified with the term ``now``. We can say
``sue [views what doc1] now.``.

Internally, every instance of **npl** keeps a record of time.
When **npl** is started, this record is set
to the UNIX time of the moment. It is kept like that till further notice. And
further notice is given with the order ``now.``. This order causes **npl** to
update its internal record with the UNIX time of the moment.
this internal record represents the 'present' time in the system.

When we say something like fact ``sue [views what doc1] now.``, the time that
is being stored for that fact is
the content of the said 'present' record at the time of saying. So, if we say several
facts with time ``now`` without changing the internal time with ``now.``, they will
all have the same time.

The ``now`` term is optional, and we might have just said ``sue [views what doc1].``.
If we do not specify a time, it is assumed to be ``now``.


Durations
~~~~~~~~~

::

    time : ONWARDS

    ONWARDS : "onwards"

To build a duration, we can use the reserved word ``onwards`` as the time
component. This will set the starting instant of the duration to the present,
and will set a special value
as the end of the duration. This value will stand for the 'present' time of the
system, irrespectively of its changes. So, if the present time is 10, the final
instant of these durations will evaluate to 10; and if we change the present
(through ``now.``) to 12, they will evaluate to 12.

Time in conditions
~~~~~~~~~~~~~~~~~~

::

    time : VAR
         | AT VAR

    AT : "at"

In conditions in rules, we can use, either an instant variable
(like ``at I1``), or a duration variable (like ``D1``).

The ``during`` condition
~~~~~~~~~~~~~~~~~~~~~~~~

::

    condition : VAR DURING durations
    
    DURING : "during"

We can build a special condition with ``during``, where we give an instant
variable and any number of duration variables like ``I1 during D1, D2, D3``.
This condition will evaluate to true when the intant that matches ``I1``
is contained in the durations that match ``D1``, ``D2``, and ``D3``.

Time in consecuences
~~~~~~~~~~~~~~~~~~~~

::

    time : SINCE instant TIL instant
         | SINCE instant UNTIL durations
         | SINCE instant ONWARDS
         | AT instant

    instant : arith
            | VAR

    durations : VAR COMMA durations
              | VAR

    arith : NUMBER

    SINCE : "since"

    TIL : "till"

    UNTIL : "until"

In consecuences in rules, we can use the same constructs as in conditions,
and we can specify the starting and ending instants in any way we want.
There is also a special construct for durations, in which we express the starting
instant with an instant and the ending instant
with the reserved word ``until`` followed by any
number of duration variables (bound in the conditions of the rule):
``since I1 until D1, D2, D3``. This will
create an ``onwards`` duration that will be bound to the durations that have
matched the duration variables specified, so that whenever any of them is
terminated, the new one will also be terminated. If two rules produce the
same consecuence, the system will do the right thing (require a condition
of each to be terminated before terminating the consecuence).

Terminating the continuous present
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

::

    consecuence : FINISH VAR

    FINISH : "finish"

There is a special type of consecuence, built with the reserved word
``finish``, that can be given as a consecuence in rules, like
``finish D1;``. This
sentence will change the special value of the final instant of ``D1``,
to replace it with the present. Terminating a duration will terminate
all durations that are derived from it through the ``until`` operator.

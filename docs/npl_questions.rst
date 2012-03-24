Questions.
==========

XXX.

Instants.
---------

    time : AT instant

    instant : NUMBER
            | VAR
            | NOW

    AT : "at"

    NUMBER : <any integer>

We can provide specific instants for facts, as integers (it is in the TODO list
to allow other time formats), prefixing them with the reserved word "at". So we can say:

300 john [views what doc1] at 30.

In rules or queries, we would use variables with the form `Instant1`.

Durations.
----------

    time : FROM instant TILL instant

    FROM : "from"

    TILL : "till"

Apart from instants, we can provide durations as time components of facts. To
do this, we use the reserved words `from` y `till`:

32) john [is_allowed to [edit what doc1]] from 10 till 20.

    time : VAR

Using variables in rules and queries, we can represent durations in 2 different
ways, with 2 instant variables (`from Instant1 till Instant2`) or with just one
duration variable (`Duration1`).

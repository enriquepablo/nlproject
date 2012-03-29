Arithmetics
===========

Here we will see the syntax for arithmetics in **npl**. An example program
that uses arithmetics can be seen
`here <https://github.com/enriquepablo/nl/blob/master/nl/npl_tests/physics22.npl#L32>`_.

Numbers
-------

::

    object : arith-obj

We can use numbers as modifiers in predicates. To do so, we must define the
modifiers of verbs with the term ``number``. At the moment, numbers are simply
floats, but this may change.

``a thing can has_position x a number, y a number.``

With this, we can say something like ``thing1 [has_position x 1, y 2].``.


Arithmetic operations
---------------------

::

    arith-obj : NUMBER
              | LCURL arith-operation RCURL

    arith-operation : arith-operand arith-operator arith-operation
                    | arith-operand arith-operator arith-operand

    arith-operand : NUMBER
                  | VAR
                  | LPAREN arith-operation RPAREN

    arith-operator : PLUS
                   | MINUS
                   | MULTIPLICATION
                   | DIVISION

    instant : arith-obj

We can use arithmetic operations in the consecuences of rules, both in
place of number modifiers and in place of instants. We enclose the
operations in outermost curly brackets, and any internal grouping id done with
parentheses. The available operators are sum ``+``, subtraction ``-``,
multiplication ``*`` and division ``/``. For example, ``{4 + (N1 - N2)}``.

Arithmetic conditions
---------------------

::

    condition : arith-condition

    arith-condition : LCURL arith-predication RCURL

    arith-predication : arith-operand arith-predicate arith-operand

    arith-predicate : LT
                    | GT
                    | EQ
                    | NEQ

We can specify arithmetic conditions. We do so enclosing the condition in curly
brackets. The available predicates are less than ``<``, greater than ``>``,
equals ``=``, and not equals ``<>``. For example ``{ I1 < 33 };``. We can use
instant variables in the condition, as well as number variables. We can also use
arithmetic operations within arithmetic conditions.


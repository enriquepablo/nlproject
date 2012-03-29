Questions
=========

::

    sentence : question

    question : fact QMARK
             | definition QMARK

You query **npl** with sentences and definitions ended with a question mark.
You can use variables in questions.

In **npl**'s `tests <https://github.com/enriquepablo/nl/blob/master/nl/npl_tests/>`_,
you must follow every question with a (python) regular expression that matches
the expected answer.


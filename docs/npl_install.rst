Installation and execution.
===========================

You can use **npl** in several different ways. You can start an ircbot and
talk to
it in an irchat, in a REPL manner. Or you can start a daemon and talk to it
over HTTP. Or you can use it from python (for this, refer to the
`nl documentation <nl_intro>`_).

Install
-------

to install::

  $ git clone git://github.com/enriquepablo/nlp.buildouts.git
  $ cd nlp.buildouts
  $ cp buildout.cfg.in buildout.cfg
  $ virtualenv --no-site-packages --python=python2.7 .
  $ source bin/activate
  $ python bootstrap.py
  $ bin/buildout
  ...

If you obtain any errors trying this, please report at `the issue tracker <http://github.com/enriquepablo/nl/issues>`_.

Installation
============

Dependencies
------------

 * Python 2.7
 * virtualenv
 * git
 * UNIX ?

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

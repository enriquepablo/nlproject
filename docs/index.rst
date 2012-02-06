.. nlproject documentation master file, created by
   sphinx-quickstart on Thu Jul  8 18:08:46 2010.
   You can adapt this file completely to your liking, but it should at least
   contain the root `toctree` directive.

Welcome to nlproject
====================

`**Documentation for nl** <http://github.com/enriquepablo/nl_intro>`_.

Support
-------

There is a `mailing list <http://groups.google.es/group/nl-users>`_ for nlproject at google groups, and a `blog about nl <http://nl-project.blogspot.com/>`_. You can also open an issue in `the tracker <http://github.com/enriquepablo/nlproject/issues>`_.

To install the software
-----------------------

The software is hosted at `github <http://github.com/enriquepablo/nl>`_. To install it, the easiest way is clone the buildouts repository, make a python2.7 virtualenv (or use your system's python2.7), and execute the buildout::

  $ git clone git://github.com/enriquepablo/nlp.buildouts.git
  $ cd nlp.buildouts
  $ virtualenv --no-site-packages --python=python2.7 .
  $ source bin/activate
  $ python bootstrap.py
  $ bin/buildout

  $ ipython
  ...
  In [1]: import nl

  In [2]:

To run the tests, from the root of your buildout do as follows::

  $ bin/test -w src/nl/nl

If you try any of this, and get any errors, I would be grateful if you report it at `the tracker <http://github.com/enriquepablo/nlproject/issues>`_.


.. _Python: http://www.python.org/


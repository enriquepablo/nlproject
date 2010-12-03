.. nlproject documentation master file, created by
   sphinx-quickstart on Thu Jul  8 18:08:46 2010.
   You can adapt this file completely to your liking, but it should at least
   contain the root `toctree` directive.

Welcome to nlproject
====================

**These docs are work in progress**

nlproject is a set of software facilities that provide development environments for logic programming in Python_. At its core is ``nl``, a bare bones logic system, comprising the basic building blocks for sentences and rules, and an in-memory knowledge base to hold these sentences and rules. On top of it is nlserv, that provides persistence for ``nl``'s knowledge bases and an XML-RPC interface to them.

Support
-------

There is a `mailing list <http://groups.google.es/group/nl-users>`_ for nlproject at google groups, and a `blog about nl <http://nl-project.blogspot.com/>`_. You can also open an issue in `the tracker <http://github.com/enriquepablo/nlproject/issues>`_.

To install the software
-----------------------

The software is hosted at `github <http://github.com/enriquepablo/nlproject>`_. To install it, the easiest way is clone the repository, make a python2.6 virtualenv (or use your system's python2.6), and execute the buildout::

  $ git clone git@github.com:enriquepablo/nlproject.git
  $ cd nlproject
  $ virtualenv --no-site-packages --python=python2.6 .
  $ source bin/activate
  $ python bootstrap.py
  $ bin/buildout

  $ ipython
  ...
  In [1]: import nl

  In [2]:

Of course, buildout is not the best tool to distribute the software, and it would be best to have an egg in `pypi <http://pypi.python.org>`_ and allow installation through easy_install or pip. The problem with this is that `PyCLIPS <http://pyclips.sourceforge.net/>`_, a dependency of nl, doesn't install well as an egg (though there are eggs in sourceforge): I have spent a few hours trying to produce a working setup.py for nl that takes care of this dependency but have never managed to get repeatable results. However, nl is at the moment just a tool for developers, and buildout is a very good way of obtaining a development environment.

To run the tests, from the root of your buildout do as follows::

  $ cd src/nl
  $ ../../bin/test

If you try any of this, and get any errors, I would be very grateful if you report it at `the tracker <http://github.com/enriquepablo/nlproject/issues>`_.

Contents
--------

.. toctree::
   :maxdepth: 1

   nl_intro
   nlserv

.. _Python: http://www.python.org/


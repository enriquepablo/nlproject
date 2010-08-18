
A content management system
===========================

In this section we will try to develop a more complex ontology that might be used as the metadata backend for a real life CMS. Such a CMS would basically consist of the following parts:

 #. A web application: mainly an HTTP request dispatcher and a system of views that the dispatcher calls to get responses for the requests;
 #. A database where actual content (text, images, etc.) is stored;
 #. A user authentication backend;
 #. The metadata backend that the views consult to produce responses.

The metadata backend would therefore know about content types, users, permissions, workflows, etc.

So let's start by defining a "person" noun:

  >>> class Person(Thing): pass

The first verb that affects people that we are going to define is "wants": Whenever a user attempts to perform an action on the system, we will tell the metadata backend (nl) that the user wants to perform the action. A basic pattern for the metadata rules will be "if someone wants to do such thing, and this and that conditions are met, (s)he does such thing". Therefore, a basic pattern for the views will be to tell nl that a user wants to do something, and then ask whether the user does it. We might have developed an alternative ontology using "can", defining a basic rule with the form "if someone wants to do X, and can do X, then (s)he does X", and afterwards defining rules with a basic pattern "if such and such conditions are met, user X can do Y". This alternative ontology would be much less efficient, though, since we would end up with loads and loads of (possibly useless) sentences with the form "user X can do Y with content Z".

  >>> class Wants(Exists):
  ...     subject = Person
  ...     mods = {'what': Exists}

We need a "content" class of things. ``Content`` will be the noun of all content objects, and the various content types (e.g., document) will be classes derived from ``Content``.

  >>> class Content(Thing): pass
  >>> class Document(Content): pass

Now we define a fairly general verb, ``Has``, that can have anything as subject and can have to modifiers, ``what``, that can be any thing, and ``where``, that has to be a context:

  >>> class Context(Thing): pass

  >>> class Has(Exists):
  ...      subject = Thing
  ...      instantaneous = False
  ...      mods = {'what': Thing,
  ...              'where': Context}

We will use ``Has`` 


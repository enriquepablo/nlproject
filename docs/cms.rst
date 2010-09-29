
Example: A content management system
====================================

In this section we will try to develop a more complex ontology that might be used as the metadata backend for a content management system (CMS). Such a CMS would basically consist of the following parts:

 #. A web application: mainly an HTTP request dispatcher and a system of views that the dispatcher calls to get responses for the requests;
 #. A database where actual content (text, images, etc.) is stored;
 #. A user authentication backend;
 #. The metadata backend that the views consult to produce responses.

The metadata backend would therefore know about content types, users, permissions, workflows, etc., and is the only part we will develop here.

So let's start by defining a "person" noun:

  >>> class Person(Thing): pass

The first verb that affects people that we are going to define is "wants": Whenever a user attempts to perform an action on the system (calls a URL, clicks a button, etc.), we will tell the metadata backend (nl) that the user wants to perform the action. A basic pattern for the metadata rules will be "if someone wants to do such thing, and this and that conditions are met, (s)he does such thing". Therefore, a basic pattern for the views will be to tell nl that a user wants to do something, extend the knowledge base, and then ask whether the user does it. We might have developed an alternative ontology using "can", defining a basic rule with the form "if someone wants to do X, and can do X, then (s)he does X", and afterwards defining rules with a basic pattern "if such and such conditions are met, user X can do Y". This alternative ontology would be much less efficient, though, since we would end up with loads and loads of (possibly useless) sentences with the form "user X can do Y with content Z".

  >>> class Wants(Exists):
  ...     subject = Person
  ...     mods = {'what': Exists}

We need a "content" class of things. ``Content`` will be the noun of all content objects, and the various content types (e.g., document) will be classes derived from ``Content``.

  >>> class Content(Thing): pass
  >>> class Document(Content): pass

Now we define a fairly general verb, ``Has``, that can have anything as subject and can have two modifiers, ``what``, that can be any thing, and ``where``, that has to be a context:

  >>> class Context(Thing): pass

  >>> class Has(Exists):
  ...      subject = Thing
  ...      mods = {'what': Thing,
  ...              'where': Context}

We will use ``Has`` for various things. For example, to tell the system that a certain user has a certain role in some context, or that a certain role has a certain permission, or that a certain content object has some workflow state.

Next, let's define 2 nouns: role and permission. As we have hinted above, roles are related with permissions through the verb "has". And, since it will be fairly common to assert that a given role has a certain permission, we will provide a utility Python_ method to establish that relationship.

  >>> class Permission(Thing): pass
  >>> class Role(Thing): pass

  >>> def role_has_perm(role, perm):
          kb.tell( Fact(role, Has(what=perm), Duration(start='now')) )

We could now start to define a few proper names for our ontology:

  >>> member = Role('member')
  >>> editor = Role('editor')
  >>> manager = Role('manager')
  >>> kb.tell(manager, editor, member)

  >>> basic_context = Context('basic_context')
  >>> kb.tell(basic_context)

  >>> view_perm = Permission('view_perm')
  >>> edit_perm = Permission('edit_perm') 
  >>> manage_perm = Permission('manage_perm')
  >>> kb.tell(manage_perm, edit_perm, view_perm)


  >>> role_has_perm(member, view_perm)
  >>> role_has_perm(editor, view_perm)
  >>> role_has_perm(editor, edit_perm)
  >>> role_has_perm(manager, view_perm)
  >>> role_has_perm(manager, edit_perm)
  >>> role_has_perm(manager, manage_perm)

We can now assert that, for whichever context, the admin person has the manager role:

  >>> kb.tell(Rule([
  ...     Context('X1'),
  ... ],[
  ...     Fact(admin, Has(what=manager, where=Context('X1')), Duration(start='now')),
  ... ]))

We now define a verb, ``Located``, that allows us to locate content objects in contexts:

  >>> class Located(Exists):
  ...     subject = Content
  ...     mods = {'where': Context}

Next, we define a ``Status`` noun, that refers to the different workflow states that a content object can be in. As a starting point, we shall define 2 different states, public and private:

  >>> class Status(Thing): pass

  >>> public = Status('public')
  >>> private = Status('private')
  >>> kb.tell(public, private)

And now, we will define verbs that refer to the different actions that people can perform with content objects. First, we define an abstract ``Action`` verb that will be the ancestor of any other action:

  >>> class Action(Exists):
  ...     subject = Person
  ...     mods = {'what': Content}

  >>> class View(Action): pass
  >>> class Edit(Action): pass

We now define an abstract workflow action, that will be primitive to any workflow action:

  >>> class WfAction(Action): pass
  >>> class Publish(WfAction): pass
  >>> class Hide(WfAction): pass

Now we define a ``Required`` verb, that is used to state that a certain permission is required to perform a given action over any content that is in a certain workflow state. Note that in this case, we are using an actual verb, and not a predicate, as the modifier for the "required" verb: We define it with ``Verb`` in its ``mods`` dictionary. For the moment, we can not set bounds to the possible verbs that can be used as modifiers for these verbs: we use ``Verb``, that is the only class we hav:e for verbs.

  >>> class Required(Exists):
  ...     subject = Permission
  ...     mods = {'to': Verb,
  ...             'over': Status}

At this point, we can define a rule that, when someone wants to perform an action over some content, decides whether (s)he is allowed to perform it or not, according to her roles and to the workflow state of that content. We want to assert that, if someone wants to perform some action on some content, and that content has some state and is located in some context, and the person has some role in that context that has the required permission to perform that action over that workflow state, then (s)he performs it:

  >>> kb.tell(Rule([
  ...      Fact(Person('P1'), Wants(to=Verb('V1', Action)(what=Content('C1'))), Instant('I1')),
  ...      Fact(Permission('M1'), Required(to=Verb('V1', Action), over=Status('S1')), Duration('T5')),
  ...      Fact(Content('C1'), Has(what=Status('S1')), Duration('T1')),
  ...      Fact(Content('C1'), Located(where=Context('X1')), Duration('T2')),
  ...      Fact(Person('P1'), Has(what=Role('R1'), where=Context('X1')), Duration('T3')),
  ...      Fact(Role('R1'), Has(what=Permission('M1')), Duration('T4')),
  ...      During('I1', 'T1','T2','T3','T4', 'T5')
  ...  ],[
  ...      Fact(Person('P1'), Verb('V1', Action)(what=Content('C1')), Instant('I1'))]))
 
Note the use of the ``V1`` verb variable to range over actual "action" verbs.

We can now define a utility funtion to assert that a given permission is required to perform a given action over content that is on a given workflow state, and use it to protect some actions with permissions:

  >>> def r_permission(action, status, perm):
  ...     kb.tell( Fact(perm, Required(to=action, over=status), Duration(start='now', end='now')) )

  >>> r_permission(View, public, view_perm)
  >>> r_permission(Edit, public, edit_perm)
  >>> r_permission(Hide, public, manage_perm)
  >>> r_permission(View, private, manage_perm)
  >>> r_permission(Edit, private, manage_perm)
  >>> r_permission(Publish, private, manage_perm)

Next, we are going to give meaning to workflow actions. For that, we are going to define a ``Workflow`` noun, an ``Assigned`` verb that will relate workflows to content types (depending on the context the content object is in), and another verb ``HasTransition`` that relates a workflow with an initial and a final workflow state and the workflow action that performs the transition:

  >>> class Workflow(Thing): pass

  >>> class Assigned(Exists):
  >>>     subject = Workflow
  ...     mods = {'to': Noun, #Content
  ...             'where': Context}

  >>> class HasTransition(Exists):
  ...     subject = Workflow
  ...     mods = {'start': Status,
  ...             'end': Status,
  ...             'by': Verb} #WfAction

With these terms in place, we can add a rule that states that, if some person performs some workflow action on some content, and that content is in the initial state of the transition corresponding to that action, and that action embodies the transition of some workflow that is assigned to the content type of the content object in the context in which the object is located, then the object ceases to be in the initial state and starts being in the final state of the transition:

  >>> kb.tell(Rule([
  ...   Fact(Workflow('W1'), HasTransition(start=Status('S1'), end=Status('S2'), by=Verb('V1', WfAction)), Duration('T4')),
  ...   Fact(Workflow('W1'), Assigned(to=Noun('N1', Content), where=Context('X1')), Duration('T2')),
  ...   Fact(Noun('N1', Content)('C1'), Located(where=Context('X1')), Duration('T1')),
  ...   Fact(Person('P1'), Verb('V1', WfAction)(what=Noun('N1', Content)('C1')), Instant('I1')),
  ...   Fact(Noun('N1', Content)('C1'), Has(what=Status('S1')), Duration('T3')),
  ...   During('I1', 'T1','T2', 'T3', 'T4')
  ... ],[
  ...   Fact(Noun('N1')('C1'), Has(what=Status('S2')), Duration(start=Instant('I1'), end=MaxComEnd('T1', 'T2'))),
  ...   Finish('T3', 'I1')]))

So, let's provide a function to define transitions, and a workflow for ``Document`` and assign it to ``Document`` in the basic context, and a couple of transitions for that workflow:

  >>> def r_transition(action, workflow, initial, final):
  ...     kb.tell( Fact(workflow, HasTransition(start=initial, end=final, by=action), Duration(start='now', end='now')) )

  >>> doc_workflow = Workflow('doc_workflow')
  >>> kb.tell(doc_workflow)

  >>> kb.tell( Fact(doc_workflow, Assigned(to=Document, where=basic_context), Duration(start=Instant('now'))))

  >>> r_transition(Publish, doc_workflow, private, public)
  >>> r_transition(Hide, doc_workflow, public, private)

With all this, we can start adding people and content objects, and test our ontology so far.


So, let's star using this ontology. We are going to define 2 contexts, 2 documents, one located in each context, both with an initial state private, and two people, each with the manager and editor role in opposite contexts.

  >>> john = Person('john')
  >>> mary = Person('mary')
  >>> context_of_john = Context('context_of_john')
  >>> context_of_mary = Context('context_of_mary')
  >>> doc_of_john = Document('doc_of_john')
  >>> doc_of_mary = Document('doc_of_mary')
  >>> kb.tell(john, mary, context_of_john, context_of_mary, doc_of_john, doc_of_mary)

Let's start time:

  >>> now()

  >>> kb.tell(Fact(john, Has(what=manager, where=context_of_john), Duration(start='now')))
  >>> kb.tell(Fact(john, Has(what=editor, where=context_of_mary), Duration(start='now')))
  >>> kb.tell(Fact(mary, Has(what=manager, where=context_of_mary), Duration(start='now')))
  >>> kb.tell(Fact(mary, Has(what=editor, where=context_of_john), Duration(start='now')))
  >>> kb.tell(Fact(doc_of_john, Located(where=context_of_john), Duration(start='now')))
  >>> kb.tell(Fact(doc_of_john, Has(what=private), Duration(start='now')))
  >>> kb.tell(Fact(doc_of_mary, Located(where=context_of_mary), Duration(start='now')))
  >>> kb.tell(Fact(doc_of_mary, Has(what=private), Duration(start='now')))

We extend the knowledge base:

  >>> kb.extend()

And now we can see that Mary cannot view or edit John's document, but john can:

  >>> kb.tell(Fact(mary, Wants(what=View(what=doc_of_john)), Instant('now')))
  >>> kb.tell(Fact(mary, Wants(what=Edit(what=doc_of_john)), Instant('now')))
  >>> kb.tell(Fact(john, Wants(what=View(what=doc_of_john)), Instant('now')))
  >>> kb.tell(Fact(john, Wants(what=Edit(what=doc_of_john)), Instant('now')))
  >>> kb.extend()
  >>> kb.ask(Fact(mary, View(what=doc_of_john), Instant('now')))
  False
  >>> kb.ask(Fact(mary, Edit(what=doc_of_john), Instant('now')))
  False
  >>> kb.ask(Fact(john, View(what=doc_of_john), Instant('now')))
  True
  >>> kb.ask(Fact(john, Edit(what=doc_of_john), Instant('now')))
  True

Time passes:

  >>> now()

Mary cannot publish John's doc, but John can:

  >>> kb.tell(Fact(mary, Wants(what=Publish(what=doc_of_john)), Instant('now')))
  >>> kb.tell(Fact(john, Wants(what=Publish(what=doc_of_john)), Instant('now')))
  >>> kb.extend()
  >>> kb.ask(Fact(mary, Publish(what=doc_of_john), Instant('now')))
  False
  >>> kb.ask(Fact(john, Publish(what=doc_of_john), Instant('now')))
  True

And, now, john's document is in the public state, and so, Mary can view it, but Mary's is private and John cannot view it:

  >>> kb.ask(Fact(doc_of_john, Has(what=public), Instant('now')))
  True
  >>> kb.tell(Fact(mary, Wants(what=View(what=doc_of_john)), Instant('now')))
  >>> kb.tell(Fact(john, Wants(what=View(what=doc_of_mary)), Instant('now')))
  >>> kb.extend()
  >>> kb.ask(Fact(mary, View(what=doc_of_john), Instant('now')))
  True
  >>> kb.ask(Fact(john, View(what=doc_of_mary), Instant('now')))
  False

Etc. etc.


.. _Python: http://www.python.org/

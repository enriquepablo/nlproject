Final Example
=============

-------

This is now obsolete. These docs have been moved to `http://npl.readthedocs.org <http://npl.readthedocs.org>`_

-------

To round up, I will sketch a workflow machine on top of the terminology we
have developed so far.

First we will need some workflow action verbs:


**000**  ``wf_action is content_action.``

**000**  ``publish is wf_action.``

**000**  ``hide is wf_action.``

States for content:


**000**  ``status are thing.``

**000**  ``public isa status.``

**000**  ``private isa status.``

Now we want workflow objects:


**000**  ``workflow are thing.``

Workflows are assigned to content types depending on the context:


**000**  ``is_assigned is exists withsubject workflow``
                           andcanbe to a noun,
                                    in a context.

We also want transitions in those workflows:


**000**  ``transition are thing.``


**000**  ``has is exists withsubject thing andcanbe what a thing.``

Transitions relate workflow actions with starting and ending states:


**000**  ``executed is exists withsubject transition``
                        andcanbe by a wf_action,
                                 from a status,
                                 to a status.

Finally, we need permissions and roles:


**000**  ``role are thing.``

**000**  ``manager isa role.``

**000**  ``editor isa role.``

**000**  ``visitor isa role.``


**000**  ``permission are thing.``

**000**  ``basic_perm isa permission.``

**000**  ``edit_perm isa permission.``

**000**  ``manage_perm isa permission.``

We reuse the ``has`` term to say that roles have permissions, and to say that
people have permissions. We also make a verb
to protect actions with permissions for states in contexts:


**000**  ``is_protected is exists withsubject content_action``
                            andcanbe by a permission,
                                     in a context,
                                     for a status.

And then, we can make a rule that says that if someone wants to perform an
action on a content, the content is in a context, the person has a role,
the role has a permission, and that permissions protects that action in that
context, then he does it:


**000**  ``if::``
        Person1 [wants to [Content_actionVerb1 what Content1]];
        Content1 [located where Context1];
        Content1 [has what Status1];
        Person1 [has what Role1];
        Role1 [has what Permission1];
        Content_actionVerb1 [protected by Permission1, in Context1, for Status1];
    Then:
        Person1 [Content_actionVerb1 what Content1].
        
Since the only consecuence of the rule is an instantaneous fact, we do not
need to bother about times.

The next rule will use workflow actions to transition content:


**000**  ``if::
        Person1 [Wf_action1 what Content1(ContentNoun1)];
        Workflow1 [is_assigned to ContentNoun1, in Context1] D1;
        Workflow [has Transition1] D2;
        Transition1 [executed by Wf_action1, from Status1, to Status2] D3;
        Content1 [has what Status1] D4;
    then:
        finish D4;
        Content1 [has what Status2] until D1, D2, D3.``


Let's try now some atomic facts:


**000**  ``manager [has what manage_perm] onwards.``

**000**  ``manager [has what edit_perm] onwards.``

**000**  ``manager [has what basic_perm] onwards.``

**000**  ``editor [has what edit_perm] onwards.``

**000**  ``editor [has what basic_perm] onwards.``

**000**  ``visitor [has what basic_perm] onwards.``


**000**  ``publish [is_protected by manage_perm, in ctx1, for private] onwards.``

**000**  ``hide [is_protected by edit_perm, in ctx1, for public] onwards.``

**000**  ``edit [is_protected by edit_perm, in ctx1, for private] onwards.``

**000**  ``edit [is_protected by manage_perm, in ctx1, for public] onwards.``

**000**  ``view [is_protected by edit_perm, in ctx1, for private] onwards.``

**000**  ``view [is_protected by basic_perm, in ctx1, for public] onwards.``


**000**  ``john [has what manager] onwards.``

**000**  ``mary [has what editor] onwards.``

**000**  ``pete [has what visitor] onwards.``


**000**  ``doc1 [has what private] onwards.``


**000**  ``pete [wants to [publish what doc1]].``


**000**  ``pete [publish what doc1]?``
     False


**000**  ``doc1 [has what Status1]?``
     private


**000**  ``john [wants to [publish what doc1]].``


**000**  ``john [publish what doc1]?``
     True


**000**  ``doc1 [has what Status1]?``
     public

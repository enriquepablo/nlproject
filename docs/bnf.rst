npl language essentials
=======================

**npl** is a logic programming language. In the use of **npl** there
are basically 4 stages:

 * Define terms;
 * Build statements and rules with those terms;
 * Extend the set of statements and rules to all logical consequences;
 * Query that extended set.

**npl** offers a superior expressive power when compared to the rest of logic
programming languages (that I know of). For example, it is possible in **npl**
to treat classes
or relations (well, **npl**'s functional equivalent of the classes and relations
of other logic languages) as individuals.
Note that **npl** is based on a finite domain first order logical theory that is
consistent and complete.

I'm going to describe the **npl** language going through its BNF grammar, from
top down. To illustrate the different constructs of the language, I will
be sketching along a set of terms, statements and rules to hold the bussiness
logic of a content management system.

Sentences
----------

::

    sentence : statement
             | question
             | order

Sentences are the top level grammatical elements. These are the elements that
can be entered (told or asked) into npl's knowledge base (kb). There are 3
kinds of sentences, statements questions and orders. Statements
add to the information held in the kb, and questions query the kb. Orders
do different things to the kb.

Statements
----------

::

    statement : definition DOT
              | fact DOT
              | rule DOT

    DOT : "."

Statements are asserted: they have truth value.
There are 3 main forms of statements, all ended with a dot, which we will
call definitions, facts, and rules.
Definitions are used to define terms. Facts use those terms to establish
relations among them. And rules establish relations (logical relations,
basically implication) among the relations expressed in the terms.

Definition of terms
-------------------

::

    definition : noun-def
               | name-def
               | verb-def

There are 3 types of terms we can define. We will call them (proper) names,
nouns, and verbs.

Noun definitions
~~~~~~~~~~~~~~~~

::

    noun-def : TERM ARE TERM

    ARE : "are"

    TERM : <a lower case letter followed by any number of lower case letters,
              underscores, and digits. 2 underscores in a row are forbidden>

We define a new noun by relating it with another (already defined) noun through
the reserved word `are`. To get started, we use a primitive predefined noun,
`thing`.


**001**  `person are thing.`

**002**  content are thing.

**003**  document are content.

**004**  image are content.

**005**  context are thing.

Name definitions
~~~~~~~~~~~~~~~~

::

    name-def : TERM ISA TERM

    ISA : "isa"

Proper names are defined relating them with a noun through the reserved word
`isa`.


**005**  john isa person.

**006**  sue isa person.

**007**  pete isa person.

**008**  mary isa person.

**009**  doc1 isa document.

**010**  doc2 isa document.

**011**  img1 isa image.

**012**  img2 isa image.

**000**  ctx1 isa context.

**000**  ctx2 isa context.

Types of terms.
~~~~~~~~~~~~~~~

Names and nouns establish a class structure. The relation established by `are`
among 2 nouns has the same form as the subclass relation among 2 classes, and
the relation established by `isa` among a name and a noun has the same form as
the relation between an individual and a class it belongs to. So, for example,
the above definitions entail that `document are thing`, or that
`mary isa thing`. This means that if we ask the system for `thing`s, `mary`
will be retrieved, and if in a rule we require a `thing`, `mary` will match.

This class structure is explicit in the case of nouns and names, and is
(implicitly) pervasive among the rest of terms in **npl**. So, for
example, all predicates
(predicates are complex terms composed of a verb and any number of modifiers,
as we shall see below) are implicitly related by `isa` with their verbs.
In addition, all verbs are to be thought of as related through `isa` with the
predifined term `verb`, and all nouns with `noun`, all numbers with
`number`, and all times with `time`.

This allows us to talk about types of terms. A type of terms is a term, and
the terms that are of that type are the terms related with the type term
through `isa`. Therefore, we have six mayor types of term: `noun`, `verb`,
`thing`, `exists` (the primitive predefined verb), `number`, and `time`,
and any number of subtypes of those.

For example, `doc1` is a term of type `thing`, and `document` is a term of type
`noun`.

NOTE: since the definitions of verbs set bounds on the predicates and facts
where they can appear, we shall defer their introduction until we have
introduced predicates and facts.

Facts.
------

::

    fact : subject predicate

    subject : TERM

Facts are composed of a subject and a predicate. The subject is
a name, a noun or a verb.

::

    predicate : LBRACK verb modification RBRACK
              | LBRACK verb RBRACK

    verb : TERM

    LBRACK : "["
    
    RBRACK : "]"

The predicate is a complex term enclosed in square brackets, composed of a verb
and an (optional) modification.

::

    modification : modifier COMMA modification
                 | modifier' 

    COMMA : ","

A modification is one or more modifiers, separated by commas.

::

    modifier : LABEL object

    object : TERM
           | predicate

    LABEL : <any sequence of lower case letters>

A modifier is composed of a label and an object, that can be any kind of
(atomic or complex) term except a time: a noun, a verb, a name, a number, or a
predicate.

A simple example of a fact could be `john [goes to london_zoo]`, where `john`
is the subject and `[goes to london_zoo]` the predicate, where `goes` is the
verb, and `london_zoo` is a modifier with label `to`.

Definition of verbs.
--------------------

::

    verb-def : verb IS verb WITHSUBJECT TERM ANDCANBE modification-def
             | verb IS verb  ANDCANBE modification-def
             | verb IS verb WITHSUBJECT TERM
             | verb IS verb

    modification-def : mod-def COMMA modification-def
                     | mod-def

    mod-def : LABEL A TERM

    IS : "is"

    WITHSUBJECT : "withsubject"

    ANDCANBE : "andcanbe"

    A : "a"

In the definition of a verb we can specify 3 different things. First, the
(already defined) verb from which we derive the new verb; second, the type of
term that can act as subject in a fact where the new verb forms the predicate;
and third, the modifiers that the verb can take to form the predicate.

The modifiers that a verb can take are specified through mod-defs, where we
give the label that the modifier will take, connected through the reserved word
`a` with the type of terms that can be used as that modifier.

So, for
example, let's define verbs that express actions that a person can perform on
content. For this we must use the primitive predefined verb
we mentioned earlier: `exists`.


**013**  content_action is exists withsubject person andcanbe what a content.

**014**  view is content_action.

**015**  edit is content_action.

**016**  owns is content_action.

We do not need to specify the type of the subject for a verb if it coincides
with that of its parent verb, and, if a mod-def for a child verb coincides with
one of its parent, we do not need to explicitly specify it on the child.
Derived verbs inherit the subject and mod-defs that they do not override.

With this new verbs, we can state facts such as:


**013**  pete [owns what doc1].

**014**  sue [edit what img2].

Rules.
------

::

    rule : IF COLON conditions SEMICOLON THEN COLON consecuences

    conditions : conditions SEMICOLON condition
               | condition

    condition : fact
              | name-def

    consecuences : consecuences SEMICOLON consecuence
                 | consecuence

    consecuence : fact

    IF : "if"

    COLON : ":"

    SEMICOLON : ";"

    THEN : "then"

A rule consists of 2 sets of statements, the conditions and the consecuences.
Conditions and consecuences are, mainly, facts (though they can be other types
of statements, as we shall be seeing below). Atomic facts (facts that are
asserted on their own, outside of rules) can match the conditions of rules,
and, when all conditions in a rule are matched, its consecuences are
automatically (atomically) added to the kb.

An atomic fact matches a condition in a rule if (but not only if) they are
identical (ignoring the order of modifiers in the predicate). It also matches
when they are identical except that the atomic fact specifies more modifiers
than the condition.

We can use logical variables in place of terms in the conditions and
consecuences of a rule. A logical variable is a symbol that starts with a
capital letter, followed by any number of lower case letters and underscores,
and ends with any number of digits. A logical variable has a range, that is a
type of terms. The range of a variable can be obtained by lower casing its
first letter and removing its final digits. A fact will match the condition of
a rule if they are identical except that, where the condition has a variable,
the fact has a term
that is in the range of the variable. The scope of variables is the rule: if a
term matches a variable, it does so for all its occurrences within the rule.

for a first example, we need to add a couple more of BNF rules:

::

    subject : VAR

    object : VAR

    VAR : <an uppercase letter followed by any number of lower case letters,
           digits and underscores and ending in any number of digits. Double
           underscores are forbidden.>

So, if we define a verb `located`, we can build a rule such as what follows.


**019** located is exists withsubject thing andcanbe in a context.


**019**  if::
        Thing1 [located in Context1];
        Context1 [located in Context2];
    then:
        Thing1 [located in Context2].

With this, if we have that


**020**  doc1 [located where ctx1].

**021**  ctx1 [located where ctx2].

The system will conclude that `doc1 [located where ctx2]`.

Predicate variables.
--------------------

::

    predicate : LBRACK VAR RBRACK

We have seen that we can use predicates as objects in the modifiers of other
predicates. This means that, in rules, we must be able to use variables that
range over predicates. We do this by building a variable from a verb, and
enclosing it in square brackets. For example, from `locate`, we might have
`[Locate1]` (the brackets are not part of the variable, but mark it as a
predicate).

To provide a working example, we will define a couple of verbs that take a
predicate as modifier, and build a rule with it.


**000**  wants is exists withsubject person andcanbe that a person, do a content_action.

**000**  is_allowed is exists withsubject person andcanbe to a content_action.


**000**  if::
        Person1 [wants that Person1, do [Content_action1]];
        Person1 [is_allowed to [Content_action1]];
     then:
        Person1 [Content_action1].

If with all this we say that


**000**  sue [wants that sue, do [wiew what doc1]].

**000**  sue [is_allowed to [wiew what doc1]].

The system will conclude that `sue [view what doc1]`.

Verb variables.
---------------

::

    predicate : LBRACK VAR VAR RBRACK
              | LBRACK VAR modification RBRACK

Since we can have verbs as subject or object in facts, we need to be able to
use variables in rules that range over verbs. We do this by capitalizing the
name of a verb, and appending to it "Verb" and an integer. for example, a verb
variable made from `locate` would be `LocateVerb1`. To show a more complete
example of this, we define a verb `can`, that will take a verb as modifier:


**000**  can is exists withsubject person andcanbe what a verb.

A rule with this verb:


**000**  if::
        Person1 [wants that Person1, to [Content_actionVerb1 Content_action1]];
        Person1 [can what Content_actionVerb1];
     then:
        Person1 [Content_action1].

Let's take a look at the construct `[Content_actionVerb1 Content_action1]`. It
stands for a predicate, and any predicate matching it would also match
`[Content_action1]`. However, we want to specify that the matching predicate's
verb must be the one that matches the variable `Content_actionVerb1` in the
second condition. Thus the oddly redundant form.

Now we can say:


**000**  mary [wants that mary, do [wiew what doc1]].

**000**  mary [can what wiew].

The system will conclude that `mary [view what doc1]`.

We can also use a verb variable in a predicate with modifiers. Also without
modifiers, just by itself in the predicate, like `[Content_actionVerb1]`. This
stands for a predicate where the content_action verb is alone without
modifiers, as opposed to `[Content_action1]` where nothing is said of the
number of modifiers. For an example of verb variables with modifiers, we might
have defined `can` like:


**000**  can is exists withsubject person
                   andcanbe what a verb,
                            where a context.

The rule would now take the form:


**000**  if::
        Person1 [wants that Person1, to [Content_actionVerb1 what Content1]];
        Person1 [can what Content_actionVerb1, where Context1];
        Content1 [located where Context1];
     then:
        Person1 [Content_actionVerb1 what Content1].

Verb variables can appear in rules anywhere a verb can appear.

Now we might say:


**000**  mary [wants that mary, do [wiew what doc1]].

**000**  mary [can what wiew, where ctx1].

The system will conclude that `mary [view what doc1]`.

Noun variables.
---------------

::

    subject : varvar

    object : varvar

    varvar :  VAR LPAREN VAR RPAREN

    LPAREN : "("

    RPAREN : ")"

The same we have said about verb variables can be said of noun variables, if
we substitute "verb" with "noun" throughout the first paragraph of the section
XXX. The only difference is when we want a variable form in a
condition to range
over names that have a type given by another (noun) variable. In that case, we
give the name variable inmediately followed by the noun variable enclosed in
parentheses. For example, `Person1(PersonNoun1)`.

Negation.
---------

XXX

Time.
-----

::

    fact : subject predicate time

We can specify a time as a distinguished part of a fact. This time
has the form of either an integer or a pair of
integers. An integer marks a fact whose interpretation is an
instantaneous happening, and a pair represents an interval of time,
a duration.

The reason we distinguish time (it would in principle suffice to represent
times as just another modifier in the predicate)
is because we want to allow for the
present continuous (this is, for facts that have a starting instant
but not an ending instant). To do this, we employ some non-monotonic
technique. Now, the logic we have drawn up to this moment is strictly
monotonic. And non-monotonicity scares the hell out of me. So, we isolate time
in a reserved place and treat it very carefully, and make it optional.

Time can thus be given as an instant or as a duration. To assert facts,
or to specify conditions in rules, we can only use the present tense.
We assume a closed world were everything is in known the instant it happens,
i.e., we know everything about the past and the present but nothing about the
future.

Instants.
~~~~~~~~~

::

    order : NOW DOT

    time : NOW

    NOW : "now"

The time can be specified with the term `now`. We can say:


**280** sue [views what doc1] now.

Internally, every instance of **npl** keeps a record of time.
When **npl** is started, this record is set
to the UNIX time of the moment. It is kept like that till further notice. And
further notice is given with the sentence:


**290** now.

This causes **npl** to update its internal record with the UNIX time of the moment.
this internal record represents the 'present' time in the system.

When we say something like fact XXX, the time that is being stored for that fact is
the content of the said 'present' record at the time of saying. So, if we say several
facts with time "now" without changing the internal time with "now.", they will
all have the same time.

The `now` term is optional, and we might have just said, in place of XXX:


**280** sue [views what doc1].

Durations.
~~~~~~~~~~

::

    time : ONWARDS 

    ONWARDS : "onwards"

To build a duration, we can use the reserved word `onwards` as the time
component. This will set the starting instant of the duration to the present,
and will set a special value
as the end of the duration. This value will stand for the 'present' time of the
system, irrespectively of its changes. So, if the present time is 10, the final
instant of these durations will evaluate to 10; and if we change the present
(through `now.`) to 12, they will evaluate to 12.

Time in conditions.
~~~~~~~~~~~~~~~~~~~

In conditions in rules, we can use, either `now`, `onwards`, or a duration
variable, that will evaluate to `onwards` (will be matched were `onwards`
would) but can be used in consecuences: `D1`.

Time in consecuences.
~~~~~~~~~~~~~~~~~~~~~

In consecuences in rules, we can use the same constructs as in conditions,
plus a special construct with the reserved word `until` followed by any
number of duration variables (bound in the conditions of the rule):
`until D1, D2, D3`. This will
create an `onwards` duration that will be bound to the durations that have
matched the duration variables specified, so that whenever any of them is
terminated, the new one will also be terminated. If two rules produce the
same consecuence, the system will do the right thing (require a condition
of each to be terminated before terminating the consecuence).

Terminating the continuous present.
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

::

    consecuence : FINISH VAR

    FINISH : "finish"

There is a special type of consecuence, built with the reserved word
`finish`, that can be given as a consecuence in rules, like
`finish D1;`. This
sentence will change the special value of the final instant of `D1`,
to replace it with the present. Terminating a duration will terminate
all durations that are derived from it through the `until` operator.

Final Example.
--------------

To round up, I will sketch a workflow machine on top of the terminology we
have developed so far.

First we will need some workflow action verbs:


**000**  wf_action is content_action.

**000**  publish is wf_action.

**000**  hide is wf_action.

States for content:


**000**  status are thing.

**000**  public isa status.

**000**  private isa status.

Now we want workflow objects:


**000**  workflow are thing.

Workflows are assigned to content types depending on the context:


**000**  is_assigned is exists withsubject workflow
                           andcanbe to a noun,
                                    in a context.

We also want transitions in those workflows:


**000**  transition are thing.


**000**  has is exists withsubject thing andcanbe what a thing.

Transitions relate workflow actions with starting and ending states:


**000**  executed is exists withsubject transition
                        andcanbe by a wf_action,
                                 from a status,
                                 to a status.

Finally, we need permissions and roles:


**000**  role are thing.

**000**  manager isa role.

**000**  editor isa role.

**000**  visitor isa role.


**000**  permission are thing.

**000**  basic_perm isa permission.

**000**  edit_perm isa permission.

**000**  manage_perm isa permission.

We reuse the `has` term to say that roles have permissions, and to say that
people have permissions. We also make a verb
to protect actions with permissions for states in contexts:


**000**  is_protected is exists withsubject content_action
                            andcanbe by a permission,
                                     in a context,
                                     for a status.

And then, we can make a rule that says that if someone wants to perform an
action on a content, the content is in a context, the person has a role,
the role has a permission, and that permissions protects that action in that
context, then he does it:


**000**  if::
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


**000**  if::
        Person1 [Wf_action1 what Content1(ContentNoun1)];
        Workflow1 [is_assigned to ContentNoun1, in Context1] D1;
        Workflow [has Transition1] D2;
        Transition1 [executed by Wf_action1, from Status1, to Status2] D3;
        Content1 [has what Status1] D4;
    then:
        finish D4;
        Content1 [has what Status2] until D1, D2, D3.


Let's try now some atomic facts:


**000**  manager [has what manage_perm] onwards.

**000**  manager [has what edit_perm] onwards.

**000**  manager [has what basic_perm] onwards.

**000**  editor [has what edit_perm] onwards.

**000**  editor [has what basic_perm] onwards.

**000**  visitor [has what basic_perm] onwards.


**000**  publish [is_protected by manage_perm, in ctx1, for private] onwards.

**000**  hide [is_protected by edit_perm, in ctx1, for public] onwards.

**000**  edit [is_protected by edit_perm, in ctx1, for private] onwards.

**000**  edit [is_protected by manage_perm, in ctx1, for public] onwards.

**000**  view [is_protected by edit_perm, in ctx1, for private] onwards.

**000**  view [is_protected by basic_perm, in ctx1, for public] onwards.


**000**  john [has what manager] onwards.

**000**  mary [has what editor] onwards.

**000**  pete [has what visitor] onwards.


**000**  doc1 [has what private] onwards.


**000**  pete [wants to [publish what doc1]].


**000**  pete [publish what doc1]?
     False


**000**  doc1 [has what Status1]?
     private


**000**  john [wants to [publish what doc1]].


**000**  john [publish what doc1]?
     True


**000**  doc1 [has what Status1]?
     public

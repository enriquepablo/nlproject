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
the reserved word ``are``. To get started, we use a primitive predefined noun,
``thing``.


**001**  ``person are thing.``

**002**  ``content are thing.``

**003**  ``document are content.``

**004**  ``image are content.``

**005**  ``context are thing.``

Name definitions
~~~~~~~~~~~~~~~~

::

    name-def : TERM ISA TERM

    ISA : "isa"

Proper names are defined relating them with a noun through the reserved word
``isa``.


**006**  ``john isa person.``

**007**  ``sue isa person.``

**008**  ``pete isa person.``

**009**  ``mary isa person.``

**010**  ``doc1 isa document.``

**011**  ``doc2 isa document.``

**012**  ``img1 isa image.``

**013**  ``img2 isa image.``

**014**  ``ctx1 isa context.``

**015**  ``ctx2 isa context.``

Types of terms.
~~~~~~~~~~~~~~~

Names and nouns establish a class structure. The relation established by ``are``
among 2 nouns has the same form as the subclass relation among 2 classes, and
the relation established by ``isa`` among a name and a noun has the same form as
the relation between an individual and a class it belongs to. So, for example,
the above definitions entail that ``document are thing``, or that
``mary isa thing``. This means that if we ask the system for ``thing``s, ``mary``
will be retrieved, and if in a rule we require a ``thing``, ``mary`` will match.

This class structure is explicit in the case of nouns and names, and is
(implicitly) pervasive among the rest of terms in **npl**. So, for
example, all predicates
(predicates are complex terms composed of a verb and any number of modifiers,
as we shall see below) are implicitly related by ``isa`` with their verbs.
In addition, all verbs are to be thought of as related through ``isa`` with the
predifined term ``verb``, and all nouns with ``noun``, all numbers with
``number``, and all times with ``time``.

This allows us to talk about types of terms. A type of terms is a term, and
the terms that are of that type are the terms related with the type term
through ``isa``. Therefore, we have six mayor types of term: ``noun``, ``verb``,
``thing``, ``exists`` (the primitive predefined verb), ``number``, and ``time``,
and any number of subtypes of those.

For example, ``doc1`` is a term of type ``thing``, and ``document`` is a term of type
``noun``.

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

    LABEL : <same pattern as TERM>

A modifier is composed of a label and an object, that can be any kind of
(atomic or complex) term except a time: a noun, a verb, a name, a number, or a
predicate.

A simple example of a fact could be ``john [goes to london_zoo]``, where ``john``
is the subject and ``[goes to london_zoo]`` the predicate, where ``goes`` is the
verb, and ``london_zoo`` is a modifier with label ``to``.

Definition of verbs.
--------------------

::

    verb-def : TERM CAN TERM LPAREN verbs RPAREN modification-def
             | TERM CAN TERM LPAREN verbs RPAREN

    verbs : verb COMMA verbs
          | verb

    modification-def : mod-def COMMA modification-def
                     | mod-def

    mod-def : LABEL A TERM

    CAN : "can"

    A : "a"

In the definition of a verb we can specify 3 different things. First, the
type of
term that can act as subject in a fact where the new verb forms the predicate;
second, the
(already defined) verb(s) from which we derive the new verb;
and third, the modifiers that the verb can take to form the predicate.

The modifiers that a verb can take are specified through mod-defs, where we
give the label that the modifier will take, connected through the reserved word
``a`` with the type of terms that can be used as that modifier.

So, for
example, let's define verbs that express actions that a person can perform on
content. For this we must use the primitive predefined verb
we mentioned earlier: ``exists``.


**016**  ``person can content_action (exists) what a content.``

**017**  ``person can view (content_action).``

**018**  ``person can edit (content_action).``

**019**  ``person can owns (content_action).``

We do not need to specify the type of the subject for a verb if it coincides
with that of its first parent verb, and, if a mod-def for a child verb
coincides with
one of its parents, it is not necessary to explicitly specify it on the child.
Derived verbs inherit the subject and mod-defs that they do not override.

With this new verbs, we can state facts such as:


**020**  ``pete [owns what doc1].``

**021**  ``sue [edit what img2].``

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

So, if we define a verb ``located``, we can build a rule such as what follows.


**022** ``thing can located (exists) in a context.``


**023**  ``if:``

        ``Thing1 [located in Context1];``

        ``Context1 [located in Context2];``

     ``then:``

        ``Thing1 [located in Context2].``

With this, if we have that


**024**  ``doc1 [located where ctx1].``

**025**  ``ctx1 [located where ctx2].``

The system will conclude that ``doc1 [located where ctx2]``.

Predicate variables.
--------------------

::

    predicate : LBRACK VAR RBRACK

We have seen that we can use predicates as objects in the modifiers of other
predicates. This means that, in rules, we must be able to use variables that
range over predicates. We do this by building a variable from a verb, and
enclosing it in square brackets. For example, from ``locate``, we might have
``[Locate1]`` (the brackets are not part of the variable, but mark it as a
predicate).

To provide a working example, we will define a couple of verbs that take a
predicate as modifier, and build a rule with it.


**026**  ``person can wants (exists) that a person, do a content_action.``

**027**  ``person can is_allowed (exists) to a content_action.``


**028**  ``if:``

        ``Person1 [wants that Person1, do [Content_action1]];``

        ``Person1 [is_allowed to [Content_action1]];``

     ``then:``

        ``Person1 [Content_action1].``

If with all this we say that


**029**  ``sue [wants that sue, do [wiew what doc1]].``

**030**  ``sue [is_allowed to [wiew what doc1]].``

The system will conclude that ``sue [view what doc1]``.

Verb variables.
---------------

::

    predicate : LBRACK VAR VAR RBRACK
              | LBRACK VAR modification RBRACK

Since we can have verbs as subject or object in facts, we need to be able to
use variables in rules that range over verbs. We do this by capitalizing the
name of a verb, and appending to it "Verb" and an integer. for example, a verb
variable made from ``locate`` would be ``LocateVerb1``. To show a more complete
example of this, we define a verb ``can``, that will take a verb as modifier:


**031**  ``person can may (exists) what a verb.``

A rule with this verb:


**032**  ``if::``

        ``Person1 [wants that Person1, to [Content_actionVerb1 Content_action1]];``

        ``Person1 [may what Content_actionVerb1];``

     ``then:``
   
        ``Person1 [Content_action1].``

Let's take a look at the construct ``[Content_actionVerb1 Content_action1]``. It
stands for a predicate, and any predicate matching it would also match
``[Content_action1]``. However, we want to specify that the matching predicate's
verb must be the one that matches the variable ``Content_actionVerb1`` in the
second condition. Thus the oddly redundant form.

Now we can say:


**033**  ``mary [wants that mary, do [wiew what doc1]].``

**034**  ``mary [can what wiew].``

The system will conclude that ``mary [view what doc1]``.

We can also use a verb variable in a predicate with modifiers. Also without
modifiers, just by itself in the predicate, like ``[Content_actionVerb1]``. This
stands for a predicate where the content_action verb is alone without
modifiers, as opposed to ``[Content_action1]`` where nothing is said of the
number of modifiers. For an example of verb variables with modifiers, we might
have defined ``may`` like:


**031'**  ``person can may (exists)``
                   ``what a verb,``
                   ``where a context.``

The rule would now take the form:


**032'**  ``if:``

        ``Person1 [wants that Person1, to [Content_actionVerb1 what Content1]];``

        ``Person1 [can what Content_actionVerb1, where Context1];``

        ``Content1 [located where Context1];``

     ``then:``

        ``Person1 [Content_actionVerb1 what Content1].``

Verb variables can appear in rules anywhere a verb can appear.

Now we might say:


**033'**  ``mary [wants that mary, do [wiew what doc1]].``

**034'**  ``mary [can what wiew, where ctx1].``

The system will conclude that ``mary [view what doc1]``.

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
parentheses. For example, ``Person1(PersonNoun1)``.


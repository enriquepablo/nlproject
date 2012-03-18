The npl language
===============

**npl** is a logic programming language. In the use of **npl** there
are basically 4 stages:

 * Define terms;
 * Build sentences and rules with those terms;
 * Extend the set of sentences and rules to all logical consequences;
 * Query that extended set.

<inmodest hype>
**npl** offers a superior expressive power when compared to the rest of logic
programming languages (that I know of). For example, it is possible in **npl**
to treat classes
or relations (well, **npl**'s functional equivalent of the classes and relations
of other logic languages) as individuals.
Note that **npl** is based on a finite domain first order logical theory that is
consistent and complete.
</inmodest hype>

I'm going to describe the **npl** language going through its BNF grammar, from
top down. To illustrate the different constructs of the language, I will
be sketching along a set of terms, sentences and rules to hold the bussiness
logic of a content management system.

Statements
----------

    statement : sentence
              | question

Statements are the top level grammatical elements. These are the elements that
can be entered (told or asked) into npl's knowledge base (kb). There are two
kinds of statements, sentences and questions. Sentences
add to the information held in the kb, and questions query the kb.

Sentences.
----------

    sentence : definition DOT
             | fact DOT
             | rule DOT

    DOT : "."

Sentences are asserted: they have truth value.
There are 3 main forms of sentences, all ended with a dot, which we will
call definitions, facts, and rules.
Definitions are used to define terms. Facts use those terms to establish
relations among them. And rules establish relations (logical relations,
basically implication) among the relations expressed in the terms.

Definition of terms.
--------------------

    definition : noun-def
               | name-def
               | verb-def

There are 3 types of terms we can define. We will call them (proper) names,
nouns, and verbs.

Noun definitions.
~~~~~~~~~~~~~~~~~

    noun-def : TERM ARE TERM

    ARE : "are"

    TERM : <a lower case letter followed by any number of lower case letters,
              underscores, and digits. 2 underscores in a row are forbidden>

We define a new noun by relating it with another (already defined) noun through
the reserved word `are`. To get started, we use a primitive predefined noun,
`thing`.

001  person are thing.
002  content are thing.
003  document are content.
004  image are content.
005  context are thing.

Name definitions.
~~~~~~~~~~~~~~~~~

    name-def : TERM ISA TERM

    ISA : "isa"

Proper names are defined relating them with a noun through the reserved word
`isa`.

005  john isa person.
006  sue isa person.
007  pete isa person.
008  mary isa person.
009  doc1 isa document.
010  doc2 isa document.
011  img1 isa image.
012  img2 isa image.
000  ctx1 isa context.
000  ctx2 isa context.

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
through `isa`. Therefore, we have six main types of term: `noun`, `verb`,
`thing`, `exists` (the primitive predefined verb), `number`, and `time`,
and any number of subtypes of those.

For example, `doc1` is a term of type `thing`, and `document` is a term of type
`noun`.

NOTE: since the definitions of verbs set bounds on the predicates and facts
where they can appear, we shall defer their introduction until we have
introduced predicates and facts.

Facts.
------

    fact : subject predicate

    subject : TERM

Facts are composed of a subject and a predicate. The subject is
a name, a noun or a verb.

    predicate : LBRACK verb modification RBRACK
              | LBRACK verb RBRACK

    verb : TERM

    LBRACK : "["
    
    RBRACK : "]"

The predicate is a complex term enclosed in square brackets, composed of a verb
and an (optional) modification.

    modification : modifier COMMA modification
                 | modifier' 

    COMMA : ","

A modification is one or more modifiers, separated by commas.

    modifier : LABEL object

    object : TERM
           | predicate

A modifier is composed of a label and an object, that can be any kind of
(atomic or complex) term except a time: a noun, a verb, a name, a number, or a
predicate.

A simple example of a fact could be `john [goes to london_zoo]`.

Definition of verbs.
--------------------

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
`a` with the type of terms that can be used in that modifier.

So, for
example, let's define verbs that express actions that a person can perform on
content. For this we must use the primitive predefined verb
we mentioned earlier: `exists`.

013  content_action is exists withsubject person andcanbe what a content.
014  view is content_action.
015  edit is content_action.
016  owns is content_action.

We do not need to specify the type of the subject for a verb if it coincides
with that of its parent verb, and, if a mod-def for a child verb coincides with
one of its parent, we do not need to explicitly specify it on the child.
Derived verbs inherit the subject and mod-defs that they do not override.

With this new verbs, we can state facts such as:

013  pete [owns what doc1].
014 sue [edit what img2].

Rules.
------

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

A rule consists of 2 sets of sentences, the conditions and the consecuences.
Conditions and consecuences are, mainly, facts (though they can be other types
of sentences, as we shall be seeing below). Atomic facts (facts that are
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
variable matches a term, it does so for all its occurrences within the rule.

for a first example, we need to add a couple more of BNF rules:

    subject : VAR

    object : VAR

    VAR : <an uppercase letter followed by any number of lower case letters,
           digits and underscores and ending in any number of digits. Double
           underscores are forbidden.>

So, if we define a verb `located`, we can build a rule such as what follows.

019 located is exists withsubject thing andcanbe in a context.

019  if:
        Thing1 [located in Context1];
        Context1 [located in Context2];
    then:
        Thing1 [located in Context2].

With this, if we have that

020  doc1 [located where ctx1].
021  ctx1 [located where ctx2].

The system will conclude that `doc1 [located where ctx2]`.

Predicate variables.
--------------------

    predicate : LBRACK VAR RBRACK

We have seen that we can use predicates as objects in the modifiers of other
predicates. This means that, in rules, we must be able to use variables that
range over predicates. We do this by building a variable from a verb, and
enclosing it in square brackets. For example, from `locate`, we might have
`[Locate1]` (the brackets are not part of the variable, but mark it as a
predicate).

To provide a working example, we will define a coaple of verbs that take a
predicate as modifier, and build a rule with it.

000  wants is exists withsubject person andcanbe that a person, do a content_action.
000  is_allowed is exists withsubject person andcanbe to a content_action.

000  if:
        Person1 [wants that Person1, do [Content_action1]];
        Person1 [is_allowed to [Content_action1]];
     then:
        Person1 [Content_action1].

If with all this we say that

000  sue [wants that sue, do [wiew what doc1]].
000  sue [is_allowed to [wiew what doc1]].

The system will conclude that `sue [view what doc1]`.

Verb variables.
---------------

    predicate : LBRACK VAR VAR RBRACK
              | LBRACK VAR modification RBRACK

Since we can have verbs as subject or object in facts, we need to be able to
use variables in rules that range over verbs. We do this by capitalizing the
name of a verb, and appending to it "Verb" and an integer. for example, a verb
variable made from `locate` would be `LocateVerb1`. To show a more complete
example of this, we define a verb `can`, that will take a verb as modifier:

000  can is exists withsubject person andcanbe what a verb.

A rule with this verb:

000  if:
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

000  mary [wants that mary, do [wiew what doc1]].
000  mary [can what wiew].

The system will conclude that `mary [view what doc1]`.

We can also use a verb variable in a predicate with modifiers. Also without
modifiers, just by itself in the predicate, like `[Content_actionVerb1]`. This
stands for a predicate where the content_action verb is alone without
modifiers, as opposed to `[Content_action1]` where nothing is said of the
number of modifiers.

Verb variables can appear in rules anywhere a verb can appear.

Noun variables.
---------------

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

    fact : subject predicate time

We can specify a time as a distinguished part of a fact. This time would
correspond, at heart, to just another modifier in the predicate (like the
subject). It would have the form of either an integer or a pair of
integers. An integer would mark a fact whose interpretation is an
instantaneous happening, and a pair would represent an interval of time.
The reason we distinguish time is because we want to allow for the
present continuous (this is, for facts in which we know the starting instant
but not the ending instant). To do this, we employ some non-monotonic
technique. Now, the logic we have drawn up to this moment is strictly
monotonic. And non-monotonicity scares the hell out of me. So, we isolate time
in a reserved place and treat it carefully.

Time terms can take one of several forms.

Now.
~~~~

    time : NOW

    NOW : "now"

The time can be specified with the term `now`. We can say:

280 sue [views what doc1] now.

    statement : order DOT

    order : NOW

Internally, every instance of **npl** keeps a record of time.
When **npl** is started, this record is set
to the UNIX time of the moment. It is kept like that till further notice. And
further notice is given with the statement:

290 now.

This causes **npl** to update its internal record with the UNIX time of the moment.
this internal record represents the 'present' time in the system.

When we say something like fact XXX, the time that is being stored for that fact is
the content of the said 'present' record at the time of saying. So, if we say several
facts with time "now" without changing the internal time with "now.", they will
all have the same time.

Instants.
~~~~~~~~~

    time : AT instant

    instant : NUMBER
            | VAR
            | NOW

    AT : "at"

    NUMBER : <any integer>

We can provide specific instants for facts, as integers (it is in the TODO list
to allow other time formats), prefixing them with the reserved word "at". So we can say:

300 john [views what doc1] at 30.

In rules or queries, we would use variables with the form `Instant1`.

Durations.
~~~~~~~~~~

    time : FROM instant TILL instant

    FROM : "from"

    TILL : "till"

Apart from instants, we can provide durations as time components of facts. To
do this, we use the reserved words `from` y `till`:

32) john [is_allowed to [edit what doc1]] from 10 till 20.

    time : VAR

Using variables in rules and queries, we can represent durations in 2 different
ways, with 2 instant variables (`from Instant1 till Instant2`) or with just one
duration variable (`Duration1`).

Times as terms.
~~~~~~~~~~~~~~~

Instants and durations are terms, of type `instant` and `duration`. As such,
they can only be used as time components of facts; you cannot have a modifier
or a subject of type `instant`. You can, however, use their values in
arithmetic operations, as we shall see later on, and create them in the
consecuences of rules out of number variables.


Continuous present tense.
~~~~~~~~~~~~~~~~~~~~~~~~~

    time : FROM instant ONWARDS
         | ONWARDS 

    ONWARDS : "onwards"

To build a duration, we can use the reserved word `onwards` as the final
instant of a duration, like `from 20 onwards`. This will set a special value
as the end of the duration. This value will stand for the 'present' time of the
system, irrespectively of its changes. So, if the present time is 10, the final
instant of these durations will evaluate to 10; and if we change the present
(through `now.`) to 12, they will evaluate to 12. If we use `onwards` by itself
as the time component of a fact, it will stand for `from now onwards`.

Usage of the present continuous
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

To use the present continuous, there are some restrictions we must follow.
We can only assert things in the present. Either we state facts `now`,
or we state facts `onwards`. The rest of time constructs we have seen are
reserved for rules and questions.

In conditions in rules, we can use, either `now`, `onwards`, or a duration
variable, that will evaluate to `onwards` (will be matched were `onwards`
would) but can be used in consecuences: `O1`.

In consecuences in rules, we can use the same constructs as in conditions,
plus:

  * A special construct with `until` followed by any number of duration
    variables (bound in the conditions of the rule). This will create an
    `onwards` duration that will be bound to the durations that have matched
    the duration variables specified, so that whenever any of them is
    terminated, the new one will also be terminated. If two rules produce the
    same consecuence, the system will do the right thing (require a condition
    of each to be terminated before terminating the consecuence).


In questions, we can use whichever of the constructs we have seen.
Note that the evaluation of time is smart: if there is a fact with a duration
starting at **t**, and we enter a question that needs a duration starting at
(or an instant at) **t + 1**, it will be a valid anwswer.

Terminating the continuous present.
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    consecuence : FINISH VAR

    FINISH : "finish"

There is a special type of consecuence, built with the reserved word
`finish`, that can be given as a consecuence in rules, like
`finish O1`. This
statement will change the special value of the final instant of `O1`,
to replace it with the present. This will unqualify the fact to
participate in the logic of the system (since conditions can only match the
present), and will move it to the historic store.

Terminating a duration will terminate all durations that are derived from it
by the `until` operator.

 Arithmetics.
 ============



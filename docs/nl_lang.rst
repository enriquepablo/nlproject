
The nl language
################

At the oment, the only documentation about nl is a blog entry hereXXX.

Install
=======

to install::

  $ git clone git://github.com/enriquepablo/nlp.buildouts.git
  $ cd nlp.buildouts
  $ cp buildout.cfg.in buildout.cfg
  $ vim buildout.cfg  # -> uncomment profiles/ircbot.cfg
  $ virtualenv --no-site-packages --python=python2.7 .
  $ source bin/activate
  $ python bootstrap.py
  $ bin/buildout
  ...
  $ bin/ircbot some_name
  Generating LALR tables
  Signed on as some_name_bot.
  Joined #nlpbot_some_name.
.

Now you can talk to the ircbot on freenode at channel #nlpbot_some_name.
You provide definitions, facts and rules ending them with a dot, and you ask
facts ending them with a question mark. Question facts can contain variables.
Whenever you tell or ask something to the bot, you have to address it
prefixing your message with its nickname: "some_name_bot: <message>" where <message>
is any definition, fact, rule, or question.

If you obtain any errors trying this, please report at `the issue tracker <http://github.com/enriquepablo/nlproject/issues>`_.

Description of the language
===========================

nl is a general purpose logic system, simple, yet extremelly powerful. It is
distributed under an open source license.

The pattern of use of nl is to define terms, to build sets of facts and rules
with these terms, to extend these sets to their logical consecuences, and to
query those extended sets.

The Basics.
===========

There are three basic types of atomic terms in nl: proper name, noun, and
verb. I say 'basic' because there are more, such as number or time, that we
will introduce later.

Nouns and proper names.
-----------------------

There is a predefined primitive noun: "thing". New nouns are defined relating
them with "thing" (or with any noun defined over "thing") through the reserved
word "are".

01) animal are thing.
02) person are animal.

We define proper names relating them with nouns through the reserved word
"isa".

03) john isa person.
04) anne isa person.

Proper names and nouns form a class structure. This means that definitions
01, 02 and 03 imply that "john isa animal", that "john isa thing", and also
that "person are thing". So, if in the conditions of a rule, or in a query, we
require a term that "isa thing", "john" will serve.

These definitions are considered sentences, since we can attach a truth value
to them.

Facts.
------

Next, we should consider verbs; however, since the definition of verbs sets
bounds on the facts in which they can take place, we will precede verbs with
a short introduction on facts.

Facts are composed of a subject and a predicate. The subject can be a proper
name, a noun, or a verb. The predicate is composed of a verb and
any number of modifiers separated by commas, and is enclosed in square
brackets. Each modifier in the predicate is
made up of a label and a term (a proper name, a verb, a noun, or another
predicate). An example would be:

05) john [knows who anne].

Where "john" would be the subject, "knows" the verb, and "anne" would be a
modifier labelled "who". If you are trying out this example, introduce this
after sentence 06, where we define the verb "knows".

Types of terms.
---------------

A type of terms is a term "x" that determines a set of terms
"y", "z"...  such that "y isa x", "z isa x"... Then, "y", "z"... would be of
type "x". So, "john" would be a term of type "person", because
"john isa person" (and also of type "thing").

In this sense, we have to assume that any predicate is related through "isa"
with its verb, so that the type of a predicate is its verb. Also, introducing
two new predifined terms (we shall see how to use them below),
we assume that verbs are of type "verb" and nouns of
type "noun".

Thus, proper names are terms of type "thing", predicates are (compound) terms
of type "exists" (the primitive predifined verb, as we shall see in a moment),
nouns are terms of type "noun", and verbs of type "verb".

Verbs.
------ 

To define a verb, we need to specify the type of terms that can be
subject in a fact in which it takes part, and also the labels and types of the
modifiers that can help it to form a predicate.

To start with, we have a predifined verb, "exists", with no defined modifiers.
We use it to define new verbs, like this:

06) knows is exists withsubject person andcanbe who a person.

Here, "is", "withsubject", "andcanbe", and "a" are reserved words, common to
all verb definitions. Altogether, definition 06 specifies that "knows" is a
verb derived from "exists", that can take part in facts where the subject is of
type "person", and that the predicates it forms in such facts can have a
modifier named "who" of type "person".

NOTE: We can derive verbs from verbs other than "exists". In those cases, the
new verb will inherit all the possible modifiers of its ancestors. This way, a
verb derived from "knows" won't have to specify that it can take a "who"
modifier of type "person". Also, to form a predicate, a verb does not need to
take all its possible modifiers. This is the reason we have labelled (instead
of unnamed) modifiers. Had we limited predicates to have always one and only 
one modifier, we might have disposed with the label of the modifier, and facts
would be reduced to subject-verb-object triplets.

NOTE: Logically, we might consider the subject of a fact as just a
distinguished modifier, whose name is simply the position in the fact.

In all, we might say that facts express relations, whith the terms of subject
and modifiers standing as the related individuals, and that the labels of the
modifiers indicate the position of each individual in the relation. 
However, they are not
actual relations in the logical sense, (only the relations expressed by "isa"
and "are" are relations in that sense) so we will call them just facts.

Rules.
------

A rule is composed of 2 sets of facts: the conditions and the consecuences.
When we extend a set of sentences (definitions, facts and rules), what we do
(more or less) is to search the database for facts that match the conditions of
the rules, and, everytime all the conditions in a rule are covered, to add the
consecuences to the database.

We can use logical variables in the rules. These variables act as placeholders
that (in principle) any term may match. And I say 'in principle', because the
form of the variables limit their ranges. A variable starts with an uppercase
letter and ends in an integer. If we lowercase the first letter, and remove the
final digits, we obtain the type that is the range of the variable. For
example, "john" would be in the range of "Person1" (that would be "person").

So, we might define a rule such as:

07) if:
        Person1 [knows who Person2];
    then:
        Person2 [knows who Person1].

And the system would conclude that:

08) anne [knows who john].

Complex predicates.
===================

Other predicates as modifiers.
------------------------------

We have said that in a predicate, we can use other predicates as modifiers.
Let us see an example, defining 2 new verbs:

09) wants is exists withsubject person andcanbe to a exists.
10) can is exists withsubject person andcanbe what a exists.

The modifiers that "wants" and "can" can take are defined to be of type
"exists". This means that they can be other predicates. So, we can compose a
rule such as:

11) if:
        Person1 [wants to [Exists1]];
        Person1 [can what [Exists1]];
    then:
        Person1 [Exists1].

which means that if we say:

12) robert isa person.
13) john [wants to [knows who robert]].
14) john [can what [knows who robert]].

The system will conclude that:

15) john [knows who robert].

We might also have used "Knows1" instead of "Exists1".
In the same sense, in the
definitions of "wants" and "can", we might have given the type of the modifiers
with "knows" instead of with "exists". In summary, we specify that
a modifier is a predicate by giving it a verb as type, and we use predicate
variables enclosed in square brackets.

Verbs as modifiers.
-------------------

We can use verbs as modifiers in predicates. This means that we can have
variables that range over verbs. And if we have such variables, they must be
allowed to be used in the place of the verb in the predicates of conditions or
consecuences.

As an example to show all this, we will sketch the permission
subsystem for a document management system. We have "person", and now we need
content, documents to manage:

16) content are thing.
17) document are content.

We also need verbs to talk about the actions that people can perform on
content:

18) content_action is exists withsubject person andcanbe what a content.
19) edit is content_action.
20) view is content_action.
21) delete is content_action.

Now we define a verb and a rule that will allow us to know whether someone can
or cannot do something with some content:

22) is_allowed is exists withsubject person andcanbe to a verb.

As we can see, to specify that a modifier can be a verb, we use the term "verb"
as its type. Thus we can mark verbs, so that we can check that mark in rules,
like we do in the following rule, where we say that if someone wants to do
something, and is authorized to do that kind of thing, she will do it:

23) if:
        Person1 [is_allowed to Content_actionVerb1];
        Person1 [wants to [Content_actionVerb1 Content_action1]];
    then:
        Person1 [Content_actionVerb1 Content_action1].

Variables that range over verbs break the rule of naming variables that we
indicated in the section on rules. They start with the capitalized name of a
verb of which they are a sub-verb, and end with "Verb" and an integer.
If we want to range over all verbs, we would use 'Verb1'.

In rule 23, "Content_actionVerb1" is a variable that ranges over the verbs
derived from "content_action", and "Content_action1" is a variable with range
on the predicates built with "content_action" or verbs derived from it.
The expression "[ContentActionVerb1 Content_action1]" stands for a
"Content_action1" predicate that must have a verb that coincides with that of
the rest of conditions that use the "Content_actionVerb1" variable.

So we can say:

24) john [is_allowed to view].
25) doc1 isa document.
26) john [wants to [view what doc1]].

And the system will conclude that:

27) john [view what doc1].

We can also use verbs as subject of facts.
To do so, we simply use "verb" for the type of the
subject in the definition of the verb that will have it as subject. Doing this,
we might have ended saying something like "view [is_allowed for john]." and
"if: Content_actionVerb1 [is_allowed for Person1];"...

Nouns as modifiers.
-------------------

Nouns can be used the same way as verbs, but the syntax is different. To
specify that a modifier is a noun, it is given the type "noun". To name a
variable that ranges over nouns, "Noun" is added to it the same way "Verb" was
added for verbs. And to denote a variable that ranges over proper names of a
certain type (noun), we give the variable for the proper name followed by the
variable for the noun in parentheses, like: "Person1(PersonNoun1)".

In the same way as they can be modifiers, nouns can be the subject in facts.

Time.
=====

Appart from subject and predicate, facts can take a third part, the time. There
are several forms that the time can take.

Now.
----

The time can be specified with the term "now". We can say:

28) john [knows who anne] now.

Internally, nl keeps a record of time. When nl is started, this record is set
to the UNIX time of the moment. It is kept like that till further notice. And
further notice is given with the statement:

29) now.

This causes nl to update its internal record with the UNIX time of the moment.
this internal record represents the 'present' time in the system.

When we say something like fact 28, the time that is being stored for that fact is
the content of the said 'present' record at the time of saying. So, if we say several
facts with time "now" without changing the internal time with "now.", they will
all have the same time.

Instants.
---------

We can provide specific instants for facts, as integers (it is in the TODO list
to allow other time formats), prefixing them with the reserved word "at". So we can say:

30) john [knows who anne] at 30.

In rules or queries, we would use an "InstantN" variable:

31) if: john [knows who anne] at Instant1;
    then: anne [knows who john] at Instant1.

Durations.
----------

Apart from instants, we can provide durations as time components of facts. To
do this, we use the reserved words "from" y "till":

32) john [knows who anne] from 10 till 20.

Using variables in rules and queries, we can represent durations in 2 different
ways, with 2 instant variables ("from Instant1 till Instant2") or with just one
duration variable ("Duration1").

Special time conditions.
------------------------

There are 2 special contructs we can use as conditions in rules.

We can use the
reserved word "coincide" followed by any number of duration variables
separated by commas, like "coincide Duration1, Duration2, Duration3;". This
will evaluate to true when the listed durations have all some common interval.

We can also use the reserved word "during", preceded by an instant variable and
followed by any number of duration variables separated by commas. This will
evaluate to true when the given instant lies within all the given durations.

Special time operators.
-----------------------

We will show 3 operators to be used in the consecences of rules, that each take
a sequence of duration variables and produce new time terms.

The first is "intersection", that will produce a new duration that is the
maximum duration included in all given durations.

The second and third operators are "minend" and "maxstart", that both produce
a new instant from a sequence of durations. "maxstart" will produce the
earliest instant that is included in all given durations, and "minend" the
latest.

Times as terms.
---------------

Instants and durations are terms, of type "instant" and "duration". As such,
they can only be used as time components of facts; you cannot have a modifier
or a subject of type "instant". You can, however, use their values in
arithmetic operations, as we shall see later on.


Continuous present tense.
-------------------------

To build a duration, we can use the reserved word "onwards" as the final
instant of a duration, like "from 20 onwards". This will set a special value
as the end of the duration. This value will stand for the 'present' time of the
system, irrespectively of its changes. So, if the present time is 10, the final
instant of these durations will evaluate to 10; and if we change the present
(through "now.") to 12, they will evaluate to 12.

There is a special type of sentence, built with the reserved word
"endduration", that can be given as a consecuence in rules, like
"endduration Duration1 now" or "endduration Duration1 at Instant1". This
statement will change the special value of the final instant of "Duration1",
to replace it with the value of "Instant1".

Problems with the continuous present.
-------------------------------------

Though it can be useful in certain scenarios, where we want to track real time,
this special behaviour of durations presents some problems and has to be used
with care.

 1- The value of the final instant of present continuous durations cannot be
 used in arithmetic operations or conditions, only in time operations or
 conditions (the ones we have just described). This is sensible, since it is a
 changing value, and the results of operations with it would have to change 
 value with time.
 There is a (TODO) condition to check that a duration is ended; and terminating
 a duration would trigger rules using "terminated".

 2- If we state a fact in the future, we cannot expect nl to recognize it when
 nl's present overtakes the time of that fact and makes it past. Rules that
 would fire if we had waited to enter the fact until it was in the present or
 the past (because its instant would be "during" a set of durations that are
 in the continuous present), will not fire when that fact is made past by the
 passage of time. This is kind of ok, though: if we speak about time from the
 perspective of a present, we cannot state facts about the future, as David
 Hume would have told us. What we can
 do is express present intentions or probabilities for the future.

 Arithmetics.
 ============



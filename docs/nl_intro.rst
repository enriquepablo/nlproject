
Documentation for nl
====================


Introduction to nl as a logic system
------------------------------------

nl is a Python_ library for logic programming. In this short introduction, I try to characterize nl as a logic system. However, its use is fairly straightforward, and, to use it, it is not essential to understand how it fits within the world of logic systems, so you can safely skip these couple of paragraphs and start with the following one about the basic pattern of usage.

As normal with logic programming systems, it has a declarative API. It follows a productive paradigm, that stems from its internal use of the CLIPS_ expert system. Since it is a productive logic system, where we have a knowledge base that is extended to all its logical consecuences before querying it, its *ontologies* (see below) can only refer to (be interpreted in) finite universes. Therefore, for example, classical arithmetics could not be defined in nl, though we can use "computer" arithmetics as a built-in characteristc of the system.

nl is not a general first order logic system, as could be said of prolog or CLIPS_. Rather, it implements a particular first order logic, and therefore can be said to belong to the category of description logics or CLIPS_'s COOL (CLIPS_ object oriented language). This particular first order theory implemented by nl is inspired in the form of the natural languages (again, the same can be said of description logics,) and will be given as an appendix to these docs.

To finish the classification of nl within the family of logic systems, we can say that it is strictly monotonic, though some non-monotonic tricks of CLIPS_ are used in the built-in treatment of time, as will be shown in a later section. 

Basic pattern of usage of nl
----------------------------

nl's basic usage can be summarized in five steps:

 #. Define a set of terms to be used in sentences and rules;
 #. Build a basic set of sentences and rules, and input them into nl's kowledge base [#]_. The defined terms, and this basic set, will be called here an *ontology*;
 #. Build a set of sentences based on the previous ontology, that I will call a *factset*, and input them to the knowledge base;
 #. Extend the knowledge base to all its logical consecuences;
 #. Query the knowledge base.

This would be the simplest pattern of usage of nl. After extending the knowledge base and querying it, we may add more sentences to the factset, extend again the knowledge base, and query it, as many times as necessary. Also, we can build complex ontologies by importing from other more basic ontologies, through Python_'s import system.

Introduction to the sentences that we can express in nl 
-------------------------------------------------------

Sentences are the principal units in the usage of nl. They have a meaning (in whichever universe of interpretation the user may want to choose) and a truth value (tarskian, i.e., a sentence is true or false only within the setting of a given ontology and factset and its given interpretation), just as might be said of sentences in English.

With nl we can build 2 classes of sentences: copulative (copulas), and non-copulative (facts). I will introduce them in comparison to the corresponding constructs of the English language [#]_.

**Copulas** have two parts: a proper name, and a common noun. They are semantically equivalent to English copulas, such as "john is a man", or "yellow is a colour".

**Facts** (Non-copulative sentences) are made up of 4 parts: a *subject*, a *predicate*, a *time expression*, and a *negation bit*.

 - The subject would be a proper name, that has to be already declared in a copulative sentence. 
 - The predicate would be made up of a verb and a number of named modifiers. They would be equivalent to English predicates with non copulative, non conjugated verbs, of the kind that `Tarzan of the apes`__ accustomed to utter. For example, "go from London to Paris", where "go" would be the verb, "London" would be a modifier named "from", and "Paris" would be another modifier named "to".
 - Time expressions would correspond to the conjugation of verbs, and can be of two kinds: *instants*, and *durations* (composed of a pair of instants).
 - A fact can be negated, or not [#]_.
 
__ http://farm2.static.flickr.com/1287/1347675828_bd

A complete (and simple) example of a "sort of English" sentence that could be expressed as a *fact* in nl might be "Tarzan go from London to Paris between <date1> and <date2>". Another example, using an instant instead of a duration as time expression, might be "Tarzan kill what that_lion at <date1>" (here, we have to insert "what" as the name of the "that_lion" -the *proper name* of a lion- modifier for "kill": all verb modifiers need a name).

In principle, we make no distiction among verbs or their kinds of modifiers. Transitive and intransitive, direct or indirect objects, kinds of prepositions, etc., are treated all the same in the definition of the verbs, and are only differentiated through the rules in which they take part.

One decisive characteristic of nl, which is what I think that sets it appart from other logic programming systems, is that the modifiers of the verbs in predicates are not restricted to proper names: they can be common nouns, or other verbs, or, even, other predicates, nested with no limit. For example, a predicate might be "want to go to Paris", where we have a verb ("want") and a modifier named "to" that is in itself another predicate, "go to Paris", composed of a verb "go" and a modifier "Paris" named, also, "to". Nesting further, we might express a predicate in nl corresponding to "think that want to go to Paris", and so on.

Contents
--------

.. toctree::
   :maxdepth: 2

   common_nouns
   verbs
   time
   facts
   rules
   arithmetics
   special_conditions
   queries
   counting
   cms


**FOOTNOTES:**

.. [#] nl's knowledge base is actually a CLIPS_ "program", which is manipulated by nl through the PyCLIPS_ Python_ library.

.. [#]  The name nl stands for "natural logic", and the aim of nl is to be able to program in a way that is as close as possible to the way we use English (or any other natural langage) to produce science. So, in these docs, I will be continuously refering to the English language to describe the usage of nl.

.. [#]  Here we talk about classical negation, where a sentence and its negated form constitute a contradiction. However, we can also, in the conditions of rules or in the queries, use negation by failure, i.e. ask whether a given sentence is or is not known (is present in the knowledge base).

.. _CLIPS: http://clipsrules.sourceforge.net/

.. _PyCLIPS: http://pyclips.sourceforge.net/

.. _Python: http://www.python.org/

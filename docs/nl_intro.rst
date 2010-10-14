
Documentation for nl
====================

nl is, basically, a Python_ library for logic programming. What you do with nl is to enter sentences into a *knowledge base* and then query that knowledge base for those sentences or for their consecuences. So, you might enter a sentence with the form "all men are mortal", another sentence with the form "Socrate is a man", and then enter queries with the form "is Socrate mortal?", "what things are men?", "what things is Socrate?". And that is all. What you then do with the answers, how you interpret them and how you (or your application) react to them, is entirely up to you. The only responsibilities of nl are to provide the user with as much expressive power as possible to build his sentences, and to give answers in a reasonable time.

Basic pattern of usage of nl
----------------------------

nl's basic usage can be summarized in five steps:

 #. Define a set of terms to be used in sentences and rules, basing them on a basic set of terms provided by nl;
 #. Build a basic set of sentences and rules, and input them into nl's kowledge base [#]_. The defined terms, and this basic set, will be called here an *ontology*;
 #. Build a set of sentences based on the previous ontology, that I will call a *factset*, and input them to the knowledge base;
 #. Extend the knowledge base to all its logical consecuences;
 #. Query the knowledge base.

This would be the simplest pattern of usage of nl. After extending the knowledge base and querying it, we may add more sentences to the factset, extend again the knowledge base, and query it, as many times as necessary. Also, we can build complex ontologies by importing from other more basic ontologies, through Python_'s import system.

Introduction to the sentences that we can express in nl 
-------------------------------------------------------

Sentences are the principal units in the usage of nl. We can build 2 classes of sentences: copulative (copulas), and non-copulative (facts). I will introduce them in comparison to the corresponding constructs of the English language.

**Copulas** have two parts: a proper name or a common noun, and a common noun. They are morphologically equivalent to English copulas, such as "john is a man", "men are animals", or "yellow is a colour".

**Facts** (Non-copulative sentences) are made up of 4 parts: a *subject*, a *predicate*, a *time expression*, and a *negation bit*.

 - The subject would be a proper name, that has to be already declared in a copulative sentence.
 - The predicate would be made up of a verb and a number of named modifiers. They would be equivalent to English predicates with non copulative, non conjugated verbs, of the kind that `Tarzan of the apes`__ accustomed to utter. For example, "go from London to Paris", where "go" would be the verb, "London" would be a modifier named "from", and "Paris" would be another modifier named "to".
 - Time expressions would correspond to the conjugation of verbs, and can be of two kinds: *instants*, and *durations* (composed of a pair of instants).
 - A fact can be negated, or not [#]_.
 
__ http://www.coverbrowser.com/image/tarzan-of-the-apes-1972/1-1.jpg

A complete (and simple) example of a "sort of English" sentence that could be expressed as a *fact* in nl might be "Tarzan go from London to Paris between <date1> and <date2>".

In principle, we make no distiction among verbs or their kinds of modifiers. Transitive and intransitive, direct or indirect objects, kinds of prepositions, etc., are treated all the same in the definition of the verbs, and are only differentiated through the rules in which they take part.

One decisive characteristic of nl, which is what I think that sets it appart from other logic programming systems (in terms of expressive power), is that the modifiers of the verbs in predicates are not restricted to proper names: they can be common nouns, or other verbs, or, even, other predicates, nested with no limit. For example, a predicate might be "want to go to Paris", where we have a verb ("want") and a modifier named "to" that is in itself another predicate, "go to Paris", composed of a verb "go" and a modifier "Paris" named, also, "to". Nesting further, we might express a predicate in nl corresponding to "think that want to go to Paris", and so on.

Characterization of nl as a logic system
----------------------------------------

Here I try to characterize nl as a logic system. However, its use is fairly straightforward, and, to use it, it is not essential to understand how it fits within the world of logic systems, so you can safely skip these couple of paragraphs and continue reading the following page.

As normal with logic programming systems, nl has a declarative API. It follows a productive (forward-chaining) paradigm, that stems from its internal use of the CLIPS_ expert system. Since it is a productive logic system, where we have a knowledge base that is extended to all its logical consecuences before querying it, its *ontologies* (see below) can only refer to (be interpreted in) finite universes. Therefore, for example, Peano arithmetics could not be defined in nl, though we can use "computer" arithmetics as a built-in characteristc of the system.

nl is not a general first order logic system, as could be said of prolog or CLIPS_. Rather, it implements a particular first order theory (i.e., it incorporates some built-in axioms), and therefore can be said to belong in the category of description logics or of CLIPS_'s COOL (CLIPS_ object oriented language). This particular first order theory implemented by nl is inspired in the form of the natural languages (again, the same can be said of description logics,) and will be given as an appendix to these docs.

To finish the classification of nl within the family of logic systems, we can say that it is strictly monotonic, though some non-monotonic tricks of CLIPS_ are used in the built-in treatment of time, as will be shown in a later section. 


Contents
--------

.. toctree::
   :maxdepth: 2

   common_nouns
   verbs
   time
   facts
   rules
   time2
   arithmetics
   special_conditions
   queries
   counting
   cms
   NL


**FOOTNOTES:**

.. [#] nl's knowledge base is actually a CLIPS_ "program", which is manipulated by nl through the PyCLIPS_ Python_ library.

.. [#]  Here we obviously talk about classical negation, where a sentence and its negated form constitute a contradiction. However, we can also, in the conditions of rules or in the queries, use negation by failure, i.e. ask whether a given sentence is or is not known (is present in the knowledge base).

.. _CLIPS: http://clipsrules.sourceforge.net/

.. _PyCLIPS: http://pyclips.sourceforge.net/

.. _Python: http://www.python.org/

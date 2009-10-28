(defmodule master
    (export defclass ?ALL))

(defclass Name "Common name"
    (is-a USER)
    (pattern-match reactive)
    (slot sym
        (type SYMBOL)
        (default ?NONE)
        (create-accessor read-write)))

(defclass Verb "Verb"
    (is-a USER)
    (pattern-match reactive))

(defclass Time "Time adverb/verb tense"
    (is-a USER)
    (pattern-match reactive)
    (slot instant
        (type INTEGER)
        (default ?NONE)
        (create-accessor read-write)))

(defclass Fact "nl truth valued expression"
    (is-a USER)
    (pattern-match reactive)

    (slot subject
        (type INSTANCE)                 ;of type Name
        (default ?NONE)
        (create-accessor read-write))

    (slot predicate
        (type INSTANCE)                 ;of type Verb
        (default ?NONE)
        (create-accessor read-write))

    (slot instant
        (type INSTANCE)                 ;of type Time
        (default ?NONE)
        (create-accessor read-write)))

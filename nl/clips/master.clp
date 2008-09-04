(defmodule NL
    (export defclass ?ALL))

(defclass Name "Common name"
    (is-a USER)

    (slot prop
        (type LEXEME)
        (default ?NONE)))

(defclass Verb "Verb"
    (is-a USER)

    (slot prop
        (type LEXEME)
        (default ?NONE)))

(defclass Time "Time adverb/verb tense"
    (is-a USER)

    (slot prop
        (type LEXEME)
        (default ?NONE))
    (slot int
        (type NUMBER)
        (default 0)))



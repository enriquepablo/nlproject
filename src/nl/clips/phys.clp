
(defclass Body
    (is-a Name)
    (pattern-match reactive))

(defclass Moves
    (is-a Verb)
    (pattern-match reactive)
    (slot speed
          (type INTEGER)
          (default ?NONE)
          (create-accessor read-write)))

(defclass Position
    (is-a Verb)
    (pattern-match reactive)
    (slot pos
          (type INTEGER)
          (default ?NONE)
          (create-accessor read-write)))

(defrule vel
    (declare (salience 1))
    (object (is-a Fact)
        (subject ?subj&:(superclassp Body (class ?subj))|:(eq Body (class ?subj)))
        (predicate ?pred&:(superclassp Position (class ?pred))|:(eq Position (class ?pred)))
        (instant ?inst))
    (object (is-a Fact)
        (subject ?subj2&:(eq (send ?subj get-sym) (send ?subj2 get-sym)))
        (predicate ?pred2&:(superclassp Moves (class ?pred2))|:(eq Moves (class ?pred2)))
        (instant ?inst2&:(eq (send ?inst get-instant) (send ?inst2 get-instant))))
=>
    (make-instance of Fact
        (subject (duplicate-instance ?subj))
        (predicate (make-instance (gensym) of Position
            (pos (+ (send ?pred get-pos) (send ?pred2 get-speed)))))
        (instant (make-instance (gensym) of Time
            (instant (+ (send ?inst get-instant) 1))))))

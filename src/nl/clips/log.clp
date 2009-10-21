
(defclass Name (is-a USER))

(deffunction reduce-class (?instance ?class)
    (if (eq (length$
                (find-all-instances ((?a ?class))(eq (instance-name ?a) ?instance)))
             0)
    then (make-instance ?instance of ?class)
    else (return TRUE)))
(defclass Thing (is-a Name))
(defclass Verb (is-a USER))
(defclass State (is-a Verb) )
(set-sequence-operator-recognition TRUE)
(defmessage-handler State set-slots primary ($?slots)
        (while (> (length$ ?slots) 0) do
            (bind ?slot (first$ ?slots))
            (bind ?vslots (rest$ ?slots))
            (bind ?value (first$ ?vslots))
            (bind ?slots (rest$ ?vslots))
            (dynamic-put $?slot $?value))
        (return (instance-name ?self)))


(deffunction add-pred (?class $?slots)
        (bind ?key (str-cat ?class $?slots))
        (bind ?pos (str-index "." ?key))
        (while ?pos do
            (bind ?key (str-cat (sub-string 1 (- ?pos 1) ?key)
                                "_"
                                (sub-string (+ ?pos 1) (str-length ?key) ?key)))
            (bind ?pos (str-index "." ?key)))
        (bind ?key (sym-cat ?key))
        (if (instance-existp ?key) then
            (return (instance-name ?key))
         else
            (make-instance ?key of ?class)
            (return (send (instance-name ?key) set-slots $?slots))))

(defclass Duration (is-a Name) (slot start (type NUMBER) (pattern-match reactive)) (slot end (type NUMBER) (pattern-match reactive)))

(deffunction extract-now ($?instants)
    (if (= (length$ ?instants) 0) then
        (return -1)
    else
        (bind ?count 0)
        (bind ?not-now (create$))
        (while (> (length$ ?instants) 0) do
            (bind ?instant (nth$ 1 ?instants))
            (bind ?instants (rest$ ?instants))
            (if (= ?instant -1)
            then (bind ?count (+ ?count 1))
            else (bind ?not-now (insert$ ?not-now 1 ?instant)))
            )
        (return (insert$ ?not-now 1 ?count))
    )
)


(deffunction maxcomend ($?durations)
    (bind ?ends (create$))
    (while (> (length$ ?durations) 0)
        (bind ?ends (insert$ ?ends 1 (send (nth$ 1 ?durations) get-end)))
        (bind ?durations (rest$ ?durations))
        )
    (bind ?ret (extract-now ?ends))
    (bind ?pasts (rest$ ?ret))
    (if (> (length$ ?pasts) 0)
    then (return (min (expand$ ?pasts)))
    else (return -1)
    )
)


(deffunction mincomstart ($?durations)
    (bind ?starts (create$))
    (while (> (length$ ?durations) 0)
        (bind ?starts (insert$ ?starts 1 (send (nth$ 1 ?durations) get-start)))
        (bind ?durations (rest$ ?durations))
        )
    (bind ?ret (extract-now ?starts))
    (bind ?nows (nth$ 1 ?ret))
    (if (> ?nows 0)
    then (return -1)
    else (bind ?ret (rest$ ?ret))
         (return (max (expand$ ?ret)))
    )
)

(defclass Proposition (is-a Name) (slot truth (type INTEGER) (default 1) (pattern-match reactive)) (slot subject (type INSTANCE) (pattern-match reactive)) (slot predicate (type INSTANCE) (pattern-match reactive)) (slot time (type ?VARIABLE) (pattern-match reactive)))

(deffunction add-prop (?s ?p ?t ?r)
       (bind ?count 0)
       (do-for-all-instances ((?prop Proposition))
                          (and (eq ?prop:subject ?s)
                               (eq ?prop:predicate ?p)
                               (or (and (eq (class ?t) Duration)
                                        (= (send (send ?prop get-time) get-start) (send ?t get-start))
                                        (= (send (send ?prop get-time) get-end) (send ?t get-end)))
                                   (= ?prop:time ?t))
                               (= ?prop:truth ?r))
               (bind ?count (+ ?count 1)))
        (if (= ?count 0)
        then (make-instance of Proposition (subject ?s)
                                           (predicate ?p)
                                           (time ?t)
                                           (truth ?r))
        else (return TRUE)))
(defclass Person (is-a Thing))
(defclass Can (is-a State) (slot what (type INSTANCE) (visibility public) (pattern-match reactive)))
(defclass Wants (is-a State) (slot to (type INSTANCE) (visibility public) (pattern-match reactive)))
(defclass Has (is-a State) (slot what (type INSTANCE) (visibility public) (pattern-match reactive)))
(defclass IsNeeded (is-a State) (slot for_action (type INSTANCE) (visibility public) (pattern-match reactive)))
(defclass IsIn (is-a State) (slot what (type INSTANCE) (visibility public) (pattern-match reactive)))
(defclass Group (is-a Thing))
(defclass Permission (is-a Thing))
(defclass Role (is-a Thing))
(defclass Content (is-a Thing))
(defclass Create (is-a State) (slot what (type INSTANCE) (visibility public) (pattern-match reactive)))
(defclass IsOwner (is-a State) (slot of (type INSTANCE) (visibility public) (pattern-match reactive)))
(defclass Status (is-a Thing))
(defclass View (is-a State) (slot what (type INSTANCE) (visibility public) (pattern-match reactive)))
(defclass Publish (is-a State) (slot what (type INSTANCE) (visibility public) (pattern-match reactive)))
(defclass Hide (is-a State) (slot what (type INSTANCE) (visibility public) (pattern-match reactive)))
(reduce-class [admin] Person)
(reduce-class [member] Role)
(reduce-class [manager] Role)
(reduce-class [basic_perm] Permission)
(reduce-class [manage_perm] Permission)
(reduce-class [create_perm] Permission)
(reduce-class [public] Status)
(reduce-class [private] Status)
(add-prop [admin] (add-pred Has what [manager]) (make-instance of Duration (start 733701.0) (end -1.0)) 1)
(add-prop [member] (add-pred Has what [basic_perm]) (make-instance of Duration (start 733701.0) (end -1.0)) 1)
(defrule a4dba8fb586c4d80a466d4e79522b017 (logical (object (is-a Proposition) (subject ?X1&:(or (eq (class ?X1) Person) (subclassp (class ?X1) Person))) (predicate ?Y1&:(or (eq (class ?Y1) Wants) (subclassp (class ?Y1) Wants))&:(or (eq (class (send ?Y1 get-to)) Create) (subclassp (class (send ?Y1 get-to)) Create))&:(or (eq (class (send (send ?Y1 get-to) get-what)) Thing) (subclassp (class (send (send ?Y1 get-to) get-what)) Thing))) (time ?X2) (truth 1))) (logical (object (is-a Proposition) (subject ?X1) (predicate ?Y2&:(or (eq (class ?Y2) Has) (subclassp (class ?Y2) Has))&:(eq (send ?Y2 get-what) [create_perm])) (time ?X3&:(or (eq (class ?X3) Duration) (subclassp (class ?X3) Duration))) (truth 1))) (test (and (<= (send ?X3 get-start) ?X2) (or (= (send ?X3 get-end) -1) (>= (send ?X3 get-end) ?X2)))) => (add-prop ?X1 (add-pred Create what (send (send ?Y1 get-to) get-what)) ?X2 1))
(defrule 92195dcb3ac04b11a5fa2d0c55735747 (logical (object (is-a Proposition) (subject ?X1&:(or (eq (class ?X1) Person) (subclassp (class ?X1) Person))) (predicate ?Y4&:(or (eq (class ?Y4) Wants) (subclassp (class ?Y4) Wants))) (time ?X2) (truth 1))) (logical (object (is-a Proposition) (subject ?X1) (predicate ?Y5&:(or (eq (class ?Y5) Can) (subclassp (class ?Y5) Can))&:(eq (send ?Y4 get-to) (send ?Y5 get-what))) (time ?X3&:(or (eq (class ?X3) Duration) (subclassp (class ?X3) Duration))) (truth 1))) (test (and (<= (send ?X3 get-start) ?X2) (or (= (send ?X3 get-end) -1) (>= (send ?X3 get-end) ?X2)))) => (add-prop ?X1 (send ?Y4 get-to) ?X2 1))
(defrule 3ec8b3b2bc0e443a83ec608aca0a72e0 (logical (object (is-a Proposition) (subject ?X2&:(or (eq (class ?X2) Thing) (subclassp (class ?X2) Thing))) (predicate ?Y7&:(or (eq (class ?Y7) IsNeeded) (subclassp (class ?Y7) IsNeeded))) (time ?X3&:(or (eq (class ?X3) Duration) (subclassp (class ?X3) Duration))) (truth 1))) (logical (object (is-a Proposition) (subject ?X1&:(or (eq (class ?X1) Thing) (subclassp (class ?X1) Thing))) (predicate ?Y9&:(or (eq (class ?Y9) Has) (subclassp (class ?Y9) Has))&:(eq (send ?Y9 get-what) ?X2)) (time ?X5&:(or (eq (class ?X5) Duration) (subclassp (class ?X5) Duration))) (truth 1))) (test (or (and (>= (send ?X3 get-start) (send ?X3 get-start)) (or (<= (send ?X3 get-start) (send ?X3 get-end)) (= (send ?X3 get-end) -1))) (and (>= (send ?X3 get-start) (send ?X3 get-start)) (or (<= (send ?X3 get-start) (send ?X3 get-end)) (= (send ?X3 get-end) -1))))) => (add-prop ?X1 (add-pred Can what (send ?Y7 get-for_action)) (make-instance of Duration (start (mincomstart ?X3 ?X5)) (end (maxcomend ?X3 ?X5))) 1))
(defrule fac9f2d318cd45bb94bbb106c8a3b4fb (logical (object (is-a Proposition) (subject ?X1&:(or (eq (class ?X1) Thing) (subclassp (class ?X1) Thing))) (predicate ?Y11&:(or (eq (class ?Y11) IsIn) (subclassp (class ?Y11) IsIn))&:(or (eq (class (send ?Y11 get-what)) Thing) (subclassp (class (send ?Y11 get-what)) Thing))) (time ?X4&:(or (eq (class ?X4) Duration) (subclassp (class ?X4) Duration))) (truth 1))) (logical (object (is-a Proposition) (subject ?X2&:(eq ?X2 (send ?Y11 get-what))) (predicate ?Y13&:(or (eq (class ?Y13) IsIn) (subclassp (class ?Y13) IsIn))&:(or (eq (class (send ?Y13 get-what)) Thing) (subclassp (class (send ?Y13 get-what)) Thing))) (time ?X5&:(or (eq (class ?X5) Duration) (subclassp (class ?X5) Duration))) (truth 1))) (test (or (and (>= (send ?X4 get-start) (send ?X4 get-start)) (or (<= (send ?X4 get-start) (send ?X4 get-end)) (= (send ?X4 get-end) -1))) (and (>= (send ?X4 get-start) (send ?X4 get-start)) (or (<= (send ?X4 get-start) (send ?X4 get-end)) (= (send ?X4 get-end) -1))))) => (add-prop ?X1 (add-pred IsIn what (send ?Y13 get-what)) (make-instance of Duration (start (mincomstart ?X4 ?X5)) (end (maxcomend ?X4 ?X5))) 1))
(defrule 8f5f4d46085540338feb88180853fab9 (logical (object (is-a Proposition) (subject ?X1&:(or (eq (class ?X1) Person) (subclassp (class ?X1) Person))) (predicate ?Y15&:(or (eq (class ?Y15) IsIn) (subclassp (class ?Y15) IsIn))) (time ?X3&:(or (eq (class ?X3) Duration) (subclassp (class ?X3) Duration))) (truth 1))) (logical (object (is-a Proposition) (subject ?X2&:(or (eq (class ?X2) Group) (subclassp (class ?X2) Group))) (predicate ?Y17&:(or (eq (class ?Y17) Has) (subclassp (class ?Y17) Has))&:(or (eq (class (send ?Y17 get-what)) Permission) (subclassp (class (send ?Y17 get-what)) Permission))) (time ?X5&:(or (eq (class ?X5) Duration) (subclassp (class ?X5) Duration))) (truth 1))) (test (or (and (>= (send ?X3 get-start) (send ?X3 get-start)) (or (<= (send ?X3 get-start) (send ?X3 get-end)) (= (send ?X3 get-end) -1))) (and (>= (send ?X3 get-start) (send ?X3 get-start)) (or (<= (send ?X3 get-start) (send ?X3 get-end)) (= (send ?X3 get-end) -1))))) => (add-prop ?X1 (add-pred Has what (send ?Y17 get-what)) (make-instance of Duration (start (mincomstart ?X3 ?X5)) (end (maxcomend ?X3 ?X5))) 1))
(defrule 8d065d67239c42408950d5d42b733a25 (logical (object (is-a Proposition) (subject ?X1&:(or (eq (class ?X1) Person) (subclassp (class ?X1) Person))) (predicate ?Y19&:(or (eq (class ?Y19) Has) (subclassp (class ?Y19) Has))&:(or (eq (class (send ?Y19 get-what)) Role) (subclassp (class (send ?Y19 get-what)) Role))) (time ?X3&:(or (eq (class ?X3) Duration) (subclassp (class ?X3) Duration))) (truth 1))) (logical (object (is-a Proposition) (subject ?X2&:(eq ?X2 (send ?Y19 get-what))) (predicate ?Y21&:(or (eq (class ?Y21) Has) (subclassp (class ?Y21) Has))&:(or (eq (class (send ?Y21 get-what)) Permission) (subclassp (class (send ?Y21 get-what)) Permission))) (time ?X5&:(or (eq (class ?X5) Duration) (subclassp (class ?X5) Duration))) (truth 1))) (test (or (and (>= (send ?X3 get-start) (send ?X3 get-start)) (or (<= (send ?X3 get-start) (send ?X3 get-end)) (= (send ?X3 get-end) -1))) (and (>= (send ?X3 get-start) (send ?X3 get-start)) (or (<= (send ?X3 get-start) (send ?X3 get-end)) (= (send ?X3 get-end) -1))))) => (add-prop ?X1 (add-pred Has what (send ?Y21 get-what)) (make-instance of Duration (start (mincomstart ?X3 ?X5)) (end (maxcomend ?X3 ?X5))) 1))
(defrule d98d2547de144c4f8ccdef804feb8546 (logical (object (is-a Person) (name ?X1))) => (add-prop ?X1 (add-pred Has what [member]) (make-instance of Duration (start 733701.0) (end -1.0)) 1))
(defrule 13c06ead2677421e8cb3a94358b05b09 (logical (object (is-a Proposition) (subject ?X1&:(or (eq (class ?X1) Person) (subclassp (class ?X1) Person))) (predicate ?Y23&:(or (eq (class ?Y23) Create) (subclassp (class ?Y23) Create))&:(or (eq (class (send ?Y23 get-what)) Content) (subclassp (class (send ?Y23 get-what)) Content))) (time ?X3) (truth 1))) => (reduce-class (send ?Y23 get-what) Content) (add-prop ?X1 (add-pred IsOwner of (send ?Y23 get-what)) (make-instance of Duration (start ?X3) (end -1.0)) 1) (add-prop (send ?Y23 get-what) (add-pred Has what [private]) (make-instance of Duration (start ?X3) (end -1.0)) 1))
(defrule 51b8155551e64849b8912ff2bbb65803 (logical (object (is-a Permission) (name ?X2))) => (add-prop [manager] (add-pred Has what ?X2) (make-instance of Duration (start 733701.0) (end -1.0)) 1))
(defrule 9140f4e281c54c7ba34c681758a589a0 (logical (object (is-a Proposition) (subject ?X1&:(or (eq (class ?X1) Content) (subclassp (class ?X1) Content))) (predicate ?Y24&:(or (eq (class ?Y24) Has) (subclassp (class ?Y24) Has))&:(eq (send ?Y24 get-what) [public])) (time ?X2&:(or (eq (class ?X2) Duration) (subclassp (class ?X2) Duration))) (truth 1))) => (add-prop [basic_perm] (add-pred IsNeeded for_action (add-pred View what ?X1)) ?X2 1))
(defrule cd8c2052266042aead9464ba6b6eb5da (logical (object (is-a Proposition) (subject ?X1&:(or (eq (class ?X1) Content) (subclassp (class ?X1) Content))) (predicate ?Y26&:(or (eq (class ?Y26) Has) (subclassp (class ?Y26) Has))&:(eq (send ?Y26 get-what) [private])) (time ?X2&:(or (eq (class ?X2) Duration) (subclassp (class ?X2) Duration))) (truth 1))) => (add-prop [manage_perm] (add-pred IsNeeded for_action (add-pred View what ?X1)) ?X2 1))
(defrule 3ba3e5246d004e97a9e65d4077ecd72c (logical (object (is-a Proposition) (subject ?X1&:(or (eq (class ?X1) Content) (subclassp (class ?X1) Content))) (predicate ?Y28&:(or (eq (class ?Y28) Has) (subclassp (class ?Y28) Has))&:(eq (send ?Y28 get-what) [private])) (time ?X3&:(or (eq (class ?X3) Duration) (subclassp (class ?X3) Duration))) (truth 1))) (logical (object (is-a Proposition) (subject ?X2&:(or (eq (class ?X2) Person) (subclassp (class ?X2) Person))) (predicate ?Y30&:(or (eq (class ?Y30) IsOwner) (subclassp (class ?Y30) IsOwner))&:(eq (send ?Y30 get-of) ?X1)) (time ?X4&:(or (eq (class ?X4) Duration) (subclassp (class ?X4) Duration))) (truth 1))) (test (or (and (>= (send ?X3 get-start) (send ?X3 get-start)) (or (<= (send ?X3 get-start) (send ?X3 get-end)) (= (send ?X3 get-end) -1))) (and (>= (send ?X3 get-start) (send ?X3 get-start)) (or (<= (send ?X3 get-start) (send ?X3 get-end)) (= (send ?X3 get-end) -1))))) => (add-prop ?X2 (add-pred Can what (add-pred View what ?X1)) (make-instance of Duration (start (mincomstart ?X3 ?X4)) (end (maxcomend ?X3 ?X4))) 1))
(defrule c3763fe865de47a3ad6c137e745dce3f (logical (object (is-a Proposition) (subject ?X1&:(or (eq (class ?X1) Person) (subclassp (class ?X1) Person))) (predicate ?Y32&:(or (eq (class ?Y32) Publish) (subclassp (class ?Y32) Publish))&:(or (eq (class (send ?Y32 get-what)) Content) (subclassp (class (send ?Y32 get-what)) Content))) (time ?X3) (truth 1))) (logical (object (is-a Proposition) (subject ?X2&:(eq ?X2 (send ?Y32 get-what))) (predicate ?Y33&:(or (eq (class ?Y33) Has) (subclassp (class ?Y33) Has))&:(or (eq (class (send ?Y33 get-what)) Status) (subclassp (class (send ?Y33 get-what)) Status))) (time ?X5&:(or (eq (class ?X5) Duration) (subclassp (class ?X5) Duration))) (truth 1))) => (send ?X5 put-end 733701) (add-prop (send ?Y32 get-what) (add-pred Has what [public]) (make-instance of Duration (start ?X3) (end -1.0)) 1))
(defrule db5566597ee6451990568b8d4553861e (logical (object (is-a Content) (name ?X1))) => (add-prop [manage_perm] (add-pred IsNeeded for_action (add-pred Publish what ?X1)) (make-instance of Duration (start 733701.0) (end -1.0)) 1))
(defrule dab1bd9881fb4d42b8982861df51a21b (logical (object (is-a Proposition) (subject ?X1&:(or (eq (class ?X1) Person) (subclassp (class ?X1) Person))) (predicate ?Y35&:(or (eq (class ?Y35) Hide) (subclassp (class ?Y35) Hide))&:(or (eq (class (send ?Y35 get-what)) Content) (subclassp (class (send ?Y35 get-what)) Content))) (time ?X3) (truth 1))) (logical (object (is-a Proposition) (subject ?X2&:(eq ?X2 (send ?Y35 get-what))) (predicate ?Y36&:(or (eq (class ?Y36) Has) (subclassp (class ?Y36) Has))&:(or (eq (class (send ?Y36 get-what)) Status) (subclassp (class (send ?Y36 get-what)) Status))) (time ?X5&:(or (eq (class ?X5) Duration) (subclassp (class ?X5) Duration))) (truth 1))) => (send ?X5 put-end 733701) (add-prop (send ?Y35 get-what) (add-pred Has what [private]) (make-instance of Duration (start ?X3) (end -1.0)) 1))
(defrule a5a944836bd3400ba83babeb6e38e717 (logical (object (is-a Proposition) (subject ?X1&:(or (eq (class ?X1) Person) (subclassp (class ?X1) Person))) (predicate ?Y38&:(or (eq (class ?Y38) IsOwner) (subclassp (class ?Y38) IsOwner))&:(or (eq (class (send ?Y38 get-of)) Content) (subclassp (class (send ?Y38 get-of)) Content))) (time ?X3&:(or (eq (class ?X3) Duration) (subclassp (class ?X3) Duration))) (truth 1))) => (add-prop ?X1 (add-pred Can what (add-pred Hide what (send ?Y38 get-of))) ?X3 1))
(reduce-class [john] Person)
(reduce-class [pete] Person)
(reduce-class [jane] Person)
(reduce-class [c1] Content)
(reduce-class [c2] Content)
(add-prop [john] (add-pred Has what [manager]) (make-instance of Duration (start 733701.0) (end -1.0)) 1)
(add-prop [jane] (add-pred Has what [create_perm]) (make-instance of Duration (start 733701.0) (end -1.0)) 1)
(add-prop [jane] (add-pred Wants to (add-pred Create what [c1])) 733701.0 1)
(add-prop [pete] (add-pred Wants to (add-pred Create what [c2])) 733701.0 1)
(find-all-instances ((?prop Proposition) (?Y40 IsOwner) (?Y41 Duration)) (and (eq ?prop:subject [jane]) (eq ?Y40:of [c1]) (eq ?prop:predicate ?Y40) (= ?Y41:start 733701.0) (= ?Y41:end -1.0) (eq ?prop:truth 1)))
0


no
----------running---------------------
----------runned: 33---------------------
(find-all-instances ((?prop Proposition) (?Y42 IsOwner) (?Y43 Duration)) (and (eq ?prop:subject [jane]) (eq ?Y42:of [c1]) (eq ?prop:predicate ?Y42) (= ?Y43:start 733701.0) (= ?Y43:end -1.0) (eq ?prop:truth 1)))
35


jane isowner of c1 at from 733701.0 till -1.0
(find-all-instances ((?prop Proposition) (?Y44 Has) (?Y45 Duration)) (and (eq ?prop:subject [c1]) (eq ?Y44:what [private]) (eq ?prop:predicate ?Y44) (= ?Y45:start 733701.0) (= ?Y45:end -1.0) (eq ?prop:truth 1)))
35


c1 has what private at from 733701.0 till -1.0
(find-all-instances ((?prop Proposition) (?Y46 IsOwner) (?Y47 Duration)) (and (eq ?prop:subject [pete]) (eq ?Y46:of [c2]) (eq ?prop:predicate ?Y46) (= ?Y47:start 733701.0) (= ?Y47:end -1.0) (eq ?prop:truth 1)))
0


no
(add-prop [jane] (add-pred Wants to (add-pred Publish what [c1])) 733701.0 1)
(add-prop [pete] (add-pred Wants to (add-pred Publish what [c2])) 733701.0 1)
----------running---------------------
----------runned: 0---------------------
(find-all-instances ((?prop Proposition) (?Y48 Has) (?Y49 Duration)) (and (eq ?prop:subject [c1]) (eq ?Y48:what [public]) (eq ?prop:predicate ?Y48) (= ?Y49:start 733701.0) (= ?Y49:end -1.0) (eq ?prop:truth 1)))
0


no
(find-all-instances ((?prop Proposition) (?Y50 Has) (?Y51 Duration)) (and (eq ?prop:subject [c2]) (eq ?Y50:what [public]) (eq ?prop:predicate ?Y50) (= ?Y51:start 733701.0) (= ?Y51:end -1.0) (eq ?prop:truth 1)))
0


no
(find-all-instances ((?prop Proposition) (?Y52 Can) (?Y53 View) (?Y54 Duration)) (and (eq ?prop:subject [jane]) (eq ?Y53:what [c1]) (eq ?Y52:what ?Y53) (eq ?prop:predicate ?Y52) (= ?Y54:start 733701.0) (= ?Y54:end -1.0) (eq ?prop:truth 1)))
35


jane can what view what c1 at from 733701.0 till -1.0
(find-all-instances ((?prop Proposition) (?Y55 Can) (?Y56 View) (?Y57 Duration)) (and (eq ?prop:subject [pete]) (eq ?Y56:what [c1]) (eq ?Y55:what ?Y56) (eq ?prop:predicate ?Y55) (= ?Y57:start 733701.0) (= ?Y57:end -1.0) (eq ?prop:truth 1)))
0


no
(add-prop [john] (add-pred Wants to (add-pred Publish what [c1])) 733701.0 1)
----------running---------------------
----------runned: 10---------------------
(find-all-instances ((?prop Proposition) (?Y58 Has) (?Y59 Duration)) (and (eq ?prop:subject [c1]) (eq ?Y58:what [private]) (eq ?prop:predicate ?Y58) (= ?Y59:start 733701.0) (= ?Y59:end -1.0) (eq ?prop:truth 1)))
0


no
(find-all-instances ((?prop Proposition) (?Y60 Has) (?Y61 Duration)) (and (eq ?prop:subject [c1]) (eq ?Y60:what [public]) (eq ?prop:predicate ?Y60) (= ?Y61:start 733701.0) (= ?Y61:end -1.0) (eq ?prop:truth 1)))
41


c1 has what public at from 733701.0 till -1.0
(find-all-instances ((?prop Proposition) (?Y62 Can) (?Y63 View) (?Y64 Duration)) (and (eq ?prop:subject [pete]) (eq ?Y63:what [c1]) (eq ?Y62:what ?Y63) (eq ?prop:predicate ?Y62) (= ?Y64:start 733701.0) (= ?Y64:end -1.0) (eq ?prop:truth 1)))
41


pete can what view what c1 at from 733701.0 till -1.0
(defclass Name (is-a USER))

(deffunction reduce-class (?instance ?class)
    (if (eq (length$
                (find-all-instances ((?a ?class))(eq (instance-name ?a) ?instance)))
             0)
    then (make-instance ?instance of ?class)
    else (return TRUE)))
(defclass Thing (is-a Name))
(defclass Verb (is-a USER))
(defclass State (is-a Verb) )
(set-sequence-operator-recognition TRUE)
(defmessage-handler State set-slots primary ($?slots)
        (while (> (length$ ?slots) 0) do
            (bind ?slot (first$ ?slots))
            (bind ?vslots (rest$ ?slots))
            (bind ?value (first$ ?vslots))
            (bind ?slots (rest$ ?vslots))
            (dynamic-put $?slot $?value))
        (return (instance-name ?self)))


(deffunction add-pred (?class $?slots)
        (bind ?key (str-cat ?class $?slots))
        (bind ?pos (str-index "." ?key))
        (while ?pos do
            (bind ?key (str-cat (sub-string 1 (- ?pos 1) ?key)
                                "_"
                                (sub-string (+ ?pos 1) (str-length ?key) ?key)))
            (bind ?pos (str-index "." ?key)))
        (bind ?key (sym-cat ?key))
        (if (instance-existp ?key) then
            (return (instance-name ?key))
         else
            (make-instance ?key of ?class)
            (return (send (instance-name ?key) set-slots $?slots))))

(defclass Duration (is-a Name) (slot start (type NUMBER) (pattern-match reactive)) (slot end (type NUMBER) (pattern-match reactive)))

(deffunction extract-now ($?instants)
    (if (= (length$ ?instants) 0) then
        (return -1)
    else
        (bind ?count 0)
        (bind ?not-now (create$))
        (while (> (length$ ?instants) 0) do
            (bind ?instant (nth$ 1 ?instants))
            (bind ?instants (rest$ ?instants))
            (if (= ?instant -1)
            then (bind ?count (+ ?count 1))
            else (bind ?not-now (insert$ ?not-now 1 ?instant)))
            )
        (return (insert$ ?not-now 1 ?count))
    )
)


(deffunction maxcomend ($?durations)
    (bind ?ends (create$))
    (while (> (length$ ?durations) 0)
        (bind ?ends (insert$ ?ends 1 (send (nth$ 1 ?durations) get-end)))
        (bind ?durations (rest$ ?durations))
        )
    (bind ?ret (extract-now ?ends))
    (bind ?pasts (rest$ ?ret))
    (if (> (length$ ?pasts) 0)
    then (return (min (expand$ ?pasts)))
    else (return -1)
    )
)


(deffunction mincomstart ($?durations)
    (bind ?starts (create$))
    (while (> (length$ ?durations) 0)
        (bind ?starts (insert$ ?starts 1 (send (nth$ 1 ?durations) get-start)))
        (bind ?durations (rest$ ?durations))
        )
    (bind ?ret (extract-now ?starts))
    (bind ?nows (nth$ 1 ?ret))
    (if (> ?nows 0)
    then (return -1)
    else (bind ?ret (rest$ ?ret))
         (return (max (expand$ ?ret)))
    )
)

(defclass Proposition (is-a Name) (slot truth (type INTEGER) (default 1) (pattern-match reactive)) (slot subject (type INSTANCE) (pattern-match reactive)) (slot predicate (type INSTANCE) (pattern-match reactive)) (slot time (type ?VARIABLE) (pattern-match reactive)))

(deffunction add-prop (?s ?p ?t ?r)
       (bind ?count 0)
       (do-for-all-instances ((?prop Proposition))
                          (and (eq ?prop:subject ?s)
                               (eq ?prop:predicate ?p)
                               (or (and (eq (class ?t) Duration)
                                        (= (send (send ?prop get-time) get-start) (send ?t get-start))
                                        (= (send (send ?prop get-time) get-end) (send ?t get-end)))
                                   (= ?prop:time ?t))
                               (= ?prop:truth ?r))
               (bind ?count (+ ?count 1)))
        (if (= ?count 0)
        then (make-instance of Proposition (subject ?s)
                                           (predicate ?p)
                                           (time ?t)
                                           (truth ?r))
        else (return TRUE)))
(defclass Person (is-a Thing))
(defclass Can (is-a State) (slot what (type INSTANCE) (visibility public) (pattern-match reactive)))
(defclass Wants (is-a State) (slot to (type INSTANCE) (visibility public) (pattern-match reactive)))
(defclass Has (is-a State) (slot what (type INSTANCE) (visibility public) (pattern-match reactive)))
(defclass IsNeeded (is-a State) (slot for_action (type INSTANCE) (visibility public) (pattern-match reactive)))
(defclass IsIn (is-a State) (slot what (type INSTANCE) (visibility public) (pattern-match reactive)))
(defclass Group (is-a Thing))
(defclass Permission (is-a Thing))
(defclass Role (is-a Thing))
(defclass Content (is-a Thing))
(defclass Create (is-a State) (slot what (type INSTANCE) (visibility public) (pattern-match reactive)))
(defclass IsOwner (is-a State) (slot of (type INSTANCE) (visibility public) (pattern-match reactive)))
(defclass Status (is-a Thing))
(defclass View (is-a State) (slot what (type INSTANCE) (visibility public) (pattern-match reactive)))
(defclass Publish (is-a State) (slot what (type INSTANCE) (visibility public) (pattern-match reactive)))
(defclass Hide (is-a State) (slot what (type INSTANCE) (visibility public) (pattern-match reactive)))
(reduce-class [admin] Person)
(reduce-class [member] Role)
(reduce-class [manager] Role)
(reduce-class [basic_perm] Permission)
(reduce-class [manage_perm] Permission)
(reduce-class [create_perm] Permission)
(reduce-class [public] Status)
(reduce-class [private] Status)
(add-prop [admin] (add-pred Has what [manager]) (make-instance of Duration (start 733701.0) (end -1.0)) 1)
(add-prop [member] (add-pred Has what [basic_perm]) (make-instance of Duration (start 733701.0) (end -1.0)) 1)
(defrule 85b17ff403ef4639a1754c45c7515424 (logical (object (is-a Proposition) (subject ?X1&:(or (eq (class ?X1) Person) (subclassp (class ?X1) Person))) (predicate ?Y1&:(or (eq (class ?Y1) Wants) (subclassp (class ?Y1) Wants))&:(or (eq (class (send ?Y1 get-to)) Create) (subclassp (class (send ?Y1 get-to)) Create))&:(or (eq (class (send (send ?Y1 get-to) get-what)) Thing) (subclassp (class (send (send ?Y1 get-to) get-what)) Thing))) (time ?X2) (truth 1))) (logical (object (is-a Proposition) (subject ?X1) (predicate ?Y2&:(or (eq (class ?Y2) Has) (subclassp (class ?Y2) Has))&:(eq (send ?Y2 get-what) [create_perm])) (time ?X3&:(or (eq (class ?X3) Duration) (subclassp (class ?X3) Duration))) (truth 1))) (test (and (<= (send ?X3 get-start) ?X2) (or (= (send ?X3 get-end) -1) (>= (send ?X3 get-end) ?X2)))) => (add-prop ?X1 (add-pred Create what (send (send ?Y1 get-to) get-what)) ?X2 1))
(defrule 6258bc9059cb4cbeb34c810c5bb9ac77 (logical (object (is-a Proposition) (subject ?X1&:(or (eq (class ?X1) Person) (subclassp (class ?X1) Person))) (predicate ?Y4&:(or (eq (class ?Y4) Wants) (subclassp (class ?Y4) Wants))) (time ?X2) (truth 1))) (logical (object (is-a Proposition) (subject ?X1) (predicate ?Y5&:(or (eq (class ?Y5) Can) (subclassp (class ?Y5) Can))&:(eq (send ?Y4 get-to) (send ?Y5 get-what))) (time ?X3&:(or (eq (class ?X3) Duration) (subclassp (class ?X3) Duration))) (truth 1))) (test (and (<= (send ?X3 get-start) ?X2) (or (= (send ?X3 get-end) -1) (>= (send ?X3 get-end) ?X2)))) => (add-prop ?X1 (send ?Y4 get-to) ?X2 1))
(defrule 83836487d883451aa1e49e662e297aca (logical (object (is-a Proposition) (subject ?X2&:(or (eq (class ?X2) Thing) (subclassp (class ?X2) Thing))) (predicate ?Y7&:(or (eq (class ?Y7) IsNeeded) (subclassp (class ?Y7) IsNeeded))) (time ?X3&:(or (eq (class ?X3) Duration) (subclassp (class ?X3) Duration))) (truth 1))) (logical (object (is-a Proposition) (subject ?X1&:(or (eq (class ?X1) Thing) (subclassp (class ?X1) Thing))) (predicate ?Y9&:(or (eq (class ?Y9) Has) (subclassp (class ?Y9) Has))&:(eq (send ?Y9 get-what) ?X2)) (time ?X5&:(or (eq (class ?X5) Duration) (subclassp (class ?X5) Duration))) (truth 1))) (test (or (and (>= (send ?X3 get-start) (send ?X3 get-start)) (or (<= (send ?X3 get-start) (send ?X3 get-end)) (= (send ?X3 get-end) -1))) (and (>= (send ?X3 get-start) (send ?X3 get-start)) (or (<= (send ?X3 get-start) (send ?X3 get-end)) (= (send ?X3 get-end) -1))))) => (add-prop ?X1 (add-pred Can what (send ?Y7 get-for_action)) (make-instance of Duration (start (mincomstart ?X3 ?X5)) (end (maxcomend ?X3 ?X5))) 1))
(defrule 981b7dec35b94a05b7b7e4aeea36d5d9 (logical (object (is-a Proposition) (subject ?X1&:(or (eq (class ?X1) Thing) (subclassp (class ?X1) Thing))) (predicate ?Y11&:(or (eq (class ?Y11) IsIn) (subclassp (class ?Y11) IsIn))&:(or (eq (class (send ?Y11 get-what)) Thing) (subclassp (class (send ?Y11 get-what)) Thing))) (time ?X4&:(or (eq (class ?X4) Duration) (subclassp (class ?X4) Duration))) (truth 1))) (logical (object (is-a Proposition) (subject ?X2&:(eq ?X2 (send ?Y11 get-what))) (predicate ?Y13&:(or (eq (class ?Y13) IsIn) (subclassp (class ?Y13) IsIn))&:(or (eq (class (send ?Y13 get-what)) Thing) (subclassp (class (send ?Y13 get-what)) Thing))) (time ?X5&:(or (eq (class ?X5) Duration) (subclassp (class ?X5) Duration))) (truth 1))) (test (or (and (>= (send ?X4 get-start) (send ?X4 get-start)) (or (<= (send ?X4 get-start) (send ?X4 get-end)) (= (send ?X4 get-end) -1))) (and (>= (send ?X4 get-start) (send ?X4 get-start)) (or (<= (send ?X4 get-start) (send ?X4 get-end)) (= (send ?X4 get-end) -1))))) => (add-prop ?X1 (add-pred IsIn what (send ?Y13 get-what)) (make-instance of Duration (start (mincomstart ?X4 ?X5)) (end (maxcomend ?X4 ?X5))) 1))
(defrule abe648b639ba40d5b6a340b1d8ce4bd2 (logical (object (is-a Proposition) (subject ?X1&:(or (eq (class ?X1) Person) (subclassp (class ?X1) Person))) (predicate ?Y15&:(or (eq (class ?Y15) IsIn) (subclassp (class ?Y15) IsIn))) (time ?X3&:(or (eq (class ?X3) Duration) (subclassp (class ?X3) Duration))) (truth 1))) (logical (object (is-a Proposition) (subject ?X2&:(or (eq (class ?X2) Group) (subclassp (class ?X2) Group))) (predicate ?Y17&:(or (eq (class ?Y17) Has) (subclassp (class ?Y17) Has))&:(or (eq (class (send ?Y17 get-what)) Permission) (subclassp (class (send ?Y17 get-what)) Permission))) (time ?X5&:(or (eq (class ?X5) Duration) (subclassp (class ?X5) Duration))) (truth 1))) (test (or (and (>= (send ?X3 get-start) (send ?X3 get-start)) (or (<= (send ?X3 get-start) (send ?X3 get-end)) (= (send ?X3 get-end) -1))) (and (>= (send ?X3 get-start) (send ?X3 get-start)) (or (<= (send ?X3 get-start) (send ?X3 get-end)) (= (send ?X3 get-end) -1))))) => (add-prop ?X1 (add-pred Has what (send ?Y17 get-what)) (make-instance of Duration (start (mincomstart ?X3 ?X5)) (end (maxcomend ?X3 ?X5))) 1))
(defrule acda1c078905497e979168c183cd19fc (logical (object (is-a Proposition) (subject ?X1&:(or (eq (class ?X1) Person) (subclassp (class ?X1) Person))) (predicate ?Y19&:(or (eq (class ?Y19) Has) (subclassp (class ?Y19) Has))&:(or (eq (class (send ?Y19 get-what)) Role) (subclassp (class (send ?Y19 get-what)) Role))) (time ?X3&:(or (eq (class ?X3) Duration) (subclassp (class ?X3) Duration))) (truth 1))) (logical (object (is-a Proposition) (subject ?X2&:(eq ?X2 (send ?Y19 get-what))) (predicate ?Y21&:(or (eq (class ?Y21) Has) (subclassp (class ?Y21) Has))&:(or (eq (class (send ?Y21 get-what)) Permission) (subclassp (class (send ?Y21 get-what)) Permission))) (time ?X5&:(or (eq (class ?X5) Duration) (subclassp (class ?X5) Duration))) (truth 1))) (test (or (and (>= (send ?X3 get-start) (send ?X3 get-start)) (or (<= (send ?X3 get-start) (send ?X3 get-end)) (= (send ?X3 get-end) -1))) (and (>= (send ?X3 get-start) (send ?X3 get-start)) (or (<= (send ?X3 get-start) (send ?X3 get-end)) (= (send ?X3 get-end) -1))))) => (add-prop ?X1 (add-pred Has what (send ?Y21 get-what)) (make-instance of Duration (start (mincomstart ?X3 ?X5)) (end (maxcomend ?X3 ?X5))) 1))
(defrule aac7ca6614a748159c90a48cf927670e (logical (object (is-a Person) (name ?X1))) => (add-prop ?X1 (add-pred Has what [member]) (make-instance of Duration (start 733701.0) (end -1.0)) 1))
(defrule d5423c1691bc4b4ebaa7de598641d9cc (logical (object (is-a Proposition) (subject ?X1&:(or (eq (class ?X1) Person) (subclassp (class ?X1) Person))) (predicate ?Y23&:(or (eq (class ?Y23) Create) (subclassp (class ?Y23) Create))&:(or (eq (class (send ?Y23 get-what)) Content) (subclassp (class (send ?Y23 get-what)) Content))) (time ?X3) (truth 1))) => (reduce-class (send ?Y23 get-what) Content) (add-prop ?X1 (add-pred IsOwner of (send ?Y23 get-what)) (make-instance of Duration (start ?X3) (end -1.0)) 1) (add-prop (send ?Y23 get-what) (add-pred Has what [private]) (make-instance of Duration (start ?X3) (end -1.0)) 1))
(defrule ba865636ea194e1f99ac8ce001aaf2af (logical (object (is-a Permission) (name ?X2))) => (add-prop [manager] (add-pred Has what ?X2) (make-instance of Duration (start 733701.0) (end -1.0)) 1))
(defrule 8da6c77b59ba4198ae1769ddda6c781d (logical (object (is-a Proposition) (subject ?X1&:(or (eq (class ?X1) Content) (subclassp (class ?X1) Content))) (predicate ?Y24&:(or (eq (class ?Y24) Has) (subclassp (class ?Y24) Has))&:(eq (send ?Y24 get-what) [public])) (time ?X2&:(or (eq (class ?X2) Duration) (subclassp (class ?X2) Duration))) (truth 1))) => (add-prop [basic_perm] (add-pred IsNeeded for_action (add-pred View what ?X1)) ?X2 1))
(defrule d05f805405654fe49f36a886fff341f1 (logical (object (is-a Proposition) (subject ?X1&:(or (eq (class ?X1) Content) (subclassp (class ?X1) Content))) (predicate ?Y26&:(or (eq (class ?Y26) Has) (subclassp (class ?Y26) Has))&:(eq (send ?Y26 get-what) [private])) (time ?X2&:(or (eq (class ?X2) Duration) (subclassp (class ?X2) Duration))) (truth 1))) => (add-prop [manage_perm] (add-pred IsNeeded for_action (add-pred View what ?X1)) ?X2 1))
(defrule d53aad2f8ae74aafbb3294c83f13e6b9 (logical (object (is-a Proposition) (subject ?X1&:(or (eq (class ?X1) Content) (subclassp (class ?X1) Content))) (predicate ?Y28&:(or (eq (class ?Y28) Has) (subclassp (class ?Y28) Has))&:(eq (send ?Y28 get-what) [private])) (time ?X3&:(or (eq (class ?X3) Duration) (subclassp (class ?X3) Duration))) (truth 1))) (logical (object (is-a Proposition) (subject ?X2&:(or (eq (class ?X2) Person) (subclassp (class ?X2) Person))) (predicate ?Y30&:(or (eq (class ?Y30) IsOwner) (subclassp (class ?Y30) IsOwner))&:(eq (send ?Y30 get-of) ?X1)) (time ?X4&:(or (eq (class ?X4) Duration) (subclassp (class ?X4) Duration))) (truth 1))) (test (or (and (>= (send ?X3 get-start) (send ?X3 get-start)) (or (<= (send ?X3 get-start) (send ?X3 get-end)) (= (send ?X3 get-end) -1))) (and (>= (send ?X3 get-start) (send ?X3 get-start)) (or (<= (send ?X3 get-start) (send ?X3 get-end)) (= (send ?X3 get-end) -1))))) => (add-prop ?X2 (add-pred Can what (add-pred View what ?X1)) (make-instance of Duration (start (mincomstart ?X3 ?X4)) (end (maxcomend ?X3 ?X4))) 1))
(defrule b030c1945d0449b88a4bec8fdf6104e1 (logical (object (is-a Proposition) (subject ?X1&:(or (eq (class ?X1) Person) (subclassp (class ?X1) Person))) (predicate ?Y32&:(or (eq (class ?Y32) Publish) (subclassp (class ?Y32) Publish))&:(or (eq (class (send ?Y32 get-what)) Content) (subclassp (class (send ?Y32 get-what)) Content))) (time ?X3) (truth 1))) (logical (object (is-a Proposition) (subject ?X2&:(eq ?X2 (send ?Y32 get-what))) (predicate ?Y33&:(or (eq (class ?Y33) Has) (subclassp (class ?Y33) Has))&:(or (eq (class (send ?Y33 get-what)) Status) (subclassp (class (send ?Y33 get-what)) Status))) (time ?X5&:(or (eq (class ?X5) Duration) (subclassp (class ?X5) Duration))) (truth 1))) => (send ?X5 put-end 733701) (add-prop (send ?Y32 get-what) (add-pred Has what [public]) (make-instance of Duration (start ?X3) (end -1.0)) 1))
(defrule ac8fe18d2d8c4b658c6ddb1c91ebacac (logical (object (is-a Content) (name ?X1))) => (add-prop [manage_perm] (add-pred IsNeeded for_action (add-pred Publish what ?X1)) (make-instance of Duration (start 733701.0) (end -1.0)) 1))
(defrule 12b97fcc0920437d84e1fa85d9e3409f (logical (object (is-a Proposition) (subject ?X1&:(or (eq (class ?X1) Person) (subclassp (class ?X1) Person))) (predicate ?Y35&:(or (eq (class ?Y35) Hide) (subclassp (class ?Y35) Hide))&:(or (eq (class (send ?Y35 get-what)) Content) (subclassp (class (send ?Y35 get-what)) Content))) (time ?X3) (truth 1))) (logical (object (is-a Proposition) (subject ?X2&:(eq ?X2 (send ?Y35 get-what))) (predicate ?Y36&:(or (eq (class ?Y36) Has) (subclassp (class ?Y36) Has))&:(or (eq (class (send ?Y36 get-what)) Status) (subclassp (class (send ?Y36 get-what)) Status))) (time ?X5&:(or (eq (class ?X5) Duration) (subclassp (class ?X5) Duration))) (truth 1))) => (send ?X5 put-end 733701) (add-prop (send ?Y35 get-what) (add-pred Has what [private]) (make-instance of Duration (start ?X3) (end -1.0)) 1))
(defrule 03e73b0a5bf74951973860cbca9ed3f4 (logical (object (is-a Proposition) (subject ?X1&:(or (eq (class ?X1) Person) (subclassp (class ?X1) Person))) (predicate ?Y38&:(or (eq (class ?Y38) IsOwner) (subclassp (class ?Y38) IsOwner))&:(or (eq (class (send ?Y38 get-of)) Content) (subclassp (class (send ?Y38 get-of)) Content))) (time ?X3&:(or (eq (class ?X3) Duration) (subclassp (class ?X3) Duration))) (truth 1))) => (add-prop ?X1 (add-pred Can what (add-pred Hide what (send ?Y38 get-of))) ?X3 1))
(reduce-class [john] Person)
(reduce-class [pete] Person)
(reduce-class [jane] Person)
(reduce-class [c1] Content)
(reduce-class [c2] Content)
(add-prop [john] (add-pred Has what [manager]) (make-instance of Duration (start 733701.0) (end -1.0)) 1)
(add-prop [jane] (add-pred Has what [create_perm]) (make-instance of Duration (start 733701.0) (end -1.0)) 1)
(add-prop [jane] (add-pred Wants to (add-pred Create what [c1])) 733701.0 1)
(add-prop [pete] (add-pred Wants to (add-pred Create what [c2])) 733701.0 1)
(find-all-instances ((?prop Proposition) (?Y40 IsOwner) (?Y41 Duration)) (and (eq ?prop:subject [jane]) (eq ?Y40:of [c1]) (eq ?prop:predicate ?Y40) (= ?Y41:start 733701.0) (= ?Y41:end -1.0) (eq ?prop:truth 1)))
0


no
----------running---------------------
----------runned: 33---------------------
(find-all-instances ((?prop Proposition) (?Y42 IsOwner) (?Y43 Duration)) (and (eq ?prop:subject [jane]) (eq ?Y42:of [c1]) (eq ?prop:predicate ?Y42) (= ?Y43:start 733701.0) (= ?Y43:end -1.0) (eq ?prop:truth 1)))
35


jane isowner of c1 at from 733701.0 till -1.0
(find-all-instances ((?prop Proposition) (?Y44 Has) (?Y45 Duration)) (and (eq ?prop:subject [c1]) (eq ?Y44:what [private]) (eq ?prop:predicate ?Y44) (= ?Y45:start 733701.0) (= ?Y45:end -1.0) (eq ?prop:truth 1)))
35


c1 has what private at from 733701.0 till -1.0
(find-all-instances ((?prop Proposition) (?Y46 IsOwner) (?Y47 Duration)) (and (eq ?prop:subject [pete]) (eq ?Y46:of [c2]) (eq ?prop:predicate ?Y46) (= ?Y47:start 733701.0) (= ?Y47:end -1.0) (eq ?prop:truth 1)))
0


no
(add-prop [jane] (add-pred Wants to (add-pred Publish what [c1])) 733701.0 1)
(add-prop [pete] (add-pred Wants to (add-pred Publish what [c2])) 733701.0 1)
----------running---------------------
----------runned: 0---------------------
(find-all-instances ((?prop Proposition) (?Y48 Has) (?Y49 Duration)) (and (eq ?prop:subject [c1]) (eq ?Y48:what [public]) (eq ?prop:predicate ?Y48) (= ?Y49:start 733701.0) (= ?Y49:end -1.0) (eq ?prop:truth 1)))
0


no
(find-all-instances ((?prop Proposition) (?Y50 Has) (?Y51 Duration)) (and (eq ?prop:subject [c2]) (eq ?Y50:what [public]) (eq ?prop:predicate ?Y50) (= ?Y51:start 733701.0) (= ?Y51:end -1.0) (eq ?prop:truth 1)))
0


no
(find-all-instances ((?prop Proposition) (?Y52 Can) (?Y53 View) (?Y54 Duration)) (and (eq ?prop:subject [jane]) (eq ?Y53:what [c1]) (eq ?Y52:what ?Y53) (eq ?prop:predicate ?Y52) (= ?Y54:start 733701.0) (= ?Y54:end -1.0) (eq ?prop:truth 1)))
35


jane can what view what c1 at from 733701.0 till -1.0
(find-all-instances ((?prop Proposition) (?Y55 Can) (?Y56 View) (?Y57 Duration)) (and (eq ?prop:subject [pete]) (eq ?Y56:what [c1]) (eq ?Y55:what ?Y56) (eq ?prop:predicate ?Y55) (= ?Y57:start 733701.0) (= ?Y57:end -1.0) (eq ?prop:truth 1)))
0


no
(add-prop [john] (add-pred Wants to (add-pred Publish what [c1])) 733701.0 1)
----------running---------------------
----------runned: 10---------------------
(find-all-instances ((?prop Proposition) (?Y58 Has) (?Y59 Duration)) (and (eq ?prop:subject [c1]) (eq ?Y58:what [private]) (eq ?prop:predicate ?Y58) (= ?Y59:start 733701.0) (= ?Y59:end -1.0) (eq ?prop:truth 1)))
0


no
(find-all-instances ((?prop Proposition) (?Y60 Has) (?Y61 Duration)) (and (eq ?prop:subject [c1]) (eq ?Y60:what [public]) (eq ?prop:predicate ?Y60) (= ?Y61:start 733701.0) (= ?Y61:end -1.0) (eq ?prop:truth 1)))
41


c1 has what public at from 733701.0 till -1.0
(find-all-instances ((?prop Proposition) (?Y62 Can) (?Y63 View) (?Y64 Duration)) (and (eq ?prop:subject [pete]) (eq ?Y63:what [c1]) (eq ?Y62:what ?Y63) (eq ?prop:predicate ?Y62) (= ?Y64:start 733701.0) (= ?Y64:end -1.0) (eq ?prop:truth 1)))
41


pete can what view what c1 at from 733701.0 till -1.0
(defclass Name (is-a USER))

(deffunction reduce-class (?instance ?class)
    (if (eq (length$
                (find-all-instances ((?a ?class))(eq (instance-name ?a) ?instance)))
             0)
    then (make-instance ?instance of ?class)
    else (return TRUE)))
(defclass Thing (is-a Name))
(defclass Verb (is-a USER))
(defclass State (is-a Verb) )
(set-sequence-operator-recognition TRUE)
(defmessage-handler State set-slots primary ($?slots)
        (while (> (length$ ?slots) 0) do
            (bind ?slot (first$ ?slots))
            (bind ?vslots (rest$ ?slots))
            (bind ?value (first$ ?vslots))
            (bind ?slots (rest$ ?vslots))
            (dynamic-put $?slot $?value))
        (return (instance-name ?self)))


(deffunction add-pred (?class $?slots)
        (bind ?key (str-cat ?class $?slots))
        (bind ?pos (str-index "." ?key))
        (while ?pos do
            (bind ?key (str-cat (sub-string 1 (- ?pos 1) ?key)
                                "_"
                                (sub-string (+ ?pos 1) (str-length ?key) ?key)))
            (bind ?pos (str-index "." ?key)))
        (bind ?key (sym-cat ?key))
        (if (instance-existp ?key) then
            (return (instance-name ?key))
         else
            (make-instance ?key of ?class)
            (return (send (instance-name ?key) set-slots $?slots))))

(defclass Duration (is-a Name) (slot start (type NUMBER) (pattern-match reactive)) (slot end (type NUMBER) (pattern-match reactive)))

(deffunction extract-now ($?instants)
    (if (= (length$ ?instants) 0) then
        (return -1)
    else
        (bind ?count 0)
        (bind ?not-now (create$))
        (while (> (length$ ?instants) 0) do
            (bind ?instant (nth$ 1 ?instants))
            (bind ?instants (rest$ ?instants))
            (if (= ?instant -1)
            then (bind ?count (+ ?count 1))
            else (bind ?not-now (insert$ ?not-now 1 ?instant)))
            )
        (return (insert$ ?not-now 1 ?count))
    )
)


(deffunction maxcomend ($?durations)
    (bind ?ends (create$))
    (while (> (length$ ?durations) 0)
        (bind ?ends (insert$ ?ends 1 (send (nth$ 1 ?durations) get-end)))
        (bind ?durations (rest$ ?durations))
        )
    (bind ?ret (extract-now ?ends))
    (bind ?pasts (rest$ ?ret))
    (if (> (length$ ?pasts) 0)
    then (return (min (expand$ ?pasts)))
    else (return -1)
    )
)


(deffunction mincomstart ($?durations)
    (bind ?starts (create$))
    (while (> (length$ ?durations) 0)
        (bind ?starts (insert$ ?starts 1 (send (nth$ 1 ?durations) get-start)))
        (bind ?durations (rest$ ?durations))
        )
    (bind ?ret (extract-now ?starts))
    (bind ?nows (nth$ 1 ?ret))
    (if (> ?nows 0)
    then (return -1)
    else (bind ?ret (rest$ ?ret))
         (return (max (expand$ ?ret)))
    )
)

(defclass Proposition (is-a Name) (slot truth (type INTEGER) (default 1) (pattern-match reactive)) (slot subject (type INSTANCE) (pattern-match reactive)) (slot predicate (type INSTANCE) (pattern-match reactive)) (slot time (type ?VARIABLE) (pattern-match reactive)))

(deffunction add-prop (?s ?p ?t ?r)
       (bind ?count 0)
       (do-for-all-instances ((?prop Proposition))
                          (and (eq ?prop:subject ?s)
                               (eq ?prop:predicate ?p)
                               (or (and (eq (class ?t) Duration)
                                        (= (send (send ?prop get-time) get-start) (send ?t get-start))
                                        (= (send (send ?prop get-time) get-end) (send ?t get-end)))
                                   (= ?prop:time ?t))
                               (= ?prop:truth ?r))
               (bind ?count (+ ?count 1)))
        (if (= ?count 0)
        then (make-instance of Proposition (subject ?s)
                                           (predicate ?p)
                                           (time ?t)
                                           (truth ?r))
        else (return TRUE)))
(defclass Person (is-a Thing))
(defclass Can (is-a State) (slot what (type INSTANCE) (visibility public) (pattern-match reactive)))
(defclass Wants (is-a State) (slot to (type INSTANCE) (visibility public) (pattern-match reactive)))
(defclass Has (is-a State) (slot what (type INSTANCE) (visibility public) (pattern-match reactive)))
(defclass IsNeeded (is-a State) (slot for_action (type INSTANCE) (visibility public) (pattern-match reactive)))
(defclass IsIn (is-a State) (slot what (type INSTANCE) (visibility public) (pattern-match reactive)))
(defclass Group (is-a Thing))
(defclass Permission (is-a Thing))
(defclass Role (is-a Thing))
(defclass Content (is-a Thing))
(defclass Create (is-a State) (slot what (type INSTANCE) (visibility public) (pattern-match reactive)))
(defclass IsOwner (is-a State) (slot of (type INSTANCE) (visibility public) (pattern-match reactive)))
(defclass Status (is-a Thing))
(defclass View (is-a State) (slot what (type INSTANCE) (visibility public) (pattern-match reactive)))
(defclass Publish (is-a State) (slot what (type INSTANCE) (visibility public) (pattern-match reactive)))
(defclass Hide (is-a State) (slot what (type INSTANCE) (visibility public) (pattern-match reactive)))
(reduce-class [admin] Person)
(reduce-class [member] Role)
(reduce-class [manager] Role)
(reduce-class [basic_perm] Permission)
(reduce-class [manage_perm] Permission)
(reduce-class [create_perm] Permission)
(reduce-class [public] Status)
(reduce-class [private] Status)
(add-prop [admin] (add-pred Has what [manager]) (make-instance of Duration (start 733701.0) (end -1.0)) 1)
(add-prop [member] (add-pred Has what [basic_perm]) (make-instance of Duration (start 733701.0) (end -1.0)) 1)
(defrule c271e60ae9e2454f9a22d7dc15a28e01 (logical (object (is-a Proposition) (subject ?X1&:(or (eq (class ?X1) Person) (subclassp (class ?X1) Person))) (predicate ?Y1&:(or (eq (class ?Y1) Wants) (subclassp (class ?Y1) Wants))&:(or (eq (class (send ?Y1 get-to)) Create) (subclassp (class (send ?Y1 get-to)) Create))&:(or (eq (class (send (send ?Y1 get-to) get-what)) Thing) (subclassp (class (send (send ?Y1 get-to) get-what)) Thing))) (time ?X2) (truth 1))) (logical (object (is-a Proposition) (subject ?X1) (predicate ?Y2&:(or (eq (class ?Y2) Has) (subclassp (class ?Y2) Has))&:(eq (send ?Y2 get-what) [create_perm])) (time ?X3&:(or (eq (class ?X3) Duration) (subclassp (class ?X3) Duration))) (truth 1))) (test (and (<= (send ?X3 get-start) ?X2) (or (= (send ?X3 get-end) -1) (>= (send ?X3 get-end) ?X2)))) => (add-prop ?X1 (add-pred Create what (send (send ?Y1 get-to) get-what)) ?X2 1))
(defrule 93d5581a71514a15ab974d03a0920877 (logical (object (is-a Proposition) (subject ?X1&:(or (eq (class ?X1) Person) (subclassp (class ?X1) Person))) (predicate ?Y4&:(or (eq (class ?Y4) Wants) (subclassp (class ?Y4) Wants))) (time ?X2) (truth 1))) (logical (object (is-a Proposition) (subject ?X1) (predicate ?Y5&:(or (eq (class ?Y5) Can) (subclassp (class ?Y5) Can))&:(eq (send ?Y4 get-to) (send ?Y5 get-what))) (time ?X3&:(or (eq (class ?X3) Duration) (subclassp (class ?X3) Duration))) (truth 1))) (test (and (<= (send ?X3 get-start) ?X2) (or (= (send ?X3 get-end) -1) (>= (send ?X3 get-end) ?X2)))) => (add-prop ?X1 (send ?Y4 get-to) ?X2 1))
(defrule 09e10e33460144e5ac6f6c5c9b73bb14 (logical (object (is-a Proposition) (subject ?X2&:(or (eq (class ?X2) Thing) (subclassp (class ?X2) Thing))) (predicate ?Y7&:(or (eq (class ?Y7) IsNeeded) (subclassp (class ?Y7) IsNeeded))) (time ?X3&:(or (eq (class ?X3) Duration) (subclassp (class ?X3) Duration))) (truth 1))) (logical (object (is-a Proposition) (subject ?X1&:(or (eq (class ?X1) Thing) (subclassp (class ?X1) Thing))) (predicate ?Y9&:(or (eq (class ?Y9) Has) (subclassp (class ?Y9) Has))&:(eq (send ?Y9 get-what) ?X2)) (time ?X5&:(or (eq (class ?X5) Duration) (subclassp (class ?X5) Duration))) (truth 1))) (test (or (and (>= (send ?X3 get-start) (send ?X3 get-start)) (or (<= (send ?X3 get-start) (send ?X3 get-end)) (= (send ?X3 get-end) -1))) (and (>= (send ?X3 get-start) (send ?X3 get-start)) (or (<= (send ?X3 get-start) (send ?X3 get-end)) (= (send ?X3 get-end) -1))))) => (add-prop ?X1 (add-pred Can what (send ?Y7 get-for_action)) (make-instance of Duration (start (mincomstart ?X3 ?X5)) (end (maxcomend ?X3 ?X5))) 1))
(defrule c65cddc4cf4e4b71b398f693885183c4 (logical (object (is-a Proposition) (subject ?X1&:(or (eq (class ?X1) Thing) (subclassp (class ?X1) Thing))) (predicate ?Y11&:(or (eq (class ?Y11) IsIn) (subclassp (class ?Y11) IsIn))&:(or (eq (class (send ?Y11 get-what)) Thing) (subclassp (class (send ?Y11 get-what)) Thing))) (time ?X4&:(or (eq (class ?X4) Duration) (subclassp (class ?X4) Duration))) (truth 1))) (logical (object (is-a Proposition) (subject ?X2&:(eq ?X2 (send ?Y11 get-what))) (predicate ?Y13&:(or (eq (class ?Y13) IsIn) (subclassp (class ?Y13) IsIn))&:(or (eq (class (send ?Y13 get-what)) Thing) (subclassp (class (send ?Y13 get-what)) Thing))) (time ?X5&:(or (eq (class ?X5) Duration) (subclassp (class ?X5) Duration))) (truth 1))) (test (or (and (>= (send ?X4 get-start) (send ?X4 get-start)) (or (<= (send ?X4 get-start) (send ?X4 get-end)) (= (send ?X4 get-end) -1))) (and (>= (send ?X4 get-start) (send ?X4 get-start)) (or (<= (send ?X4 get-start) (send ?X4 get-end)) (= (send ?X4 get-end) -1))))) => (add-prop ?X1 (add-pred IsIn what (send ?Y13 get-what)) (make-instance of Duration (start (mincomstart ?X4 ?X5)) (end (maxcomend ?X4 ?X5))) 1))
(defrule a6d70166f9524034ba21a4f309f9f574 (logical (object (is-a Proposition) (subject ?X1&:(or (eq (class ?X1) Person) (subclassp (class ?X1) Person))) (predicate ?Y15&:(or (eq (class ?Y15) IsIn) (subclassp (class ?Y15) IsIn))) (time ?X3&:(or (eq (class ?X3) Duration) (subclassp (class ?X3) Duration))) (truth 1))) (logical (object (is-a Proposition) (subject ?X2&:(or (eq (class ?X2) Group) (subclassp (class ?X2) Group))) (predicate ?Y17&:(or (eq (class ?Y17) Has) (subclassp (class ?Y17) Has))&:(or (eq (class (send ?Y17 get-what)) Permission) (subclassp (class (send ?Y17 get-what)) Permission))) (time ?X5&:(or (eq (class ?X5) Duration) (subclassp (class ?X5) Duration))) (truth 1))) (test (or (and (>= (send ?X3 get-start) (send ?X3 get-start)) (or (<= (send ?X3 get-start) (send ?X3 get-end)) (= (send ?X3 get-end) -1))) (and (>= (send ?X3 get-start) (send ?X3 get-start)) (or (<= (send ?X3 get-start) (send ?X3 get-end)) (= (send ?X3 get-end) -1))))) => (add-prop ?X1 (add-pred Has what (send ?Y17 get-what)) (make-instance of Duration (start (mincomstart ?X3 ?X5)) (end (maxcomend ?X3 ?X5))) 1))
(defrule b31f59a20ddc4ce88a252d2d0e781f82 (logical (object (is-a Proposition) (subject ?X1&:(or (eq (class ?X1) Person) (subclassp (class ?X1) Person))) (predicate ?Y19&:(or (eq (class ?Y19) Has) (subclassp (class ?Y19) Has))&:(or (eq (class (send ?Y19 get-what)) Role) (subclassp (class (send ?Y19 get-what)) Role))) (time ?X3&:(or (eq (class ?X3) Duration) (subclassp (class ?X3) Duration))) (truth 1))) (logical (object (is-a Proposition) (subject ?X2&:(eq ?X2 (send ?Y19 get-what))) (predicate ?Y21&:(or (eq (class ?Y21) Has) (subclassp (class ?Y21) Has))&:(or (eq (class (send ?Y21 get-what)) Permission) (subclassp (class (send ?Y21 get-what)) Permission))) (time ?X5&:(or (eq (class ?X5) Duration) (subclassp (class ?X5) Duration))) (truth 1))) (test (or (and (>= (send ?X3 get-start) (send ?X3 get-start)) (or (<= (send ?X3 get-start) (send ?X3 get-end)) (= (send ?X3 get-end) -1))) (and (>= (send ?X3 get-start) (send ?X3 get-start)) (or (<= (send ?X3 get-start) (send ?X3 get-end)) (= (send ?X3 get-end) -1))))) => (add-prop ?X1 (add-pred Has what (send ?Y21 get-what)) (make-instance of Duration (start (mincomstart ?X3 ?X5)) (end (maxcomend ?X3 ?X5))) 1))
(defrule ee483292593e4305afb3700161fb9b95 (logical (object (is-a Person) (name ?X1))) => (add-prop ?X1 (add-pred Has what [member]) (make-instance of Duration (start 733701.0) (end -1.0)) 1))
(defrule fe37808466e64656bfc09ecc5bba6ec1 (logical (object (is-a Proposition) (subject ?X1&:(or (eq (class ?X1) Person) (subclassp (class ?X1) Person))) (predicate ?Y23&:(or (eq (class ?Y23) Create) (subclassp (class ?Y23) Create))&:(or (eq (class (send ?Y23 get-what)) Content) (subclassp (class (send ?Y23 get-what)) Content))) (time ?X3) (truth 1))) => (reduce-class (send ?Y23 get-what) Content) (add-prop ?X1 (add-pred IsOwner of (send ?Y23 get-what)) (make-instance of Duration (start ?X3) (end -1.0)) 1) (add-prop (send ?Y23 get-what) (add-pred Has what [private]) (make-instance of Duration (start ?X3) (end -1.0)) 1))
(defrule a59a779b6b4e4c9e8a8214ec9012a8bb (logical (object (is-a Permission) (name ?X2))) => (add-prop [manager] (add-pred Has what ?X2) (make-instance of Duration (start 733701.0) (end -1.0)) 1))
(defrule d6f4db6762964da6979a6abac41357f3 (logical (object (is-a Proposition) (subject ?X1&:(or (eq (class ?X1) Content) (subclassp (class ?X1) Content))) (predicate ?Y24&:(or (eq (class ?Y24) Has) (subclassp (class ?Y24) Has))&:(eq (send ?Y24 get-what) [public])) (time ?X2&:(or (eq (class ?X2) Duration) (subclassp (class ?X2) Duration))) (truth 1))) => (add-prop [basic_perm] (add-pred IsNeeded for_action (add-pred View what ?X1)) ?X2 1))
(defrule 17f931d35ea94fab8266b7057e879396 (logical (object (is-a Proposition) (subject ?X1&:(or (eq (class ?X1) Content) (subclassp (class ?X1) Content))) (predicate ?Y26&:(or (eq (class ?Y26) Has) (subclassp (class ?Y26) Has))&:(eq (send ?Y26 get-what) [private])) (time ?X2&:(or (eq (class ?X2) Duration) (subclassp (class ?X2) Duration))) (truth 1))) => (add-prop [manage_perm] (add-pred IsNeeded for_action (add-pred View what ?X1)) ?X2 1))
(defrule 862032ee359343f8a7de798c56eee994 (logical (object (is-a Proposition) (subject ?X1&:(or (eq (class ?X1) Content) (subclassp (class ?X1) Content))) (predicate ?Y28&:(or (eq (class ?Y28) Has) (subclassp (class ?Y28) Has))&:(eq (send ?Y28 get-what) [private])) (time ?X3&:(or (eq (class ?X3) Duration) (subclassp (class ?X3) Duration))) (truth 1))) (logical (object (is-a Proposition) (subject ?X2&:(or (eq (class ?X2) Person) (subclassp (class ?X2) Person))) (predicate ?Y30&:(or (eq (class ?Y30) IsOwner) (subclassp (class ?Y30) IsOwner))&:(eq (send ?Y30 get-of) ?X1)) (time ?X4&:(or (eq (class ?X4) Duration) (subclassp (class ?X4) Duration))) (truth 1))) (test (or (and (>= (send ?X3 get-start) (send ?X3 get-start)) (or (<= (send ?X3 get-start) (send ?X3 get-end)) (= (send ?X3 get-end) -1))) (and (>= (send ?X3 get-start) (send ?X3 get-start)) (or (<= (send ?X3 get-start) (send ?X3 get-end)) (= (send ?X3 get-end) -1))))) => (add-prop ?X2 (add-pred Can what (add-pred View what ?X1)) (make-instance of Duration (start (mincomstart ?X3 ?X4)) (end (maxcomend ?X3 ?X4))) 1))
(defrule 46e9ab0db4f04b7f99b2ca81531c3c4a (logical (object (is-a Proposition) (subject ?X1&:(or (eq (class ?X1) Person) (subclassp (class ?X1) Person))) (predicate ?Y32&:(or (eq (class ?Y32) Publish) (subclassp (class ?Y32) Publish))&:(or (eq (class (send ?Y32 get-what)) Content) (subclassp (class (send ?Y32 get-what)) Content))) (time ?X3) (truth 1))) (logical (object (is-a Proposition) (subject ?X2&:(eq ?X2 (send ?Y32 get-what))) (predicate ?Y33&:(or (eq (class ?Y33) Has) (subclassp (class ?Y33) Has))&:(or (eq (class (send ?Y33 get-what)) Status) (subclassp (class (send ?Y33 get-what)) Status))) (time ?X5&:(or (eq (class ?X5) Duration) (subclassp (class ?X5) Duration))) (truth 1))) => (send ?X5 put-end 733701) (add-prop (send ?Y32 get-what) (add-pred Has what [public]) (make-instance of Duration (start ?X3) (end -1.0)) 1))
(defrule cf9ebcea1ed34151bfd989fcfb402cfe (logical (object (is-a Content) (name ?X1))) => (add-prop [manage_perm] (add-pred IsNeeded for_action (add-pred Publish what ?X1)) (make-instance of Duration (start 733701.0) (end -1.0)) 1))
(defrule 9593ebd31d5146289d00b1930260aa2f (logical (object (is-a Proposition) (subject ?X1&:(or (eq (class ?X1) Person) (subclassp (class ?X1) Person))) (predicate ?Y35&:(or (eq (class ?Y35) Hide) (subclassp (class ?Y35) Hide))&:(or (eq (class (send ?Y35 get-what)) Content) (subclassp (class (send ?Y35 get-what)) Content))) (time ?X3) (truth 1))) (logical (object (is-a Proposition) (subject ?X2&:(eq ?X2 (send ?Y35 get-what))) (predicate ?Y36&:(or (eq (class ?Y36) Has) (subclassp (class ?Y36) Has))&:(or (eq (class (send ?Y36 get-what)) Status) (subclassp (class (send ?Y36 get-what)) Status))) (time ?X5&:(or (eq (class ?X5) Duration) (subclassp (class ?X5) Duration))) (truth 1))) => (send ?X5 put-end 733701) (add-prop (send ?Y35 get-what) (add-pred Has what [private]) (make-instance of Duration (start ?X3) (end -1.0)) 1))
(defrule bd9f2c9acf9444319355a0bf9f36579c (logical (object (is-a Proposition) (subject ?X1&:(or (eq (class ?X1) Person) (subclassp (class ?X1) Person))) (predicate ?Y38&:(or (eq (class ?Y38) IsOwner) (subclassp (class ?Y38) IsOwner))&:(or (eq (class (send ?Y38 get-of)) Content) (subclassp (class (send ?Y38 get-of)) Content))) (time ?X3&:(or (eq (class ?X3) Duration) (subclassp (class ?X3) Duration))) (truth 1))) => (add-prop ?X1 (add-pred Can what (add-pred Hide what (send ?Y38 get-of))) ?X3 1))
(reduce-class [john] Person)
(reduce-class [pete] Person)
(reduce-class [jane] Person)
(reduce-class [c1] Content)
(reduce-class [c2] Content)
(add-prop [john] (add-pred Has what [manager]) (make-instance of Duration (start 733701.0) (end -1.0)) 1)
(add-prop [jane] (add-pred Has what [create_perm]) (make-instance of Duration (start 733701.0) (end -1.0)) 1)
(add-prop [jane] (add-pred Wants to (add-pred Create what [c1])) 733701.0 1)
(add-prop [pete] (add-pred Wants to (add-pred Create what [c2])) 733701.0 1)
(find-all-instances ((?prop Proposition) (?Y40 IsOwner) (?Y41 Duration)) (and (eq ?prop:subject [jane]) (eq ?Y40:of [c1]) (eq ?prop:predicate ?Y40) (= ?Y41:start 733701.0) (= ?Y41:end -1.0) (eq ?prop:truth 1)))
0


no
----------running---------------------
----------runned: 33---------------------
(find-all-instances ((?prop Proposition) (?Y42 IsOwner) (?Y43 Duration)) (and (eq ?prop:subject [jane]) (eq ?Y42:of [c1]) (eq ?prop:predicate ?Y42) (= ?Y43:start 733701.0) (= ?Y43:end -1.0) (eq ?prop:truth 1)))
35


jane isowner of c1 at from 733701.0 till -1.0
(find-all-instances ((?prop Proposition) (?Y44 Has) (?Y45 Duration)) (and (eq ?prop:subject [c1]) (eq ?Y44:what [private]) (eq ?prop:predicate ?Y44) (= ?Y45:start 733701.0) (= ?Y45:end -1.0) (eq ?prop:truth 1)))
35


c1 has what private at from 733701.0 till -1.0
(find-all-instances ((?prop Proposition) (?Y46 IsOwner) (?Y47 Duration)) (and (eq ?prop:subject [pete]) (eq ?Y46:of [c2]) (eq ?prop:predicate ?Y46) (= ?Y47:start 733701.0) (= ?Y47:end -1.0) (eq ?prop:truth 1)))
0


no
(add-prop [jane] (add-pred Wants to (add-pred Publish what [c1])) 733701.0 1)
(add-prop [pete] (add-pred Wants to (add-pred Publish what [c2])) 733701.0 1)
----------running---------------------
----------runned: 0---------------------
(find-all-instances ((?prop Proposition) (?Y48 Has) (?Y49 Duration)) (and (eq ?prop:subject [c1]) (eq ?Y48:what [public]) (eq ?prop:predicate ?Y48) (= ?Y49:start 733701.0) (= ?Y49:end -1.0) (eq ?prop:truth 1)))
0


no
(find-all-instances ((?prop Proposition) (?Y50 Has) (?Y51 Duration)) (and (eq ?prop:subject [c2]) (eq ?Y50:what [public]) (eq ?prop:predicate ?Y50) (= ?Y51:start 733701.0) (= ?Y51:end -1.0) (eq ?prop:truth 1)))
0


no
(find-all-instances ((?prop Proposition) (?Y52 Can) (?Y53 View) (?Y54 Duration)) (and (eq ?prop:subject [jane]) (eq ?Y53:what [c1]) (eq ?Y52:what ?Y53) (eq ?prop:predicate ?Y52) (= ?Y54:start 733701.0) (= ?Y54:end -1.0) (eq ?prop:truth 1)))
35


jane can what view what c1 at from 733701.0 till -1.0
(find-all-instances ((?prop Proposition) (?Y55 Can) (?Y56 View) (?Y57 Duration)) (and (eq ?prop:subject [pete]) (eq ?Y56:what [c1]) (eq ?Y55:what ?Y56) (eq ?prop:predicate ?Y55) (= ?Y57:start 733701.0) (= ?Y57:end -1.0) (eq ?prop:truth 1)))
0


no
(add-prop [john] (add-pred Wants to (add-pred Publish what [c1])) 733701.0 1)
----------running---------------------
----------runned: 10---------------------
(find-all-instances ((?prop Proposition) (?Y58 Has) (?Y59 Duration)) (and (eq ?prop:subject [c1]) (eq ?Y58:what [private]) (eq ?prop:predicate ?Y58) (= ?Y59:start 733701.0) (= ?Y59:end -1.0) (eq ?prop:truth 1)))
0


no
(find-all-instances ((?prop Proposition) (?Y60 Has) (?Y61 Duration)) (and (eq ?prop:subject [c1]) (eq ?Y60:what [public]) (eq ?prop:predicate ?Y60) (= ?Y61:start 733701.0) (= ?Y61:end -1.0) (eq ?prop:truth 1)))
41


c1 has what public at from 733701.0 till -1.0
(find-all-instances ((?prop Proposition) (?Y62 Can) (?Y63 View) (?Y64 Duration)) (and (eq ?prop:subject [pete]) (eq ?Y63:what [c1]) (eq ?Y62:what ?Y63) (eq ?prop:predicate ?Y62) (= ?Y64:start 733701.0) (= ?Y64:end -1.0) (eq ?prop:truth 1)))
41


pete can what view what c1 at from 733701.0 till -1.0
(defclass Name (is-a USER))

(deffunction reduce-class (?instance ?class)
    (if (eq (length$
                (find-all-instances ((?a ?class))(eq (instance-name ?a) ?instance)))
             0)
    then (make-instance ?instance of ?class)
    else (return TRUE)))
(defclass Thing (is-a Name))
(defclass Verb (is-a USER))
(defclass State (is-a Verb) )
(set-sequence-operator-recognition TRUE)
(defmessage-handler State set-slots primary ($?slots)
        (while (> (length$ ?slots) 0) do
            (bind ?slot (first$ ?slots))
            (bind ?vslots (rest$ ?slots))
            (bind ?value (first$ ?vslots))
            (bind ?slots (rest$ ?vslots))
            (dynamic-put $?slot $?value))
        (return (instance-name ?self)))


(deffunction add-pred (?class $?slots)
        (bind ?key (str-cat ?class $?slots))
        (bind ?pos (str-index "." ?key))
        (while ?pos do
            (bind ?key (str-cat (sub-string 1 (- ?pos 1) ?key)
                                "_"
                                (sub-string (+ ?pos 1) (str-length ?key) ?key)))
            (bind ?pos (str-index "." ?key)))
        (bind ?key (sym-cat ?key))
        (if (instance-existp ?key) then
            (return (instance-name ?key))
         else
            (make-instance ?key of ?class)
            (return (send (instance-name ?key) set-slots $?slots))))

(defclass Duration (is-a Name) (slot start (type NUMBER) (pattern-match reactive)) (slot end (type NUMBER) (pattern-match reactive)))

(deffunction extract-now ($?instants)
    (if (= (length$ ?instants) 0) then
        (return -1)
    else
        (bind ?count 0)
        (bind ?not-now (create$))
        (while (> (length$ ?instants) 0) do
            (bind ?instant (nth$ 1 ?instants))
            (bind ?instants (rest$ ?instants))
            (if (= ?instant -1)
            then (bind ?count (+ ?count 1))
            else (bind ?not-now (insert$ ?not-now 1 ?instant)))
            )
        (return (insert$ ?not-now 1 ?count))
    )
)


(deffunction maxcomend ($?durations)
    (bind ?ends (create$))
    (while (> (length$ ?durations) 0)
        (bind ?ends (insert$ ?ends 1 (send (nth$ 1 ?durations) get-end)))
        (bind ?durations (rest$ ?durations))
        )
    (bind ?ret (extract-now ?ends))
    (bind ?pasts (rest$ ?ret))
    (if (> (length$ ?pasts) 0)
    then (return (min (expand$ ?pasts)))
    else (return -1)
    )
)


(deffunction mincomstart ($?durations)
    (bind ?starts (create$))
    (while (> (length$ ?durations) 0)
        (bind ?starts (insert$ ?starts 1 (send (nth$ 1 ?durations) get-start)))
        (bind ?durations (rest$ ?durations))
        )
    (bind ?ret (extract-now ?starts))
    (bind ?nows (nth$ 1 ?ret))
    (if (> ?nows 0)
    then (return -1)
    else (bind ?ret (rest$ ?ret))
         (return (max (expand$ ?ret)))
    )
)

(defclass Proposition (is-a Name) (slot truth (type INTEGER) (default 1) (pattern-match reactive)) (slot subject (type INSTANCE) (pattern-match reactive)) (slot predicate (type INSTANCE) (pattern-match reactive)) (slot time (type ?VARIABLE) (pattern-match reactive)))

(deffunction add-prop (?s ?p ?t ?r)
       (bind ?count 0)
       (do-for-all-instances ((?prop Proposition))
                          (and (eq ?prop:subject ?s)
                               (eq ?prop:predicate ?p)
                               (or (and (eq (class ?t) Duration)
                                        (= (send (send ?prop get-time) get-start) (send ?t get-start))
                                        (= (send (send ?prop get-time) get-end) (send ?t get-end)))
                                   (= ?prop:time ?t))
                               (= ?prop:truth ?r))
               (bind ?count (+ ?count 1)))
        (if (= ?count 0)
        then (make-instance of Proposition (subject ?s)
                                           (predicate ?p)
                                           (time ?t)
                                           (truth ?r))
        else (return TRUE)))
(defclass Person (is-a Thing))
(defclass Can (is-a State) (slot what (type INSTANCE) (visibility public) (pattern-match reactive)))
(defclass Wants (is-a State) (slot to (type INSTANCE) (visibility public) (pattern-match reactive)))
(defclass Has (is-a State) (slot what (type INSTANCE) (visibility public) (pattern-match reactive)))
(defclass IsNeeded (is-a State) (slot for_action (type INSTANCE) (visibility public) (pattern-match reactive)))
(defclass IsIn (is-a State) (slot what (type INSTANCE) (visibility public) (pattern-match reactive)))
(defclass Group (is-a Thing))
(defclass Permission (is-a Thing))
(defclass Role (is-a Thing))
(defclass Content (is-a Thing))
(defclass Create (is-a State) (slot what (type INSTANCE) (visibility public) (pattern-match reactive)))
(defclass IsOwner (is-a State) (slot of (type INSTANCE) (visibility public) (pattern-match reactive)))
(defclass Status (is-a Thing))
(defclass View (is-a State) (slot what (type INSTANCE) (visibility public) (pattern-match reactive)))
(defclass Publish (is-a State) (slot what (type INSTANCE) (visibility public) (pattern-match reactive)))
(defclass Hide (is-a State) (slot what (type INSTANCE) (visibility public) (pattern-match reactive)))
(reduce-class [admin] Person)
(reduce-class [member] Role)
(reduce-class [manager] Role)
(reduce-class [basic_perm] Permission)
(reduce-class [manage_perm] Permission)
(reduce-class [create_perm] Permission)
(reduce-class [public] Status)
(reduce-class [private] Status)
(add-prop [admin] (add-pred Has what [manager]) (make-instance of Duration (start 733701.0) (end -1.0)) 1)
(add-prop [member] (add-pred Has what [basic_perm]) (make-instance of Duration (start 733701.0) (end -1.0)) 1)
(defrule d9b9195362fb4b85b3b2dbfa4cd25208 (logical (object (is-a Proposition) (subject ?X1&:(or (eq (class ?X1) Person) (subclassp (class ?X1) Person))) (predicate ?Y1&:(or (eq (class ?Y1) Wants) (subclassp (class ?Y1) Wants))&:(or (eq (class (send ?Y1 get-to)) Create) (subclassp (class (send ?Y1 get-to)) Create))&:(or (eq (class (send (send ?Y1 get-to) get-what)) Thing) (subclassp (class (send (send ?Y1 get-to) get-what)) Thing))) (time ?X2) (truth 1))) (logical (object (is-a Proposition) (subject ?X1) (predicate ?Y2&:(or (eq (class ?Y2) Has) (subclassp (class ?Y2) Has))&:(eq (send ?Y2 get-what) [create_perm])) (time ?X3&:(or (eq (class ?X3) Duration) (subclassp (class ?X3) Duration))) (truth 1))) (test (and (<= (send ?X3 get-start) ?X2) (or (= (send ?X3 get-end) -1) (>= (send ?X3 get-end) ?X2)))) => (add-prop ?X1 (add-pred Create what (send (send ?Y1 get-to) get-what)) ?X2 1))
(defrule 42707fb41ac0458c9e7cae948304167d (logical (object (is-a Proposition) (subject ?X1&:(or (eq (class ?X1) Person) (subclassp (class ?X1) Person))) (predicate ?Y4&:(or (eq (class ?Y4) Wants) (subclassp (class ?Y4) Wants))) (time ?X2) (truth 1))) (logical (object (is-a Proposition) (subject ?X1) (predicate ?Y5&:(or (eq (class ?Y5) Can) (subclassp (class ?Y5) Can))&:(eq (send ?Y4 get-to) (send ?Y5 get-what))) (time ?X3&:(or (eq (class ?X3) Duration) (subclassp (class ?X3) Duration))) (truth 1))) (test (and (<= (send ?X3 get-start) ?X2) (or (= (send ?X3 get-end) -1) (>= (send ?X3 get-end) ?X2)))) => (add-prop ?X1 (send ?Y4 get-to) ?X2 1))
(defclass Name (is-a USER))

(deffunction reduce-class (?instance ?class)
    (if (eq (length$
                (find-all-instances ((?a ?class))(eq (instance-name ?a) ?instance)))
             0)
    then (make-instance ?instance of ?class)
    else (return TRUE)))
(defclass Thing (is-a Name))
(defclass Verb (is-a USER))
(defclass State (is-a Verb) )
(set-sequence-operator-recognition TRUE)
(defmessage-handler State set-slots primary ($?slots)
        (while (> (length$ ?slots) 0) do
            (bind ?slot (first$ ?slots))
            (bind ?vslots (rest$ ?slots))
            (bind ?value (first$ ?vslots))
            (bind ?slots (rest$ ?vslots))
            (dynamic-put $?slot $?value))
        (return (instance-name ?self)))


(deffunction add-pred (?class $?slots)
        (bind ?key (str-cat ?class $?slots))
        (bind ?pos (str-index "." ?key))
        (while ?pos do
            (bind ?key (str-cat (sub-string 1 (- ?pos 1) ?key)
                                "_"
                                (sub-string (+ ?pos 1) (str-length ?key) ?key)))
            (bind ?pos (str-index "." ?key)))
        (bind ?key (sym-cat ?key))
        (if (instance-existp ?key) then
            (return (instance-name ?key))
         else
            (make-instance ?key of ?class)
            (return (send (instance-name ?key) set-slots $?slots))))

(defclass Duration (is-a Name) (slot start (type NUMBER) (pattern-match reactive)) (slot end (type NUMBER) (pattern-match reactive)))

(deffunction extract-now ($?instants)
    (if (= (length$ ?instants) 0) then
        (return -1)
    else
        (bind ?count 0)
        (bind ?not-now (create$))
        (while (> (length$ ?instants) 0) do
            (bind ?instant (nth$ 1 ?instants))
            (bind ?instants (rest$ ?instants))
            (if (= ?instant -1)
            then (bind ?count (+ ?count 1))
            else (bind ?not-now (insert$ ?not-now 1 ?instant)))
            )
        (return (insert$ ?not-now 1 ?count))
    )
)


(deffunction maxcomend ($?durations)
    (bind ?ends (create$))
    (while (> (length$ ?durations) 0)
        (bind ?ends (insert$ ?ends 1 (send (nth$ 1 ?durations) get-end)))
        (bind ?durations (rest$ ?durations))
        )
    (bind ?ret (extract-now ?ends))
    (bind ?pasts (rest$ ?ret))
    (if (> (length$ ?pasts) 0)
    then (return (min (expand$ ?pasts)))
    else (return -1)
    )
)


(deffunction mincomstart ($?durations)
    (bind ?starts (create$))
    (while (> (length$ ?durations) 0)
        (bind ?starts (insert$ ?starts 1 (send (nth$ 1 ?durations) get-start)))
        (bind ?durations (rest$ ?durations))
        )
    (bind ?ret (extract-now ?starts))
    (bind ?nows (nth$ 1 ?ret))
    (if (> ?nows 0)
    then (return -1)
    else (bind ?ret (rest$ ?ret))
         (return (max (expand$ ?ret)))
    )
)

(defclass Proposition (is-a Name) (slot truth (type INTEGER) (default 1) (pattern-match reactive)) (slot subject (type INSTANCE) (pattern-match reactive)) (slot predicate (type INSTANCE) (pattern-match reactive)) (slot time (type ?VARIABLE) (pattern-match reactive)))

(deffunction add-prop (?s ?p ?t ?r)
       (bind ?count 0)
       (do-for-all-instances ((?prop Proposition))
                          (and (eq ?prop:subject ?s)
                               (eq ?prop:predicate ?p)
                               (or (and (eq (class ?t) Duration)
                                        (= (send (send ?prop get-time) get-start) (send ?t get-start))
                                        (= (send (send ?prop get-time) get-end) (send ?t get-end)))
                                   (= ?prop:time ?t))
                               (= ?prop:truth ?r))
               (bind ?count (+ ?count 1)))
        (if (= ?count 0)
        then (make-instance of Proposition (subject ?s)
                                           (predicate ?p)
                                           (time ?t)
                                           (truth ?r))
        else (return TRUE)))
(defclass Person (is-a Thing))
(defclass Can (is-a State) (slot what (type INSTANCE) (visibility public) (pattern-match reactive)))
(defclass Wants (is-a State) (slot to (type INSTANCE) (visibility public) (pattern-match reactive)))
(defclass Has (is-a State) (slot what (type INSTANCE) (visibility public) (pattern-match reactive)))
(defclass IsNeeded (is-a State) (slot for_action (type INSTANCE) (visibility public) (pattern-match reactive)))
(defclass IsIn (is-a State) (slot what (type INSTANCE) (visibility public) (pattern-match reactive)))
(defclass Group (is-a Thing))
(defclass Permission (is-a Thing))
(defclass Role (is-a Thing))
(defclass Content (is-a Thing))
(defclass Create (is-a State) (slot what (type INSTANCE) (visibility public) (pattern-match reactive)))
(defclass IsOwner (is-a State) (slot of (type INSTANCE) (visibility public) (pattern-match reactive)))
(defclass Status (is-a Thing))
(defclass View (is-a State) (slot what (type INSTANCE) (visibility public) (pattern-match reactive)))
(defclass Publish (is-a State) (slot what (type INSTANCE) (visibility public) (pattern-match reactive)))
(defclass Hide (is-a State) (slot what (type INSTANCE) (visibility public) (pattern-match reactive)))
(reduce-class [admin] Person)
(reduce-class [member] Role)
(reduce-class [manager] Role)
(reduce-class [basic_perm] Permission)
(reduce-class [manage_perm] Permission)
(reduce-class [create_perm] Permission)
(reduce-class [public] Status)
(reduce-class [private] Status)
(add-prop [admin] (add-pred Has what [manager]) (make-instance of Duration (start 733701.0) (end -1.0)) 1)
(add-prop [member] (add-pred Has what [basic_perm]) (make-instance of Duration (start 733701.0) (end -1.0)) 1)
(defrule 467a26870afc405ea8c050a65cc20b1f (logical (object (is-a Proposition) (subject ?X1&:(or (eq (class ?X1) Person) (subclassp (class ?X1) Person))) (predicate ?Y1&:(or (eq (class ?Y1) Wants) (subclassp (class ?Y1) Wants))&:(or (eq (class (send ?Y1 get-to)) Create) (subclassp (class (send ?Y1 get-to)) Create))&:(or (eq (class (send (send ?Y1 get-to) get-what)) Thing) (subclassp (class (send (send ?Y1 get-to) get-what)) Thing))) (time ?X2) (truth 1))) (logical (object (is-a Proposition) (subject ?X1) (predicate ?Y2&:(or (eq (class ?Y2) Has) (subclassp (class ?Y2) Has))&:(eq (send ?Y2 get-what) [create_perm])) (time ?X3&:(or (eq (class ?X3) Duration) (subclassp (class ?X3) Duration))) (truth 1))) (test (and (<= (send ?X3 get-start) ?X2) (or (= (send ?X3 get-end) -1) (>= (send ?X3 get-end) ?X2)))) => (add-prop ?X1 (add-pred Create what (send (send ?Y1 get-to) get-what)) ?X2 1))
(defrule 28de7f65794042698a30c89d4675b75e (logical (object (is-a Proposition) (subject ?X1&:(or (eq (class ?X1) Person) (subclassp (class ?X1) Person))) (predicate ?Y4&:(or (eq (class ?Y4) Wants) (subclassp (class ?Y4) Wants))) (time ?X2) (truth 1))) (logical (object (is-a Proposition) (subject ?X1) (predicate ?Y5&:(or (eq (class ?Y5) Can) (subclassp (class ?Y5) Can))&:(eq (send ?Y4 get-to) (send ?Y5 get-what))) (time ?X3&:(or (eq (class ?X3) Duration) (subclassp (class ?X3) Duration))) (truth 1))) (test (and (<= (send ?X3 get-start) ?X2) (or (= (send ?X3 get-end) -1) (>= (send ?X3 get-end) ?X2)))) => (add-prop ?X1 (send ?Y4 get-to) ?X2 1))
(defrule b77bde4d5635468289a142e8d4774cee (logical (object (is-a Proposition) (subject ?X2&:(or (eq (class ?X2) Thing) (subclassp (class ?X2) Thing))) (predicate ?Y7&:(or (eq (class ?Y7) IsNeeded) (subclassp (class ?Y7) IsNeeded))) (time ?X3&:(or (eq (class ?X3) Duration) (subclassp (class ?X3) Duration))) (truth 1))) (logical (object (is-a Proposition) (subject ?X1&:(or (eq (class ?X1) Thing) (subclassp (class ?X1) Thing))) (predicate ?Y9&:(or (eq (class ?Y9) Has) (subclassp (class ?Y9) Has))&:(eq (send ?Y9 get-what) ?X2)) (time ?X5&:(or (eq (class ?X5) Duration) (subclassp (class ?X5) Duration))) (truth 1))) 
                (test (or (and (> (mincomstart ?X3 ?X5) -1)
                               (<= (mincomstart ?X3 ?X5) (maxcomend ?X3 ?X5)))
                          (= (maxcomend ?X3 ?X5) -1))
                )
                 => (add-prop ?X1 (add-pred Can what (send ?Y7 get-for_action)) (make-instance of Duration (start (mincomstart ?X3 ?X5)) (end (maxcomend ?X3 ?X5))) 1))
(defrule f61069b18c6f40d89e3d436398b70473 (logical (object (is-a Proposition) (subject ?X1&:(or (eq (class ?X1) Thing) (subclassp (class ?X1) Thing))) (predicate ?Y11&:(or (eq (class ?Y11) IsIn) (subclassp (class ?Y11) IsIn))&:(or (eq (class (send ?Y11 get-what)) Thing) (subclassp (class (send ?Y11 get-what)) Thing))) (time ?X4&:(or (eq (class ?X4) Duration) (subclassp (class ?X4) Duration))) (truth 1))) (logical (object (is-a Proposition) (subject ?X2&:(eq ?X2 (send ?Y11 get-what))) (predicate ?Y13&:(or (eq (class ?Y13) IsIn) (subclassp (class ?Y13) IsIn))&:(or (eq (class (send ?Y13 get-what)) Thing) (subclassp (class (send ?Y13 get-what)) Thing))) (time ?X5&:(or (eq (class ?X5) Duration) (subclassp (class ?X5) Duration))) (truth 1))) 
                (test (or (and (> (mincomstart ?X4 ?X5) -1)
                               (<= (mincomstart ?X4 ?X5) (maxcomend ?X4 ?X5)))
                          (= (maxcomend ?X4 ?X5) -1))
                )
                 => (add-prop ?X1 (add-pred IsIn what (send ?Y13 get-what)) (make-instance of Duration (start (mincomstart ?X4 ?X5)) (end (maxcomend ?X4 ?X5))) 1))
(defrule 6a2a2e51ebbc4750bb297da9005d604e (logical (object (is-a Proposition) (subject ?X1&:(or (eq (class ?X1) Person) (subclassp (class ?X1) Person))) (predicate ?Y15&:(or (eq (class ?Y15) IsIn) (subclassp (class ?Y15) IsIn))) (time ?X3&:(or (eq (class ?X3) Duration) (subclassp (class ?X3) Duration))) (truth 1))) (logical (object (is-a Proposition) (subject ?X2&:(or (eq (class ?X2) Group) (subclassp (class ?X2) Group))) (predicate ?Y17&:(or (eq (class ?Y17) Has) (subclassp (class ?Y17) Has))&:(or (eq (class (send ?Y17 get-what)) Permission) (subclassp (class (send ?Y17 get-what)) Permission))) (time ?X5&:(or (eq (class ?X5) Duration) (subclassp (class ?X5) Duration))) (truth 1))) 
                (test (or (and (> (mincomstart ?X3 ?X5) -1)
                               (<= (mincomstart ?X3 ?X5) (maxcomend ?X3 ?X5)))
                          (= (maxcomend ?X3 ?X5) -1))
                )
                 => (add-prop ?X1 (add-pred Has what (send ?Y17 get-what)) (make-instance of Duration (start (mincomstart ?X3 ?X5)) (end (maxcomend ?X3 ?X5))) 1))
(defrule f4fd4611787d4c2488c6127310008351 (logical (object (is-a Proposition) (subject ?X1&:(or (eq (class ?X1) Person) (subclassp (class ?X1) Person))) (predicate ?Y19&:(or (eq (class ?Y19) Has) (subclassp (class ?Y19) Has))&:(or (eq (class (send ?Y19 get-what)) Role) (subclassp (class (send ?Y19 get-what)) Role))) (time ?X3&:(or (eq (class ?X3) Duration) (subclassp (class ?X3) Duration))) (truth 1))) (logical (object (is-a Proposition) (subject ?X2&:(eq ?X2 (send ?Y19 get-what))) (predicate ?Y21&:(or (eq (class ?Y21) Has) (subclassp (class ?Y21) Has))&:(or (eq (class (send ?Y21 get-what)) Permission) (subclassp (class (send ?Y21 get-what)) Permission))) (time ?X5&:(or (eq (class ?X5) Duration) (subclassp (class ?X5) Duration))) (truth 1))) 
                (test (or (and (> (mincomstart ?X3 ?X5) -1)
                               (<= (mincomstart ?X3 ?X5) (maxcomend ?X3 ?X5)))
                          (= (maxcomend ?X3 ?X5) -1))
                )
                 => (add-prop ?X1 (add-pred Has what (send ?Y21 get-what)) (make-instance of Duration (start (mincomstart ?X3 ?X5)) (end (maxcomend ?X3 ?X5))) 1))
(defrule e1e2dad8164b47f89d542692a55cacaf (logical (object (is-a Person) (name ?X1))) => (add-prop ?X1 (add-pred Has what [member]) (make-instance of Duration (start 733701.0) (end -1.0)) 1))
(defrule ca89f8b7e88b4cf695a9174c1402c62b (logical (object (is-a Proposition) (subject ?X1&:(or (eq (class ?X1) Person) (subclassp (class ?X1) Person))) (predicate ?Y23&:(or (eq (class ?Y23) Create) (subclassp (class ?Y23) Create))&:(or (eq (class (send ?Y23 get-what)) Content) (subclassp (class (send ?Y23 get-what)) Content))) (time ?X3) (truth 1))) => (reduce-class (send ?Y23 get-what) Content) (add-prop ?X1 (add-pred IsOwner of (send ?Y23 get-what)) (make-instance of Duration (start ?X3) (end -1.0)) 1) (add-prop (send ?Y23 get-what) (add-pred Has what [private]) (make-instance of Duration (start ?X3) (end -1.0)) 1))
(defrule cc7974e6c8844a2e8b497fd3831a4bf6 (logical (object (is-a Permission) (name ?X2))) => (add-prop [manager] (add-pred Has what ?X2) (make-instance of Duration (start 733701.0) (end -1.0)) 1))
(defrule 00d4ef4bd7ed4f45bca8b33e77928216 (logical (object (is-a Proposition) (subject ?X1&:(or (eq (class ?X1) Content) (subclassp (class ?X1) Content))) (predicate ?Y24&:(or (eq (class ?Y24) Has) (subclassp (class ?Y24) Has))&:(eq (send ?Y24 get-what) [public])) (time ?X2&:(or (eq (class ?X2) Duration) (subclassp (class ?X2) Duration))) (truth 1))) => (add-prop [basic_perm] (add-pred IsNeeded for_action (add-pred View what ?X1)) ?X2 1))
(defrule bbba3a13033b476982a08891079174de (logical (object (is-a Proposition) (subject ?X1&:(or (eq (class ?X1) Content) (subclassp (class ?X1) Content))) (predicate ?Y26&:(or (eq (class ?Y26) Has) (subclassp (class ?Y26) Has))&:(eq (send ?Y26 get-what) [private])) (time ?X2&:(or (eq (class ?X2) Duration) (subclassp (class ?X2) Duration))) (truth 1))) => (add-prop [manage_perm] (add-pred IsNeeded for_action (add-pred View what ?X1)) ?X2 1))
(defrule 516b0b9359cf456e9d54239abdcd6d17 (logical (object (is-a Proposition) (subject ?X1&:(or (eq (class ?X1) Content) (subclassp (class ?X1) Content))) (predicate ?Y28&:(or (eq (class ?Y28) Has) (subclassp (class ?Y28) Has))&:(eq (send ?Y28 get-what) [private])) (time ?X3&:(or (eq (class ?X3) Duration) (subclassp (class ?X3) Duration))) (truth 1))) (logical (object (is-a Proposition) (subject ?X2&:(or (eq (class ?X2) Person) (subclassp (class ?X2) Person))) (predicate ?Y30&:(or (eq (class ?Y30) IsOwner) (subclassp (class ?Y30) IsOwner))&:(eq (send ?Y30 get-of) ?X1)) (time ?X4&:(or (eq (class ?X4) Duration) (subclassp (class ?X4) Duration))) (truth 1))) 
                (test (or (and (> (mincomstart ?X3 ?X4) -1)
                               (<= (mincomstart ?X3 ?X4) (maxcomend ?X3 ?X4)))
                          (= (maxcomend ?X3 ?X4) -1))
                )
                 => (add-prop ?X2 (add-pred Can what (add-pred View what ?X1)) (make-instance of Duration (start (mincomstart ?X3 ?X4)) (end (maxcomend ?X3 ?X4))) 1))
(defrule e6565554141e4e58ad327be8e61c38b8 (logical (object (is-a Proposition) (subject ?X1&:(or (eq (class ?X1) Person) (subclassp (class ?X1) Person))) (predicate ?Y32&:(or (eq (class ?Y32) Publish) (subclassp (class ?Y32) Publish))&:(or (eq (class (send ?Y32 get-what)) Content) (subclassp (class (send ?Y32 get-what)) Content))) (time ?X3) (truth 1))) (logical (object (is-a Proposition) (subject ?X2&:(eq ?X2 (send ?Y32 get-what))) (predicate ?Y33&:(or (eq (class ?Y33) Has) (subclassp (class ?Y33) Has))&:(or (eq (class (send ?Y33 get-what)) Status) (subclassp (class (send ?Y33 get-what)) Status))) (time ?X5&:(or (eq (class ?X5) Duration) (subclassp (class ?X5) Duration))) (truth 1))) => (send ?X5 put-end 733701) (add-prop (send ?Y32 get-what) (add-pred Has what [public]) (make-instance of Duration (start ?X3) (end -1.0)) 1))
(defrule 8ae67cd4a1f84b189c17ce33ebb987f7 (logical (object (is-a Content) (name ?X1))) => (add-prop [manage_perm] (add-pred IsNeeded for_action (add-pred Publish what ?X1)) (make-instance of Duration (start 733701.0) (end -1.0)) 1))
(defrule 79712ad66f714aa8a2823abe089e7863 (logical (object (is-a Proposition) (subject ?X1&:(or (eq (class ?X1) Person) (subclassp (class ?X1) Person))) (predicate ?Y35&:(or (eq (class ?Y35) Hide) (subclassp (class ?Y35) Hide))&:(or (eq (class (send ?Y35 get-what)) Content) (subclassp (class (send ?Y35 get-what)) Content))) (time ?X3) (truth 1))) (logical (object (is-a Proposition) (subject ?X2&:(eq ?X2 (send ?Y35 get-what))) (predicate ?Y36&:(or (eq (class ?Y36) Has) (subclassp (class ?Y36) Has))&:(or (eq (class (send ?Y36 get-what)) Status) (subclassp (class (send ?Y36 get-what)) Status))) (time ?X5&:(or (eq (class ?X5) Duration) (subclassp (class ?X5) Duration))) (truth 1))) => (send ?X5 put-end 733701) (add-prop (send ?Y35 get-what) (add-pred Has what [private]) (make-instance of Duration (start ?X3) (end -1.0)) 1))
(defrule e238b815a2b04ec3959e76714d80581e (logical (object (is-a Proposition) (subject ?X1&:(or (eq (class ?X1) Person) (subclassp (class ?X1) Person))) (predicate ?Y38&:(or (eq (class ?Y38) IsOwner) (subclassp (class ?Y38) IsOwner))&:(or (eq (class (send ?Y38 get-of)) Content) (subclassp (class (send ?Y38 get-of)) Content))) (time ?X3&:(or (eq (class ?X3) Duration) (subclassp (class ?X3) Duration))) (truth 1))) => (add-prop ?X1 (add-pred Can what (add-pred Hide what (send ?Y38 get-of))) ?X3 1))
(reduce-class [john] Person)
(reduce-class [pete] Person)
(reduce-class [jane] Person)
(reduce-class [c1] Content)
(reduce-class [c2] Content)
(add-prop [john] (add-pred Has what [manager]) (make-instance of Duration (start 733701.0) (end -1.0)) 1)
(add-prop [jane] (add-pred Has what [create_perm]) (make-instance of Duration (start 733701.0) (end -1.0)) 1)
(add-prop [jane] (add-pred Wants to (add-pred Create what [c1])) 733701.0 1)
(add-prop [pete] (add-pred Wants to (add-pred Create what [c2])) 733701.0 1)
(find-all-instances ((?prop Proposition) (?Y40 IsOwner) (?Y41 Duration)) (and (eq ?prop:subject [jane]) (eq ?Y40:of [c1]) (eq ?prop:predicate ?Y40) (= ?Y41:start 733701.0) (= ?Y41:end -1.0) (eq ?prop:truth 1)))
0


no
----------running---------------------
----------runned: 33---------------------
(find-all-instances ((?prop Proposition) (?Y42 IsOwner) (?Y43 Duration)) (and (eq ?prop:subject [jane]) (eq ?Y42:of [c1]) (eq ?prop:predicate ?Y42) (= ?Y43:start 733701.0) (= ?Y43:end -1.0) (eq ?prop:truth 1)))
35


jane isowner of c1 at from 733701.0 till -1.0
(find-all-instances ((?prop Proposition) (?Y44 Has) (?Y45 Duration)) (and (eq ?prop:subject [c1]) (eq ?Y44:what [private]) (eq ?prop:predicate ?Y44) (= ?Y45:start 733701.0) (= ?Y45:end -1.0) (eq ?prop:truth 1)))
35


c1 has what private at from 733701.0 till -1.0
(find-all-instances ((?prop Proposition) (?Y46 IsOwner) (?Y47 Duration)) (and (eq ?prop:subject [pete]) (eq ?Y46:of [c2]) (eq ?prop:predicate ?Y46) (= ?Y47:start 733701.0) (= ?Y47:end -1.0) (eq ?prop:truth 1)))
0


no
(add-prop [jane] (add-pred Wants to (add-pred Publish what [c1])) 733701.0 1)
(add-prop [pete] (add-pred Wants to (add-pred Publish what [c2])) 733701.0 1)
----------running---------------------
----------runned: 0---------------------
(find-all-instances ((?prop Proposition) (?Y48 Has) (?Y49 Duration)) (and (eq ?prop:subject [c1]) (eq ?Y48:what [public]) (eq ?prop:predicate ?Y48) (= ?Y49:start 733701.0) (= ?Y49:end -1.0) (eq ?prop:truth 1)))
0


no
(find-all-instances ((?prop Proposition) (?Y50 Has) (?Y51 Duration)) (and (eq ?prop:subject [c2]) (eq ?Y50:what [public]) (eq ?prop:predicate ?Y50) (= ?Y51:start 733701.0) (= ?Y51:end -1.0) (eq ?prop:truth 1)))
0


no
(find-all-instances ((?prop Proposition) (?Y52 Can) (?Y53 View) (?Y54 Duration)) (and (eq ?prop:subject [jane]) (eq ?Y53:what [c1]) (eq ?Y52:what ?Y53) (eq ?prop:predicate ?Y52) (= ?Y54:start 733701.0) (= ?Y54:end -1.0) (eq ?prop:truth 1)))
35


jane can what view what c1 at from 733701.0 till -1.0
(find-all-instances ((?prop Proposition) (?Y55 Can) (?Y56 View) (?Y57 Duration)) (and (eq ?prop:subject [pete]) (eq ?Y56:what [c1]) (eq ?Y55:what ?Y56) (eq ?prop:predicate ?Y55) (= ?Y57:start 733701.0) (= ?Y57:end -1.0) (eq ?prop:truth 1)))
0


no
(add-prop [john] (add-pred Wants to (add-pred Publish what [c1])) 733701.0 1)
----------running---------------------
----------runned: 10---------------------
(find-all-instances ((?prop Proposition) (?Y58 Has) (?Y59 Duration)) (and (eq ?prop:subject [c1]) (eq ?Y58:what [private]) (eq ?prop:predicate ?Y58) (= ?Y59:start 733701.0) (= ?Y59:end -1.0) (eq ?prop:truth 1)))
0


no
(find-all-instances ((?prop Proposition) (?Y60 Has) (?Y61 Duration)) (and (eq ?prop:subject [c1]) (eq ?Y60:what [public]) (eq ?prop:predicate ?Y60) (= ?Y61:start 733701.0) (= ?Y61:end -1.0) (eq ?prop:truth 1)))
41


c1 has what public at from 733701.0 till -1.0
(find-all-instances ((?prop Proposition) (?Y62 Can) (?Y63 View) (?Y64 Duration)) (and (eq ?prop:subject [pete]) (eq ?Y63:what [c1]) (eq ?Y62:what ?Y63) (eq ?prop:predicate ?Y62) (= ?Y64:start 733701.0) (= ?Y64:end -1.0) (eq ?prop:truth 1)))
41


pete can what view what c1 at from 733701.0 till -1.0
(defclass Name (is-a USER))

(deffunction reduce-class (?instance ?class)
    (if (eq (length$
                (find-all-instances ((?a ?class))(eq (instance-name ?a) ?instance)))
             0)
    then (make-instance ?instance of ?class)
    else (return TRUE)))
(defclass Thing (is-a Name))
(defclass Verb (is-a USER))
(defclass State (is-a Verb) )
(set-sequence-operator-recognition TRUE)
(defmessage-handler State set-slots primary ($?slots)
        (while (> (length$ ?slots) 0) do
            (bind ?slot (first$ ?slots))
            (bind ?vslots (rest$ ?slots))
            (bind ?value (first$ ?vslots))
            (bind ?slots (rest$ ?vslots))
            (dynamic-put $?slot $?value))
        (return (instance-name ?self)))


(deffunction add-pred (?class $?slots)
        (bind ?key (str-cat ?class $?slots))
        (bind ?pos (str-index "." ?key))
        (while ?pos do
            (bind ?key (str-cat (sub-string 1 (- ?pos 1) ?key)
                                "_"
                                (sub-string (+ ?pos 1) (str-length ?key) ?key)))
            (bind ?pos (str-index "." ?key)))
        (bind ?key (sym-cat ?key))
        (if (instance-existp ?key) then
            (return (instance-name ?key))
         else
            (make-instance ?key of ?class)
            (return (send (instance-name ?key) set-slots $?slots))))

(defclass Duration (is-a Name) (slot start (type NUMBER) (pattern-match reactive)) (slot end (type NUMBER) (pattern-match reactive)))

(deffunction extract-now ($?instants)
    (if (= (length$ ?instants) 0) then
        (return -1)
    else
        (bind ?count 0)
        (bind ?not-now (create$))
        (while (> (length$ ?instants) 0) do
            (bind ?instant (nth$ 1 ?instants))
            (bind ?instants (rest$ ?instants))
            (if (= ?instant -1)
            then (bind ?count (+ ?count 1))
            else (bind ?not-now (insert$ ?not-now 1 ?instant)))
            )
        (return (insert$ ?not-now 1 ?count))
    )
)


(deffunction maxcomend ($?durations)
    (bind ?ends (create$))
    (while (> (length$ ?durations) 0)
        (bind ?ends (insert$ ?ends 1 (send (nth$ 1 ?durations) get-end)))
        (bind ?durations (rest$ ?durations))
        )
    (bind ?ret (extract-now ?ends))
    (bind ?pasts (rest$ ?ret))
    (if (> (length$ ?pasts) 0)
    then (return (min (expand$ ?pasts)))
    else (return -1)
    )
)


(deffunction mincomstart ($?durations)
    (bind ?starts (create$))
    (while (> (length$ ?durations) 0)
        (bind ?starts (insert$ ?starts 1 (send (nth$ 1 ?durations) get-start)))
        (bind ?durations (rest$ ?durations))
        )
    (bind ?ret (extract-now ?starts))
    (bind ?nows (nth$ 1 ?ret))
    (if (> ?nows 0)
    then (return -1)
    else (bind ?ret (rest$ ?ret))
         (return (max (expand$ ?ret)))
    )
)

(defclass Proposition (is-a Name) (slot truth (type INTEGER) (default 1) (pattern-match reactive)) (slot subject (type INSTANCE) (pattern-match reactive)) (slot predicate (type INSTANCE) (pattern-match reactive)) (slot time (type ?VARIABLE) (pattern-match reactive)))

(deffunction add-prop (?s ?p ?t ?r)
       (bind ?count 0)
       (do-for-all-instances ((?prop Proposition))
                          (and (eq ?prop:subject ?s)
                               (eq ?prop:predicate ?p)
                               (or (and (eq (class ?t) Duration)
                                        (= (send (send ?prop get-time) get-start) (send ?t get-start))
                                        (= (send (send ?prop get-time) get-end) (send ?t get-end)))
                                   (= ?prop:time ?t))
                               (= ?prop:truth ?r))
               (bind ?count (+ ?count 1)))
        (if (= ?count 0)
        then (make-instance of Proposition (subject ?s)
                                           (predicate ?p)
                                           (time ?t)
                                           (truth ?r))
        else (return TRUE)))
(defclass Person (is-a Thing))
(defclass Can (is-a State) (slot what (type INSTANCE) (visibility public) (pattern-match reactive)))
(defclass Wants (is-a State) (slot to (type INSTANCE) (visibility public) (pattern-match reactive)))
(defclass Has (is-a State) (slot what (type INSTANCE) (visibility public) (pattern-match reactive)))
(defclass IsNeeded (is-a State) (slot for_action (type INSTANCE) (visibility public) (pattern-match reactive)))
(defclass IsIn (is-a State) (slot what (type INSTANCE) (visibility public) (pattern-match reactive)))
(defclass Group (is-a Thing))
(defclass Permission (is-a Thing))
(defclass Role (is-a Thing))
(defclass Content (is-a Thing))
(defclass Create (is-a State) (slot what (type INSTANCE) (visibility public) (pattern-match reactive)))
(defclass IsOwner (is-a State) (slot of (type INSTANCE) (visibility public) (pattern-match reactive)))
(defclass Status (is-a Thing))
(defclass View (is-a State) (slot what (type INSTANCE) (visibility public) (pattern-match reactive)))
(defclass Publish (is-a State) (slot what (type INSTANCE) (visibility public) (pattern-match reactive)))
(defclass Hide (is-a State) (slot what (type INSTANCE) (visibility public) (pattern-match reactive)))
(reduce-class [admin] Person)
(reduce-class [member] Role)
(reduce-class [manager] Role)
(reduce-class [basic_perm] Permission)
(reduce-class [manage_perm] Permission)
(reduce-class [create_perm] Permission)
(reduce-class [public] Status)
(reduce-class [private] Status)
(add-prop [admin] (add-pred Has what [manager]) (make-instance of Duration (start 733701.0) (end -1.0)) 1)
(add-prop [member] (add-pred Has what [basic_perm]) (make-instance of Duration (start 733701.0) (end -1.0)) 1)
(defrule c6e0042ccd804b46b972d28f8978416a (logical (object (is-a Proposition) (subject ?X1&:(or (eq (class ?X1) Person) (subclassp (class ?X1) Person))) (predicate ?Y1&:(or (eq (class ?Y1) Wants) (subclassp (class ?Y1) Wants))&:(or (eq (class (send ?Y1 get-to)) Create) (subclassp (class (send ?Y1 get-to)) Create))&:(or (eq (class (send (send ?Y1 get-to) get-what)) Thing) (subclassp (class (send (send ?Y1 get-to) get-what)) Thing))) (time ?X2) (truth 1))) (logical (object (is-a Proposition) (subject ?X1) (predicate ?Y2&:(or (eq (class ?Y2) Has) (subclassp (class ?Y2) Has))&:(eq (send ?Y2 get-what) [create_perm])) (time ?X3&:(or (eq (class ?X3) Duration) (subclassp (class ?X3) Duration))) (truth 1))) (test (and (<= (send ?X3 get-start) ?X2) (or (= (send ?X3 get-end) -1) (>= (send ?X3 get-end) ?X2)))) => (add-prop ?X1 (add-pred Create what (send (send ?Y1 get-to) get-what)) ?X2 1))
(defrule 56c6de81206346a49cbc08c6340191ef (logical (object (is-a Proposition) (subject ?X1&:(or (eq (class ?X1) Person) (subclassp (class ?X1) Person))) (predicate ?Y4&:(or (eq (class ?Y4) Wants) (subclassp (class ?Y4) Wants))) (time ?X2) (truth 1))) (logical (object (is-a Proposition) (subject ?X1) (predicate ?Y5&:(or (eq (class ?Y5) Can) (subclassp (class ?Y5) Can))&:(eq (send ?Y4 get-to) (send ?Y5 get-what))) (time ?X3&:(or (eq (class ?X3) Duration) (subclassp (class ?X3) Duration))) (truth 1))) (test (and (<= (send ?X3 get-start) ?X2) (or (= (send ?X3 get-end) -1) (>= (send ?X3 get-end) ?X2)))) => (add-prop ?X1 (send ?Y4 get-to) ?X2 1))
(defrule 7c59872015424ce6bfdd4ede7eaceb2a (logical (object (is-a Proposition) (subject ?X2&:(or (eq (class ?X2) Thing) (subclassp (class ?X2) Thing))) (predicate ?Y7&:(or (eq (class ?Y7) IsNeeded) (subclassp (class ?Y7) IsNeeded))) (time ?X3&:(or (eq (class ?X3) Duration) (subclassp (class ?X3) Duration))) (truth 1))) (logical (object (is-a Proposition) (subject ?X1&:(or (eq (class ?X1) Thing) (subclassp (class ?X1) Thing))) (predicate ?Y9&:(or (eq (class ?Y9) Has) (subclassp (class ?Y9) Has))&:(eq (send ?Y9 get-what) ?X2)) (time ?X5&:(or (eq (class ?X5) Duration) (subclassp (class ?X5) Duration))) (truth 1))) 
                (test (or (and (> (mincomstart ?X3 ?X5) -1)
                               (<= (mincomstart ?X3 ?X5) (maxcomend ?X3 ?X5)))
                          (= (maxcomend ?X3 ?X5) -1))
                )
                 => (add-prop ?X1 (add-pred Can what (send ?Y7 get-for_action)) (make-instance of Duration (start (mincomstart ?X3 ?X5)) (end (maxcomend ?X3 ?X5))) 1))
(defrule f9c1584ecf9641d68d14f21bb93f58ae (logical (object (is-a Proposition) (subject ?X1&:(or (eq (class ?X1) Thing) (subclassp (class ?X1) Thing))) (predicate ?Y11&:(or (eq (class ?Y11) IsIn) (subclassp (class ?Y11) IsIn))&:(or (eq (class (send ?Y11 get-what)) Thing) (subclassp (class (send ?Y11 get-what)) Thing))) (time ?X4&:(or (eq (class ?X4) Duration) (subclassp (class ?X4) Duration))) (truth 1))) (logical (object (is-a Proposition) (subject ?X2&:(eq ?X2 (send ?Y11 get-what))) (predicate ?Y13&:(or (eq (class ?Y13) IsIn) (subclassp (class ?Y13) IsIn))&:(or (eq (class (send ?Y13 get-what)) Thing) (subclassp (class (send ?Y13 get-what)) Thing))) (time ?X5&:(or (eq (class ?X5) Duration) (subclassp (class ?X5) Duration))) (truth 1))) 
                (test (or (and (> (mincomstart ?X4 ?X5) -1)
                               (<= (mincomstart ?X4 ?X5) (maxcomend ?X4 ?X5)))
                          (= (maxcomend ?X4 ?X5) -1))
                )
                 => (add-prop ?X1 (add-pred IsIn what (send ?Y13 get-what)) (make-instance of Duration (start (mincomstart ?X4 ?X5)) (end (maxcomend ?X4 ?X5))) 1))
(defrule c85bc7aa068940cd9968937e1870f881 (logical (object (is-a Proposition) (subject ?X1&:(or (eq (class ?X1) Person) (subclassp (class ?X1) Person))) (predicate ?Y15&:(or (eq (class ?Y15) IsIn) (subclassp (class ?Y15) IsIn))) (time ?X3&:(or (eq (class ?X3) Duration) (subclassp (class ?X3) Duration))) (truth 1))) (logical (object (is-a Proposition) (subject ?X2&:(or (eq (class ?X2) Group) (subclassp (class ?X2) Group))) (predicate ?Y17&:(or (eq (class ?Y17) Has) (subclassp (class ?Y17) Has))&:(or (eq (class (send ?Y17 get-what)) Permission) (subclassp (class (send ?Y17 get-what)) Permission))) (time ?X5&:(or (eq (class ?X5) Duration) (subclassp (class ?X5) Duration))) (truth 1))) 
                (test (or (and (> (mincomstart ?X3 ?X5) -1)
                               (<= (mincomstart ?X3 ?X5) (maxcomend ?X3 ?X5)))
                          (= (maxcomend ?X3 ?X5) -1))
                )
                 => (add-prop ?X1 (add-pred Has what (send ?Y17 get-what)) (make-instance of Duration (start (mincomstart ?X3 ?X5)) (end (maxcomend ?X3 ?X5))) 1))
(defrule 5211dbe542b244fa9e6f18f0c1bc0838 (logical (object (is-a Proposition) (subject ?X1&:(or (eq (class ?X1) Person) (subclassp (class ?X1) Person))) (predicate ?Y19&:(or (eq (class ?Y19) Has) (subclassp (class ?Y19) Has))&:(or (eq (class (send ?Y19 get-what)) Role) (subclassp (class (send ?Y19 get-what)) Role))) (time ?X3&:(or (eq (class ?X3) Duration) (subclassp (class ?X3) Duration))) (truth 1))) (logical (object (is-a Proposition) (subject ?X2&:(eq ?X2 (send ?Y19 get-what))) (predicate ?Y21&:(or (eq (class ?Y21) Has) (subclassp (class ?Y21) Has))&:(or (eq (class (send ?Y21 get-what)) Permission) (subclassp (class (send ?Y21 get-what)) Permission))) (time ?X5&:(or (eq (class ?X5) Duration) (subclassp (class ?X5) Duration))) (truth 1))) 
                (test (or (and (> (mincomstart ?X3 ?X5) -1)
                               (<= (mincomstart ?X3 ?X5) (maxcomend ?X3 ?X5)))
                          (= (maxcomend ?X3 ?X5) -1))
                )
                 => (add-prop ?X1 (add-pred Has what (send ?Y21 get-what)) (make-instance of Duration (start (mincomstart ?X3 ?X5)) (end (maxcomend ?X3 ?X5))) 1))
(defrule 309034232c224233b77d5521a5935540 (logical (object (is-a Person) (name ?X1))) => (add-prop ?X1 (add-pred Has what [member]) (make-instance of Duration (start 733701.0) (end -1.0)) 1))
(defrule b77be0a7008048deb197c4afd22fadf2 (logical (object (is-a Proposition) (subject ?X1&:(or (eq (class ?X1) Person) (subclassp (class ?X1) Person))) (predicate ?Y23&:(or (eq (class ?Y23) Create) (subclassp (class ?Y23) Create))&:(or (eq (class (send ?Y23 get-what)) Content) (subclassp (class (send ?Y23 get-what)) Content))) (time ?X3) (truth 1))) => (reduce-class (send ?Y23 get-what) Content) (add-prop ?X1 (add-pred IsOwner of (send ?Y23 get-what)) (make-instance of Duration (start ?X3) (end -1.0)) 1) (add-prop (send ?Y23 get-what) (add-pred Has what [private]) (make-instance of Duration (start ?X3) (end -1.0)) 1))
(defrule 4b82661b8f89487fbe11f695dfd49e8c (logical (object (is-a Permission) (name ?X2))) => (add-prop [manager] (add-pred Has what ?X2) (make-instance of Duration (start 733701.0) (end -1.0)) 1))
(defrule fec2ab95b0ea440a8a2bc110dfc42ce6 (logical (object (is-a Proposition) (subject ?X1&:(or (eq (class ?X1) Content) (subclassp (class ?X1) Content))) (predicate ?Y24&:(or (eq (class ?Y24) Has) (subclassp (class ?Y24) Has))&:(eq (send ?Y24 get-what) [public])) (time ?X2&:(or (eq (class ?X2) Duration) (subclassp (class ?X2) Duration))) (truth 1))) => (add-prop [basic_perm] (add-pred IsNeeded for_action (add-pred View what ?X1)) ?X2 1))
(defrule c3a538ffa84240d88263a2a62a862a73 (logical (object (is-a Proposition) (subject ?X1&:(or (eq (class ?X1) Content) (subclassp (class ?X1) Content))) (predicate ?Y26&:(or (eq (class ?Y26) Has) (subclassp (class ?Y26) Has))&:(eq (send ?Y26 get-what) [private])) (time ?X2&:(or (eq (class ?X2) Duration) (subclassp (class ?X2) Duration))) (truth 1))) => (add-prop [manage_perm] (add-pred IsNeeded for_action (add-pred View what ?X1)) ?X2 1))
(defrule f6d839f17d0c4e3fb0ad2057ab30919e (logical (object (is-a Proposition) (subject ?X1&:(or (eq (class ?X1) Content) (subclassp (class ?X1) Content))) (predicate ?Y28&:(or (eq (class ?Y28) Has) (subclassp (class ?Y28) Has))&:(eq (send ?Y28 get-what) [private])) (time ?X3&:(or (eq (class ?X3) Duration) (subclassp (class ?X3) Duration))) (truth 1))) (logical (object (is-a Proposition) (subject ?X2&:(or (eq (class ?X2) Person) (subclassp (class ?X2) Person))) (predicate ?Y30&:(or (eq (class ?Y30) IsOwner) (subclassp (class ?Y30) IsOwner))&:(eq (send ?Y30 get-of) ?X1)) (time ?X4&:(or (eq (class ?X4) Duration) (subclassp (class ?X4) Duration))) (truth 1))) 
                (test (or (and (> (mincomstart ?X3 ?X4) -1)
                               (<= (mincomstart ?X3 ?X4) (maxcomend ?X3 ?X4)))
                          (= (maxcomend ?X3 ?X4) -1))
                )
                 => (add-prop ?X2 (add-pred Can what (add-pred View what ?X1)) (make-instance of Duration (start (mincomstart ?X3 ?X4)) (end (maxcomend ?X3 ?X4))) 1))
(defrule 4920d4434ccc431f9597d3546e355446 (logical (object (is-a Proposition) (subject ?X1&:(or (eq (class ?X1) Person) (subclassp (class ?X1) Person))) (predicate ?Y32&:(or (eq (class ?Y32) Publish) (subclassp (class ?Y32) Publish))&:(or (eq (class (send ?Y32 get-what)) Content) (subclassp (class (send ?Y32 get-what)) Content))) (time ?X3) (truth 1))) (logical (object (is-a Proposition) (subject ?X2&:(eq ?X2 (send ?Y32 get-what))) (predicate ?Y33&:(or (eq (class ?Y33) Has) (subclassp (class ?Y33) Has))&:(or (eq (class (send ?Y33 get-what)) Status) (subclassp (class (send ?Y33 get-what)) Status))) (time ?X5&:(or (eq (class ?X5) Duration) (subclassp (class ?X5) Duration))) (truth 1))) => (send ?X5 put-end 733701) (add-prop (send ?Y32 get-what) (add-pred Has what [public]) (make-instance of Duration (start ?X3) (end -1.0)) 1))
(defrule a68eba7f079645cca9b5d3bc8455893f (logical (object (is-a Content) (name ?X1))) => (add-prop [manage_perm] (add-pred IsNeeded for_action (add-pred Publish what ?X1)) (make-instance of Duration (start 733701.0) (end -1.0)) 1))
(defrule 56d35b77d4424a839acaa722d0156d48 (logical (object (is-a Proposition) (subject ?X1&:(or (eq (class ?X1) Person) (subclassp (class ?X1) Person))) (predicate ?Y35&:(or (eq (class ?Y35) Hide) (subclassp (class ?Y35) Hide))&:(or (eq (class (send ?Y35 get-what)) Content) (subclassp (class (send ?Y35 get-what)) Content))) (time ?X3) (truth 1))) (logical (object (is-a Proposition) (subject ?X2&:(eq ?X2 (send ?Y35 get-what))) (predicate ?Y36&:(or (eq (class ?Y36) Has) (subclassp (class ?Y36) Has))&:(or (eq (class (send ?Y36 get-what)) Status) (subclassp (class (send ?Y36 get-what)) Status))) (time ?X5&:(or (eq (class ?X5) Duration) (subclassp (class ?X5) Duration))) (truth 1))) => (send ?X5 put-end 733701) (add-prop (send ?Y35 get-what) (add-pred Has what [private]) (make-instance of Duration (start ?X3) (end -1.0)) 1))
(defrule 0a43c0696762401da09ee1e74ba378bd (logical (object (is-a Proposition) (subject ?X1&:(or (eq (class ?X1) Person) (subclassp (class ?X1) Person))) (predicate ?Y38&:(or (eq (class ?Y38) IsOwner) (subclassp (class ?Y38) IsOwner))&:(or (eq (class (send ?Y38 get-of)) Content) (subclassp (class (send ?Y38 get-of)) Content))) (time ?X3&:(or (eq (class ?X3) Duration) (subclassp (class ?X3) Duration))) (truth 1))) => (add-prop ?X1 (add-pred Can what (add-pred Hide what (send ?Y38 get-of))) ?X3 1))
(reduce-class [john] Person)
(reduce-class [pete] Person)
(reduce-class [jane] Person)
(reduce-class [c1] Content)
(reduce-class [c2] Content)
(add-prop [john] (add-pred Has what [manager]) (make-instance of Duration (start 733701.0) (end -1.0)) 1)
(add-prop [jane] (add-pred Has what [create_perm]) (make-instance of Duration (start 733701.0) (end -1.0)) 1)
(add-prop [jane] (add-pred Wants to (add-pred Create what [c1])) 733701.0 1)
(add-prop [pete] (add-pred Wants to (add-pred Create what [c2])) 733701.0 1)
(find-all-instances ((?prop Proposition) (?Y40 IsOwner) (?Y41 Duration)) (and (eq ?prop:subject [jane]) (eq ?Y40:of [c1]) (eq ?prop:predicate ?Y40) (= ?Y41:start 733701.0) (= ?Y41:end -1.0) (eq ?prop:truth 1)))
0


no
----------running---------------------
----------runned: 33---------------------
(find-all-instances ((?prop Proposition) (?Y42 IsOwner) (?Y43 Duration)) (and (eq ?prop:subject [jane]) (eq ?Y42:of [c1]) (eq ?prop:predicate ?Y42) (= ?Y43:start 733701.0) (= ?Y43:end -1.0) (eq ?prop:truth 1)))
35


jane isowner of c1 at from 733701.0 till -1.0
(find-all-instances ((?prop Proposition) (?Y44 Has) (?Y45 Duration)) (and (eq ?prop:subject [c1]) (eq ?Y44:what [private]) (eq ?prop:predicate ?Y44) (= ?Y45:start 733701.0) (= ?Y45:end -1.0) (eq ?prop:truth 1)))
35


c1 has what private at from 733701.0 till -1.0
(find-all-instances ((?prop Proposition) (?Y46 IsOwner) (?Y47 Duration)) (and (eq ?prop:subject [pete]) (eq ?Y46:of [c2]) (eq ?prop:predicate ?Y46) (= ?Y47:start 733701.0) (= ?Y47:end -1.0) (eq ?prop:truth 1)))
0


no
(add-prop [jane] (add-pred Wants to (add-pred Publish what [c1])) 733701.0 1)
(add-prop [pete] (add-pred Wants to (add-pred Publish what [c2])) 733701.0 1)
----------running---------------------
----------runned: 0---------------------
(find-all-instances ((?prop Proposition) (?Y48 Has) (?Y49 Duration)) (and (eq ?prop:subject [c1]) (eq ?Y48:what [public]) (eq ?prop:predicate ?Y48) (= ?Y49:start 733701.0) (= ?Y49:end -1.0) (eq ?prop:truth 1)))
0


no
(find-all-instances ((?prop Proposition) (?Y50 Has) (?Y51 Duration)) (and (eq ?prop:subject [c2]) (eq ?Y50:what [public]) (eq ?prop:predicate ?Y50) (= ?Y51:start 733701.0) (= ?Y51:end -1.0) (eq ?prop:truth 1)))
0


no
(find-all-instances ((?prop Proposition) (?Y52 Can) (?Y53 View) (?Y54 Duration)) (and (eq ?prop:subject [jane]) (eq ?Y53:what [c1]) (eq ?Y52:what ?Y53) (eq ?prop:predicate ?Y52) (= ?Y54:start 733701.0) (= ?Y54:end -1.0) (eq ?prop:truth 1)))
35


jane can what view what c1 at from 733701.0 till -1.0
(find-all-instances ((?prop Proposition) (?Y55 Can) (?Y56 View) (?Y57 Duration)) (and (eq ?prop:subject [pete]) (eq ?Y56:what [c1]) (eq ?Y55:what ?Y56) (eq ?prop:predicate ?Y55) (= ?Y57:start 733701.0) (= ?Y57:end -1.0) (eq ?prop:truth 1)))
0


no
(add-prop [john] (add-pred Wants to (add-pred Publish what [c1])) 733701.0 1)
----------running---------------------
----------runned: 10---------------------
(find-all-instances ((?prop Proposition) (?Y58 Has) (?Y59 Duration)) (and (eq ?prop:subject [c1]) (eq ?Y58:what [private]) (eq ?prop:predicate ?Y58) (= ?Y59:start 733701.0) (= ?Y59:end -1.0) (eq ?prop:truth 1)))
0


no
(find-all-instances ((?prop Proposition) (?Y60 Has) (?Y61 Duration)) (and (eq ?prop:subject [c1]) (eq ?Y60:what [public]) (eq ?prop:predicate ?Y60) (= ?Y61:start 733701.0) (= ?Y61:end -1.0) (eq ?prop:truth 1)))
41


c1 has what public at from 733701.0 till -1.0
(find-all-instances ((?prop Proposition) (?Y62 Can) (?Y63 View) (?Y64 Duration)) (and (eq ?prop:subject [pete]) (eq ?Y63:what [c1]) (eq ?Y62:what ?Y63) (eq ?prop:predicate ?Y62) (= ?Y64:start 733701.0) (= ?Y64:end -1.0) (eq ?prop:truth 1)))
41


pete can what view what c1 at from 733701.0 till -1.0
(defclass Name (is-a USER))

(deffunction reduce-class (?instance ?class)
    (if (eq (length$
                (find-all-instances ((?a ?class))(eq (instance-name ?a) ?instance)))
             0)
    then (make-instance ?instance of ?class)
    else (return TRUE)))
(defclass Thing (is-a Name))
(defclass Verb (is-a USER))
(defclass State (is-a Verb) )
(set-sequence-operator-recognition TRUE)
(defmessage-handler State set-slots primary ($?slots)
        (while (> (length$ ?slots) 0) do
            (bind ?slot (first$ ?slots))
            (bind ?vslots (rest$ ?slots))
            (bind ?value (first$ ?vslots))
            (bind ?slots (rest$ ?vslots))
            (dynamic-put $?slot $?value))
        (return (instance-name ?self)))


(deffunction add-pred (?class $?slots)
        (bind ?key (str-cat ?class $?slots))
        (bind ?pos (str-index "." ?key))
        (while ?pos do
            (bind ?key (str-cat (sub-string 1 (- ?pos 1) ?key)
                                "_"
                                (sub-string (+ ?pos 1) (str-length ?key) ?key)))
            (bind ?pos (str-index "." ?key)))
        (bind ?key (sym-cat ?key))
        (if (instance-existp ?key) then
            (return (instance-name ?key))
         else
            (make-instance ?key of ?class)
            (return (send (instance-name ?key) set-slots $?slots))))

(defclass Duration (is-a Name) (slot start (type NUMBER) (pattern-match reactive)) (slot end (type NUMBER) (pattern-match reactive)))

(deffunction extract-now ($?instants)
    (if (= (length$ ?instants) 0) then
        (return -1)
    else
        (bind ?count 0)
        (bind ?not-now (create$))
        (while (> (length$ ?instants) 0) do
            (bind ?instant (nth$ 1 ?instants))
            (bind ?instants (rest$ ?instants))
            (if (= ?instant -1)
            then (bind ?count (+ ?count 1))
            else (bind ?not-now (insert$ ?not-now 1 ?instant)))
            )
        (return (insert$ ?not-now 1 ?count))
    )
)


(deffunction maxcomend ($?durations)
    (bind ?ends (create$))
    (while (> (length$ ?durations) 0)
        (bind ?ends (insert$ ?ends 1 (send (nth$ 1 ?durations) get-end)))
        (bind ?durations (rest$ ?durations))
        )
    (bind ?ret (extract-now ?ends))
    (bind ?pasts (rest$ ?ret))
    (if (> (length$ ?pasts) 0)
    then (return (min (expand$ ?pasts)))
    else (return -1)
    )
)


(deffunction mincomstart ($?durations)
    (bind ?starts (create$))
    (while (> (length$ ?durations) 0)
        (bind ?starts (insert$ ?starts 1 (send (nth$ 1 ?durations) get-start)))
        (bind ?durations (rest$ ?durations))
        )
    (bind ?ret (extract-now ?starts))
    (bind ?nows (nth$ 1 ?ret))
    (if (> ?nows 0)
    then (return -1)
    else (bind ?ret (rest$ ?ret))
         (return (max (expand$ ?ret)))
    )
)

(defclass Proposition (is-a Name) (slot truth (type INTEGER) (default 1) (pattern-match reactive)) (slot subject (type INSTANCE) (pattern-match reactive)) (slot predicate (type INSTANCE) (pattern-match reactive)) (slot time (type ?VARIABLE) (pattern-match reactive)))

(deffunction add-prop (?s ?p ?t ?r)
       (bind ?count 0)
       (do-for-all-instances ((?prop Proposition))
                          (and (eq ?prop:subject ?s)
                               (eq ?prop:predicate ?p)
                               (or (and (eq (class ?t) Duration)
                                        (= (send (send ?prop get-time) get-start) (send ?t get-start))
                                        (= (send (send ?prop get-time) get-end) (send ?t get-end)))
                                   (= ?prop:time ?t))
                               (= ?prop:truth ?r))
               (bind ?count (+ ?count 1)))
        (if (= ?count 0)
        then (make-instance of Proposition (subject ?s)
                                           (predicate ?p)
                                           (time ?t)
                                           (truth ?r))
        else (return TRUE)))
(defclass Person (is-a Thing))
(defclass Can (is-a State) (slot what (type INSTANCE) (visibility public) (pattern-match reactive)))
(defclass Wants (is-a State) (slot to (type INSTANCE) (visibility public) (pattern-match reactive)))
(defclass Has (is-a State) (slot what (type INSTANCE) (visibility public) (pattern-match reactive)))
(defclass IsNeeded (is-a State) (slot for_action (type INSTANCE) (visibility public) (pattern-match reactive)))
(defclass IsIn (is-a State) (slot what (type INSTANCE) (visibility public) (pattern-match reactive)))
(defclass Group (is-a Thing))
(defclass Permission (is-a Thing))
(defclass Role (is-a Thing))
(defclass Content (is-a Thing))
(defclass Create (is-a State) (slot what (type INSTANCE) (visibility public) (pattern-match reactive)))
(defclass IsOwner (is-a State) (slot of (type INSTANCE) (visibility public) (pattern-match reactive)))
(defclass Status (is-a Thing))
(defclass View (is-a State) (slot what (type INSTANCE) (visibility public) (pattern-match reactive)))
(defclass Publish (is-a State) (slot what (type INSTANCE) (visibility public) (pattern-match reactive)))
(defclass Hide (is-a State) (slot what (type INSTANCE) (visibility public) (pattern-match reactive)))
(reduce-class [admin] Person)
(reduce-class [member] Role)
(reduce-class [manager] Role)
(reduce-class [basic_perm] Permission)
(reduce-class [manage_perm] Permission)
(reduce-class [create_perm] Permission)
(reduce-class [public] Status)
(reduce-class [private] Status)
(add-prop [admin] (add-pred Has what [manager]) (make-instance of Duration (start 733701.0) (end -1.0)) 1)
(add-prop [member] (add-pred Has what [basic_perm]) (make-instance of Duration (start 733701.0) (end -1.0)) 1)
(defrule 14f56434fff74e7084deb247da157b01 (logical (object (is-a Proposition) (subject ?X1&:(or (eq (class ?X1) Person) (subclassp (class ?X1) Person))) (predicate ?Y1&:(or (eq (class ?Y1) Wants) (subclassp (class ?Y1) Wants))&:(or (eq (class (send ?Y1 get-to)) Create) (subclassp (class (send ?Y1 get-to)) Create))&:(or (eq (class (send (send ?Y1 get-to) get-what)) Thing) (subclassp (class (send (send ?Y1 get-to) get-what)) Thing))) (time ?X2) (truth 1))) (logical (object (is-a Proposition) (subject ?X1) (predicate ?Y2&:(or (eq (class ?Y2) Has) (subclassp (class ?Y2) Has))&:(eq (send ?Y2 get-what) [create_perm])) (time ?X3&:(or (eq (class ?X3) Duration) (subclassp (class ?X3) Duration))) (truth 1))) (test (and (<= (send ?X3 get-start) ?X2) (or (= (send ?X3 get-end) -1) (>= (send ?X3 get-end) ?X2)))) => (add-prop ?X1 (add-pred Create what (send (send ?Y1 get-to) get-what)) ?X2 1))
(defrule 40256fc0ee5e4757a183addfb9746461 (logical (object (is-a Proposition) (subject ?X1&:(or (eq (class ?X1) Person) (subclassp (class ?X1) Person))) (predicate ?Y4&:(or (eq (class ?Y4) Wants) (subclassp (class ?Y4) Wants))) (time ?X2) (truth 1))) (logical (object (is-a Proposition) (subject ?X1) (predicate ?Y5&:(or (eq (class ?Y5) Can) (subclassp (class ?Y5) Can))&:(eq (send ?Y4 get-to) (send ?Y5 get-what))) (time ?X3&:(or (eq (class ?X3) Duration) (subclassp (class ?X3) Duration))) (truth 1))) (test (and (<= (send ?X3 get-start) ?X2) (or (= (send ?X3 get-end) -1) (>= (send ?X3 get-end) ?X2)))) => (add-prop ?X1 (send ?Y4 get-to) ?X2 1))
(defrule 89b1d66ee2bb40bb900852397d9ba6d4 (logical (object (is-a Proposition) (subject ?X2&:(or (eq (class ?X2) Thing) (subclassp (class ?X2) Thing))) (predicate ?Y7&:(or (eq (class ?Y7) IsNeeded) (subclassp (class ?Y7) IsNeeded))) (time ?X3&:(or (eq (class ?X3) Duration) (subclassp (class ?X3) Duration))) (truth 1))) (logical (object (is-a Proposition) (subject ?X1&:(or (eq (class ?X1) Thing) (subclassp (class ?X1) Thing))) (predicate ?Y9&:(or (eq (class ?Y9) Has) (subclassp (class ?Y9) Has))&:(eq (send ?Y9 get-what) ?X2)) (time ?X5&:(or (eq (class ?X5) Duration) (subclassp (class ?X5) Duration))) (truth 1))) 
                (test (or (and (> (mincomstart ?X3 ?X5) -1)
                               (<= (mincomstart ?X3 ?X5) (maxcomend ?X3 ?X5)))
                          (= (maxcomend ?X3 ?X5) -1))
                )
                 => (add-prop ?X1 (add-pred Can what (send ?Y7 get-for_action)) (make-instance of Duration (start (mincomstart ?X3 ?X5)) (end (maxcomend ?X3 ?X5))) 1))
(defrule cf019a6570d24382a7c1d079c91f737f (logical (object (is-a Proposition) (subject ?X1&:(or (eq (class ?X1) Thing) (subclassp (class ?X1) Thing))) (predicate ?Y11&:(or (eq (class ?Y11) IsIn) (subclassp (class ?Y11) IsIn))&:(or (eq (class (send ?Y11 get-what)) Thing) (subclassp (class (send ?Y11 get-what)) Thing))) (time ?X4&:(or (eq (class ?X4) Duration) (subclassp (class ?X4) Duration))) (truth 1))) (logical (object (is-a Proposition) (subject ?X2&:(eq ?X2 (send ?Y11 get-what))) (predicate ?Y13&:(or (eq (class ?Y13) IsIn) (subclassp (class ?Y13) IsIn))&:(or (eq (class (send ?Y13 get-what)) Thing) (subclassp (class (send ?Y13 get-what)) Thing))) (time ?X5&:(or (eq (class ?X5) Duration) (subclassp (class ?X5) Duration))) (truth 1))) 
                (test (or (and (> (mincomstart ?X4 ?X5) -1)
                               (<= (mincomstart ?X4 ?X5) (maxcomend ?X4 ?X5)))
                          (= (maxcomend ?X4 ?X5) -1))
                )
                 => (add-prop ?X1 (add-pred IsIn what (send ?Y13 get-what)) (make-instance of Duration (start (mincomstart ?X4 ?X5)) (end (maxcomend ?X4 ?X5))) 1))
(defrule 2261018a2f394afa81868ac96de6e87e (logical (object (is-a Proposition) (subject ?X1&:(or (eq (class ?X1) Person) (subclassp (class ?X1) Person))) (predicate ?Y15&:(or (eq (class ?Y15) IsIn) (subclassp (class ?Y15) IsIn))) (time ?X3&:(or (eq (class ?X3) Duration) (subclassp (class ?X3) Duration))) (truth 1))) (logical (object (is-a Proposition) (subject ?X2&:(or (eq (class ?X2) Group) (subclassp (class ?X2) Group))) (predicate ?Y17&:(or (eq (class ?Y17) Has) (subclassp (class ?Y17) Has))&:(or (eq (class (send ?Y17 get-what)) Permission) (subclassp (class (send ?Y17 get-what)) Permission))) (time ?X5&:(or (eq (class ?X5) Duration) (subclassp (class ?X5) Duration))) (truth 1))) 
                (test (or (and (> (mincomstart ?X3 ?X5) -1)
                               (<= (mincomstart ?X3 ?X5) (maxcomend ?X3 ?X5)))
                          (= (maxcomend ?X3 ?X5) -1))
                )
                 => (add-prop ?X1 (add-pred Has what (send ?Y17 get-what)) (make-instance of Duration (start (mincomstart ?X3 ?X5)) (end (maxcomend ?X3 ?X5))) 1))
(defrule 46c200b32d92417cb0716dbd574dd591 (logical (object (is-a Proposition) (subject ?X1&:(or (eq (class ?X1) Person) (subclassp (class ?X1) Person))) (predicate ?Y19&:(or (eq (class ?Y19) Has) (subclassp (class ?Y19) Has))&:(or (eq (class (send ?Y19 get-what)) Role) (subclassp (class (send ?Y19 get-what)) Role))) (time ?X3&:(or (eq (class ?X3) Duration) (subclassp (class ?X3) Duration))) (truth 1))) (logical (object (is-a Proposition) (subject ?X2&:(eq ?X2 (send ?Y19 get-what))) (predicate ?Y21&:(or (eq (class ?Y21) Has) (subclassp (class ?Y21) Has))&:(or (eq (class (send ?Y21 get-what)) Permission) (subclassp (class (send ?Y21 get-what)) Permission))) (time ?X5&:(or (eq (class ?X5) Duration) (subclassp (class ?X5) Duration))) (truth 1))) 
                (test (or (and (> (mincomstart ?X3 ?X5) -1)
                               (<= (mincomstart ?X3 ?X5) (maxcomend ?X3 ?X5)))
                          (= (maxcomend ?X3 ?X5) -1))
                )
                 => (add-prop ?X1 (add-pred Has what (send ?Y21 get-what)) (make-instance of Duration (start (mincomstart ?X3 ?X5)) (end (maxcomend ?X3 ?X5))) 1))
(defrule 56f862787d484fb59d68e9227cc82682 (logical (object (is-a Person) (name ?X1))) => (add-prop ?X1 (add-pred Has what [member]) (make-instance of Duration (start 733701.0) (end -1.0)) 1))
(defrule e93c2c00addd4488ab9fc0ca853d5cfc (logical (object (is-a Proposition) (subject ?X1&:(or (eq (class ?X1) Person) (subclassp (class ?X1) Person))) (predicate ?Y23&:(or (eq (class ?Y23) Create) (subclassp (class ?Y23) Create))&:(or (eq (class (send ?Y23 get-what)) Content) (subclassp (class (send ?Y23 get-what)) Content))) (time ?X3) (truth 1))) => (reduce-class (send ?Y23 get-what) Content) (add-prop ?X1 (add-pred IsOwner of (send ?Y23 get-what)) (make-instance of Duration (start ?X3) (end -1.0)) 1) (add-prop (send ?Y23 get-what) (add-pred Has what [private]) (make-instance of Duration (start ?X3) (end -1.0)) 1))
(defrule 88e28a8c5a724a468233b9c90935bdc5 (logical (object (is-a Permission) (name ?X2))) => (add-prop [manager] (add-pred Has what ?X2) (make-instance of Duration (start 733701.0) (end -1.0)) 1))
(defrule 752e310199354ef18721b08137bdef8c (logical (object (is-a Proposition) (subject ?X1&:(or (eq (class ?X1) Content) (subclassp (class ?X1) Content))) (predicate ?Y24&:(or (eq (class ?Y24) Has) (subclassp (class ?Y24) Has))&:(eq (send ?Y24 get-what) [public])) (time ?X2&:(or (eq (class ?X2) Duration) (subclassp (class ?X2) Duration))) (truth 1))) => (add-prop [basic_perm] (add-pred IsNeeded for_action (add-pred View what ?X1)) ?X2 1))
(defrule 0092cfa4657245cabca6ba42201336b3 (logical (object (is-a Proposition) (subject ?X1&:(or (eq (class ?X1) Content) (subclassp (class ?X1) Content))) (predicate ?Y26&:(or (eq (class ?Y26) Has) (subclassp (class ?Y26) Has))&:(eq (send ?Y26 get-what) [private])) (time ?X2&:(or (eq (class ?X2) Duration) (subclassp (class ?X2) Duration))) (truth 1))) => (add-prop [manage_perm] (add-pred IsNeeded for_action (add-pred View what ?X1)) ?X2 1))
(defrule 0dc3e8445f584eadb166ef0ab18c1151 (logical (object (is-a Proposition) (subject ?X1&:(or (eq (class ?X1) Content) (subclassp (class ?X1) Content))) (predicate ?Y28&:(or (eq (class ?Y28) Has) (subclassp (class ?Y28) Has))&:(eq (send ?Y28 get-what) [private])) (time ?X3&:(or (eq (class ?X3) Duration) (subclassp (class ?X3) Duration))) (truth 1))) (logical (object (is-a Proposition) (subject ?X2&:(or (eq (class ?X2) Person) (subclassp (class ?X2) Person))) (predicate ?Y30&:(or (eq (class ?Y30) IsOwner) (subclassp (class ?Y30) IsOwner))&:(eq (send ?Y30 get-of) ?X1)) (time ?X4&:(or (eq (class ?X4) Duration) (subclassp (class ?X4) Duration))) (truth 1))) 
                (test (or (and (> (mincomstart ?X3 ?X4) -1)
                               (<= (mincomstart ?X3 ?X4) (maxcomend ?X3 ?X4)))
                          (= (maxcomend ?X3 ?X4) -1))
                )
                 => (add-prop ?X2 (add-pred Can what (add-pred View what ?X1)) (make-instance of Duration (start (mincomstart ?X3 ?X4)) (end (maxcomend ?X3 ?X4))) 1))
(defrule ede9febde0694690b5816fb199faad32 (logical (object (is-a Proposition) (subject ?X1&:(or (eq (class ?X1) Person) (subclassp (class ?X1) Person))) (predicate ?Y32&:(or (eq (class ?Y32) Publish) (subclassp (class ?Y32) Publish))&:(or (eq (class (send ?Y32 get-what)) Content) (subclassp (class (send ?Y32 get-what)) Content))) (time ?X3) (truth 1))) (logical (object (is-a Proposition) (subject ?X2&:(eq ?X2 (send ?Y32 get-what))) (predicate ?Y33&:(or (eq (class ?Y33) Has) (subclassp (class ?Y33) Has))&:(or (eq (class (send ?Y33 get-what)) Status) (subclassp (class (send ?Y33 get-what)) Status))) (time ?X5&:(or (eq (class ?X5) Duration) (subclassp (class ?X5) Duration))) (truth 1))) => (send ?X5 put-end 733701) (add-prop (send ?Y32 get-what) (add-pred Has what [public]) (make-instance of Duration (start ?X3) (end -1.0)) 1))
(defrule 2c451e20c65f4fe7bdb08572bb8f60b8 (logical (object (is-a Content) (name ?X1))) => (add-prop [manage_perm] (add-pred IsNeeded for_action (add-pred Publish what ?X1)) (make-instance of Duration (start 733701.0) (end -1.0)) 1))
(defrule 78ca644c65fa4f01a84e352df3980209 (logical (object (is-a Proposition) (subject ?X1&:(or (eq (class ?X1) Person) (subclassp (class ?X1) Person))) (predicate ?Y35&:(or (eq (class ?Y35) Hide) (subclassp (class ?Y35) Hide))&:(or (eq (class (send ?Y35 get-what)) Content) (subclassp (class (send ?Y35 get-what)) Content))) (time ?X3) (truth 1))) (logical (object (is-a Proposition) (subject ?X2&:(eq ?X2 (send ?Y35 get-what))) (predicate ?Y36&:(or (eq (class ?Y36) Has) (subclassp (class ?Y36) Has))&:(or (eq (class (send ?Y36 get-what)) Status) (subclassp (class (send ?Y36 get-what)) Status))) (time ?X5&:(or (eq (class ?X5) Duration) (subclassp (class ?X5) Duration))) (truth 1))) => (send ?X5 put-end 733701) (add-prop (send ?Y35 get-what) (add-pred Has what [private]) (make-instance of Duration (start ?X3) (end -1.0)) 1))
(defrule f132a7fc74ef443d8c95824d9e21a2d8 (logical (object (is-a Proposition) (subject ?X1&:(or (eq (class ?X1) Person) (subclassp (class ?X1) Person))) (predicate ?Y38&:(or (eq (class ?Y38) IsOwner) (subclassp (class ?Y38) IsOwner))&:(or (eq (class (send ?Y38 get-of)) Content) (subclassp (class (send ?Y38 get-of)) Content))) (time ?X3&:(or (eq (class ?X3) Duration) (subclassp (class ?X3) Duration))) (truth 1))) => (add-prop ?X1 (add-pred Can what (add-pred Hide what (send ?Y38 get-of))) ?X3 1))
(reduce-class [john] Person)
(reduce-class [pete] Person)
(reduce-class [jane] Person)
(reduce-class [c1] Content)
(reduce-class [c2] Content)
(add-prop [john] (add-pred Has what [manager]) (make-instance of Duration (start 733701.0) (end -1.0)) 1)
(add-prop [jane] (add-pred Has what [create_perm]) (make-instance of Duration (start 733701.0) (end -1.0)) 1)
(add-prop [jane] (add-pred Wants to (add-pred Create what [c1])) 733701.0 1)
(add-prop [pete] (add-pred Wants to (add-pred Create what [c2])) 733701.0 1)
(find-all-instances ((?prop Proposition) (?Y40 IsOwner) (?Y41 Duration)) (and (eq ?prop:subject [jane]) (eq ?Y40:of [c1]) (eq ?prop:predicate ?Y40) (= ?Y41:start 733701.0) (= ?Y41:end -1.0) (eq ?prop:truth 1)))
0


no
----------running---------------------
----------runned: 33---------------------
(find-all-instances ((?prop Proposition) (?Y42 IsOwner) (?Y43 Duration)) (and (eq ?prop:subject [jane]) (eq ?Y42:of [c1]) (eq ?prop:predicate ?Y42) (= ?Y43:start 733701.0) (= ?Y43:end -1.0) (eq ?prop:truth 1)))
35


jane isowner of c1 at from 733701.0 till -1.0
(find-all-instances ((?prop Proposition) (?Y44 Has) (?Y45 Duration)) (and (eq ?prop:subject [c1]) (eq ?Y44:what [private]) (eq ?prop:predicate ?Y44) (= ?Y45:start 733701.0) (= ?Y45:end -1.0) (eq ?prop:truth 1)))
35


c1 has what private at from 733701.0 till -1.0
(find-all-instances ((?prop Proposition) (?Y46 IsOwner) (?Y47 Duration)) (and (eq ?prop:subject [pete]) (eq ?Y46:of [c2]) (eq ?prop:predicate ?Y46) (= ?Y47:start 733701.0) (= ?Y47:end -1.0) (eq ?prop:truth 1)))
0


no
(add-prop [jane] (add-pred Wants to (add-pred Publish what [c1])) 733701.0 1)
(add-prop [pete] (add-pred Wants to (add-pred Publish what [c2])) 733701.0 1)
----------running---------------------
----------runned: 0---------------------
(find-all-instances ((?prop Proposition) (?Y48 Has) (?Y49 Duration)) (and (eq ?prop:subject [c1]) (eq ?Y48:what [public]) (eq ?prop:predicate ?Y48) (= ?Y49:start 733701.0) (= ?Y49:end -1.0) (eq ?prop:truth 1)))
0


no
(find-all-instances ((?prop Proposition) (?Y50 Has) (?Y51 Duration)) (and (eq ?prop:subject [c2]) (eq ?Y50:what [public]) (eq ?prop:predicate ?Y50) (= ?Y51:start 733701.0) (= ?Y51:end -1.0) (eq ?prop:truth 1)))
0


no
(find-all-instances ((?prop Proposition) (?Y52 Can) (?Y53 View) (?Y54 Duration)) (and (eq ?prop:subject [jane]) (eq ?Y53:what [c1]) (eq ?Y52:what ?Y53) (eq ?prop:predicate ?Y52) (= ?Y54:start 733701.0) (= ?Y54:end -1.0) (eq ?prop:truth 1)))
35


jane can what view what c1 at from 733701.0 till -1.0
(find-all-instances ((?prop Proposition) (?Y55 Can) (?Y56 View) (?Y57 Duration)) (and (eq ?prop:subject [pete]) (eq ?Y56:what [c1]) (eq ?Y55:what ?Y56) (eq ?prop:predicate ?Y55) (= ?Y57:start 733701.0) (= ?Y57:end -1.0) (eq ?prop:truth 1)))
0


no
(add-prop [john] (add-pred Wants to (add-pred Publish what [c1])) 733701.0 1)
----------running---------------------
----------runned: 10---------------------
(find-all-instances ((?prop Proposition) (?Y58 Has) (?Y59 Duration)) (and (eq ?prop:subject [c1]) (eq ?Y58:what [private]) (eq ?prop:predicate ?Y58) (= ?Y59:start 733701.0) (= ?Y59:end -1.0) (eq ?prop:truth 1)))
0


no
(find-all-instances ((?prop Proposition) (?Y60 Has) (?Y61 Duration)) (and (eq ?prop:subject [c1]) (eq ?Y60:what [public]) (eq ?prop:predicate ?Y60) (= ?Y61:start 733701.0) (= ?Y61:end -1.0) (eq ?prop:truth 1)))
41


c1 has what public at from 733701.0 till -1.0
(find-all-instances ((?prop Proposition) (?Y62 Can) (?Y63 View) (?Y64 Duration)) (and (eq ?prop:subject [pete]) (eq ?Y63:what [c1]) (eq ?Y62:what ?Y63) (eq ?prop:predicate ?Y62) (= ?Y64:start 733701.0) (= ?Y64:end -1.0) (eq ?prop:truth 1)))
41


pete can what view what c1 at from 733701.0 till -1.0
(defclass Name (is-a USER))

(deffunction reduce-class (?instance ?class)
    (if (eq (length$
                (find-all-instances ((?a ?class))(eq (instance-name ?a) ?instance)))
             0)
    then (make-instance ?instance of ?class)
    else (return TRUE)))
(defclass Thing (is-a Name))
(defclass Verb (is-a USER))
(defclass State (is-a Verb) )
(set-sequence-operator-recognition TRUE)
(defmessage-handler State set-slots primary ($?slots)
        (while (> (length$ ?slots) 0) do
            (bind ?slot (first$ ?slots))
            (bind ?vslots (rest$ ?slots))
            (bind ?value (first$ ?vslots))
            (bind ?slots (rest$ ?vslots))
            (dynamic-put $?slot $?value))
        (return (instance-name ?self)))


(deffunction add-pred (?class $?slots)
        (bind ?key (str-cat ?class $?slots))
        (bind ?pos (str-index "." ?key))
        (while ?pos do
            (bind ?key (str-cat (sub-string 1 (- ?pos 1) ?key)
                                "_"
                                (sub-string (+ ?pos 1) (str-length ?key) ?key)))
            (bind ?pos (str-index "." ?key)))
        (bind ?key (sym-cat ?key))
        (if (instance-existp ?key) then
            (return (instance-name ?key))
         else
            (make-instance ?key of ?class)
            (return (send (instance-name ?key) set-slots $?slots))))

(defclass Duration (is-a Name) (slot start (type NUMBER) (pattern-match reactive)) (slot end (type NUMBER) (pattern-match reactive)))

(deffunction extract-now ($?instants)
    (if (= (length$ ?instants) 0) then
        (return -1)
    else
        (bind ?count 0)
        (bind ?not-now (create$))
        (while (> (length$ ?instants) 0) do
            (bind ?instant (nth$ 1 ?instants))
            (bind ?instants (rest$ ?instants))
            (if (= ?instant -1)
            then (bind ?count (+ ?count 1))
            else (bind ?not-now (insert$ ?not-now 1 ?instant)))
            )
        (return (insert$ ?not-now 1 ?count))
    )
)


(deffunction maxcomend ($?durations)
    (bind ?ends (create$))
    (while (> (length$ ?durations) 0)
        (bind ?ends (insert$ ?ends 1 (send (nth$ 1 ?durations) get-end)))
        (bind ?durations (rest$ ?durations))
        )
    (bind ?ret (extract-now ?ends))
    (bind ?pasts (rest$ ?ret))
    (if (> (length$ ?pasts) 0)
    then (return (min (expand$ ?pasts)))
    else (return -1)
    )
)


(deffunction mincomstart ($?durations)
    (bind ?starts (create$))
    (while (> (length$ ?durations) 0)
        (bind ?starts (insert$ ?starts 1 (send (nth$ 1 ?durations) get-start)))
        (bind ?durations (rest$ ?durations))
        )
    (bind ?ret (extract-now ?starts))
    (bind ?nows (nth$ 1 ?ret))
    (if (> ?nows 0)
    then (return -1)
    else (bind ?ret (rest$ ?ret))
         (return (max (expand$ ?ret)))
    )
)

(defclass Proposition (is-a Name) (slot truth (type INTEGER) (default 1) (pattern-match reactive)) (slot subject (type INSTANCE) (pattern-match reactive)) (slot predicate (type INSTANCE) (pattern-match reactive)) (slot time (type ?VARIABLE) (pattern-match reactive)))

(deffunction add-prop (?s ?p ?t ?r)
       (bind ?count 0)
       (do-for-all-instances ((?prop Proposition))
                          (and (eq ?prop:subject ?s)
                               (eq ?prop:predicate ?p)
                               (or (and (eq (class ?t) Duration)
                                        (= (send (send ?prop get-time) get-start) (send ?t get-start))
                                        (= (send (send ?prop get-time) get-end) (send ?t get-end)))
                                   (= ?prop:time ?t))
                               (= ?prop:truth ?r))
               (bind ?count (+ ?count 1)))
        (if (= ?count 0)
        then (make-instance of Proposition (subject ?s)
                                           (predicate ?p)
                                           (time ?t)
                                           (truth ?r))
        else (return TRUE)))
(defclass Person (is-a Thing))
(defclass Can (is-a State) (slot what (type INSTANCE) (visibility public) (pattern-match reactive)))
(defclass Wants (is-a State) (slot to (type INSTANCE) (visibility public) (pattern-match reactive)))
(defclass Has (is-a State) (slot what (type INSTANCE) (visibility public) (pattern-match reactive)))
(defclass IsNeeded (is-a State) (slot for_action (type INSTANCE) (visibility public) (pattern-match reactive)))
(defclass IsIn (is-a State) (slot what (type INSTANCE) (visibility public) (pattern-match reactive)))
(defclass Group (is-a Thing))
(defclass Permission (is-a Thing))
(defclass Role (is-a Thing))
(defclass Content (is-a Thing))
(defclass Create (is-a State) (slot what (type INSTANCE) (visibility public) (pattern-match reactive)))
(defclass IsOwner (is-a State) (slot of (type INSTANCE) (visibility public) (pattern-match reactive)))
(defclass Status (is-a Thing))
(defclass View (is-a State) (slot what (type INSTANCE) (visibility public) (pattern-match reactive)))
(defclass Publish (is-a State) (slot what (type INSTANCE) (visibility public) (pattern-match reactive)))
(defclass Hide (is-a State) (slot what (type INSTANCE) (visibility public) (pattern-match reactive)))
(reduce-class [admin] Person)
(reduce-class [member] Role)
(reduce-class [manager] Role)
(reduce-class [basic_perm] Permission)
(reduce-class [manage_perm] Permission)
(reduce-class [create_perm] Permission)
(reduce-class [public] Status)
(reduce-class [private] Status)
(add-prop [admin] (add-pred Has what [manager]) (make-instance of Duration (start 733701.0) (end -1.0)) 1)
(add-prop [member] (add-pred Has what [basic_perm]) (make-instance of Duration (start 733701.0) (end -1.0)) 1)
(defrule c6b33e40c0f74ccda7b7e7424a439f04 (logical (object (is-a Proposition) (subject ?X1&:(or (eq (class ?X1) Person) (subclassp (class ?X1) Person))) (predicate ?Y1&:(or (eq (class ?Y1) Wants) (subclassp (class ?Y1) Wants))&:(or (eq (class (send ?Y1 get-to)) Create) (subclassp (class (send ?Y1 get-to)) Create))&:(or (eq (class (send (send ?Y1 get-to) get-what)) Thing) (subclassp (class (send (send ?Y1 get-to) get-what)) Thing))) (time ?X2) (truth 1))) (logical (object (is-a Proposition) (subject ?X1) (predicate ?Y2&:(or (eq (class ?Y2) Has) (subclassp (class ?Y2) Has))&:(eq (send ?Y2 get-what) [create_perm])) (time ?X3&:(or (eq (class ?X3) Duration) (subclassp (class ?X3) Duration))) (truth 1))) (test (and (<= (send ?X3 get-start) ?X2) (or (= (send ?X3 get-end) -1) (>= (send ?X3 get-end) ?X2)))) => (add-prop ?X1 (add-pred Create what (send (send ?Y1 get-to) get-what)) ?X2 1))
(defrule a034a292c36746239d27f1280523678e (logical (object (is-a Proposition) (subject ?X1&:(or (eq (class ?X1) Person) (subclassp (class ?X1) Person))) (predicate ?Y4&:(or (eq (class ?Y4) Wants) (subclassp (class ?Y4) Wants))) (time ?X2) (truth 1))) (logical (object (is-a Proposition) (subject ?X1) (predicate ?Y5&:(or (eq (class ?Y5) Can) (subclassp (class ?Y5) Can))&:(eq (send ?Y4 get-to) (send ?Y5 get-what))) (time ?X3&:(or (eq (class ?X3) Duration) (subclassp (class ?X3) Duration))) (truth 1))) (test (and (<= (send ?X3 get-start) ?X2) (or (= (send ?X3 get-end) -1) (>= (send ?X3 get-end) ?X2)))) => (add-prop ?X1 (send ?Y4 get-to) ?X2 1))
(defrule 01dd5bbca8c142318b42c95d941412b8 (logical (object (is-a Proposition) (subject ?X2&:(or (eq (class ?X2) Thing) (subclassp (class ?X2) Thing))) (predicate ?Y7&:(or (eq (class ?Y7) IsNeeded) (subclassp (class ?Y7) IsNeeded))) (time ?X3&:(or (eq (class ?X3) Duration) (subclassp (class ?X3) Duration))) (truth 1))) (logical (object (is-a Proposition) (subject ?X1&:(or (eq (class ?X1) Thing) (subclassp (class ?X1) Thing))) (predicate ?Y9&:(or (eq (class ?Y9) Has) (subclassp (class ?Y9) Has))&:(eq (send ?Y9 get-what) ?X2)) (time ?X5&:(or (eq (class ?X5) Duration) (subclassp (class ?X5) Duration))) (truth 1))) 
                (test (or (and (> (mincomstart ?X3 ?X5) -1)
                               (<= (mincomstart ?X3 ?X5) (maxcomend ?X3 ?X5)))
                          (= (maxcomend ?X3 ?X5) -1))
                )
                 => (add-prop ?X1 (add-pred Can what (send ?Y7 get-for_action)) (make-instance of Duration (start (mincomstart ?X3 ?X5)) (end (maxcomend ?X3 ?X5))) 1))
(defrule 535c253bd79d4cae8557d1a493e2f538 (logical (object (is-a Proposition) (subject ?X1&:(or (eq (class ?X1) Thing) (subclassp (class ?X1) Thing))) (predicate ?Y11&:(or (eq (class ?Y11) IsIn) (subclassp (class ?Y11) IsIn))&:(or (eq (class (send ?Y11 get-what)) Thing) (subclassp (class (send ?Y11 get-what)) Thing))) (time ?X4&:(or (eq (class ?X4) Duration) (subclassp (class ?X4) Duration))) (truth 1))) (logical (object (is-a Proposition) (subject ?X2&:(eq ?X2 (send ?Y11 get-what))) (predicate ?Y13&:(or (eq (class ?Y13) IsIn) (subclassp (class ?Y13) IsIn))&:(or (eq (class (send ?Y13 get-what)) Thing) (subclassp (class (send ?Y13 get-what)) Thing))) (time ?X5&:(or (eq (class ?X5) Duration) (subclassp (class ?X5) Duration))) (truth 1))) 
                (test (or (and (> (mincomstart ?X4 ?X5) -1)
                               (<= (mincomstart ?X4 ?X5) (maxcomend ?X4 ?X5)))
                          (= (maxcomend ?X4 ?X5) -1))
                )
                 => (add-prop ?X1 (add-pred IsIn what (send ?Y13 get-what)) (make-instance of Duration (start (mincomstart ?X4 ?X5)) (end (maxcomend ?X4 ?X5))) 1))
(defrule d22065ef2d564c5cbb91db6cfd0deeb8 (logical (object (is-a Proposition) (subject ?X1&:(or (eq (class ?X1) Person) (subclassp (class ?X1) Person))) (predicate ?Y15&:(or (eq (class ?Y15) IsIn) (subclassp (class ?Y15) IsIn))) (time ?X3&:(or (eq (class ?X3) Duration) (subclassp (class ?X3) Duration))) (truth 1))) (logical (object (is-a Proposition) (subject ?X2&:(or (eq (class ?X2) Group) (subclassp (class ?X2) Group))) (predicate ?Y17&:(or (eq (class ?Y17) Has) (subclassp (class ?Y17) Has))&:(or (eq (class (send ?Y17 get-what)) Permission) (subclassp (class (send ?Y17 get-what)) Permission))) (time ?X5&:(or (eq (class ?X5) Duration) (subclassp (class ?X5) Duration))) (truth 1))) 
                (test (or (and (> (mincomstart ?X3 ?X5) -1)
                               (<= (mincomstart ?X3 ?X5) (maxcomend ?X3 ?X5)))
                          (= (maxcomend ?X3 ?X5) -1))
                )
                 => (add-prop ?X1 (add-pred Has what (send ?Y17 get-what)) (make-instance of Duration (start (mincomstart ?X3 ?X5)) (end (maxcomend ?X3 ?X5))) 1))
(defrule c79f97dd10a34e998082454ea83d9b89 (logical (object (is-a Proposition) (subject ?X1&:(or (eq (class ?X1) Person) (subclassp (class ?X1) Person))) (predicate ?Y19&:(or (eq (class ?Y19) Has) (subclassp (class ?Y19) Has))&:(or (eq (class (send ?Y19 get-what)) Role) (subclassp (class (send ?Y19 get-what)) Role))) (time ?X3&:(or (eq (class ?X3) Duration) (subclassp (class ?X3) Duration))) (truth 1))) (logical (object (is-a Proposition) (subject ?X2&:(eq ?X2 (send ?Y19 get-what))) (predicate ?Y21&:(or (eq (class ?Y21) Has) (subclassp (class ?Y21) Has))&:(or (eq (class (send ?Y21 get-what)) Permission) (subclassp (class (send ?Y21 get-what)) Permission))) (time ?X5&:(or (eq (class ?X5) Duration) (subclassp (class ?X5) Duration))) (truth 1))) 
                (test (or (and (> (mincomstart ?X3 ?X5) -1)
                               (<= (mincomstart ?X3 ?X5) (maxcomend ?X3 ?X5)))
                          (= (maxcomend ?X3 ?X5) -1))
                )
                 => (add-prop ?X1 (add-pred Has what (send ?Y21 get-what)) (make-instance of Duration (start (mincomstart ?X3 ?X5)) (end (maxcomend ?X3 ?X5))) 1))
(defrule e3cda4e1e69d494fa99e568fc94005eb (logical (object (is-a Person) (name ?X1))) => (add-prop ?X1 (add-pred Has what [member]) (make-instance of Duration (start 733701.0) (end -1.0)) 1))
(defrule 9bd7401da49a4dccbde2f202327012a4 (logical (object (is-a Proposition) (subject ?X1&:(or (eq (class ?X1) Person) (subclassp (class ?X1) Person))) (predicate ?Y23&:(or (eq (class ?Y23) Create) (subclassp (class ?Y23) Create))&:(or (eq (class (send ?Y23 get-what)) Content) (subclassp (class (send ?Y23 get-what)) Content))) (time ?X3) (truth 1))) => (reduce-class (send ?Y23 get-what) Content) (add-prop ?X1 (add-pred IsOwner of (send ?Y23 get-what)) (make-instance of Duration (start ?X3) (end -1.0)) 1) (add-prop (send ?Y23 get-what) (add-pred Has what [private]) (make-instance of Duration (start ?X3) (end -1.0)) 1))
(defrule d4d3f4daff1f44b281f9e8bb55a9e30b (logical (object (is-a Permission) (name ?X2))) => (add-prop [manager] (add-pred Has what ?X2) (make-instance of Duration (start 733701.0) (end -1.0)) 1))
(defrule 673ba240b27f4c4dbc1ff298748776ac (logical (object (is-a Proposition) (subject ?X1&:(or (eq (class ?X1) Content) (subclassp (class ?X1) Content))) (predicate ?Y24&:(or (eq (class ?Y24) Has) (subclassp (class ?Y24) Has))&:(eq (send ?Y24 get-what) [public])) (time ?X2&:(or (eq (class ?X2) Duration) (subclassp (class ?X2) Duration))) (truth 1))) => (add-prop [basic_perm] (add-pred IsNeeded for_action (add-pred View what ?X1)) ?X2 1))
(defrule a4a2632e84994adbb2987d03e676a5ba (logical (object (is-a Proposition) (subject ?X1&:(or (eq (class ?X1) Content) (subclassp (class ?X1) Content))) (predicate ?Y26&:(or (eq (class ?Y26) Has) (subclassp (class ?Y26) Has))&:(eq (send ?Y26 get-what) [private])) (time ?X2&:(or (eq (class ?X2) Duration) (subclassp (class ?X2) Duration))) (truth 1))) => (add-prop [manage_perm] (add-pred IsNeeded for_action (add-pred View what ?X1)) ?X2 1))
(defrule c4e439740b3b4aa1a59c678757903467 (logical (object (is-a Proposition) (subject ?X1&:(or (eq (class ?X1) Content) (subclassp (class ?X1) Content))) (predicate ?Y28&:(or (eq (class ?Y28) Has) (subclassp (class ?Y28) Has))&:(eq (send ?Y28 get-what) [private])) (time ?X3&:(or (eq (class ?X3) Duration) (subclassp (class ?X3) Duration))) (truth 1))) (logical (object (is-a Proposition) (subject ?X2&:(or (eq (class ?X2) Person) (subclassp (class ?X2) Person))) (predicate ?Y30&:(or (eq (class ?Y30) IsOwner) (subclassp (class ?Y30) IsOwner))&:(eq (send ?Y30 get-of) ?X1)) (time ?X4&:(or (eq (class ?X4) Duration) (subclassp (class ?X4) Duration))) (truth 1))) 
                (test (or (and (> (mincomstart ?X3 ?X4) -1)
                               (<= (mincomstart ?X3 ?X4) (maxcomend ?X3 ?X4)))
                          (= (maxcomend ?X3 ?X4) -1))
                )
                 => (add-prop ?X2 (add-pred Can what (add-pred View what ?X1)) (make-instance of Duration (start (mincomstart ?X3 ?X4)) (end (maxcomend ?X3 ?X4))) 1))
(defrule 35c3a514fa554c12907e68a635d2c215 (logical (object (is-a Proposition) (subject ?X1&:(or (eq (class ?X1) Person) (subclassp (class ?X1) Person))) (predicate ?Y32&:(or (eq (class ?Y32) Publish) (subclassp (class ?Y32) Publish))&:(or (eq (class (send ?Y32 get-what)) Content) (subclassp (class (send ?Y32 get-what)) Content))) (time ?X3) (truth 1))) (logical (object (is-a Proposition) (subject ?X2&:(eq ?X2 (send ?Y32 get-what))) (predicate ?Y33&:(or (eq (class ?Y33) Has) (subclassp (class ?Y33) Has))&:(or (eq (class (send ?Y33 get-what)) Status) (subclassp (class (send ?Y33 get-what)) Status))) (time ?X5&:(or (eq (class ?X5) Duration) (subclassp (class ?X5) Duration))) (truth 1))) => (send ?X5 put-end 733701) (add-prop (send ?Y32 get-what) (add-pred Has what [public]) (make-instance of Duration (start ?X3) (end -1.0)) 1))
(defrule a0bf4ca2e4e94c08b36b3a41e88eafef (logical (object (is-a Content) (name ?X1))) => (add-prop [manage_perm] (add-pred IsNeeded for_action (add-pred Publish what ?X1)) (make-instance of Duration (start 733701.0) (end -1.0)) 1))
(defrule d3d02a8a6b41476aa47998136f713a98 (logical (object (is-a Proposition) (subject ?X1&:(or (eq (class ?X1) Person) (subclassp (class ?X1) Person))) (predicate ?Y35&:(or (eq (class ?Y35) Hide) (subclassp (class ?Y35) Hide))&:(or (eq (class (send ?Y35 get-what)) Content) (subclassp (class (send ?Y35 get-what)) Content))) (time ?X3) (truth 1))) (logical (object (is-a Proposition) (subject ?X2&:(eq ?X2 (send ?Y35 get-what))) (predicate ?Y36&:(or (eq (class ?Y36) Has) (subclassp (class ?Y36) Has))&:(or (eq (class (send ?Y36 get-what)) Status) (subclassp (class (send ?Y36 get-what)) Status))) (time ?X5&:(or (eq (class ?X5) Duration) (subclassp (class ?X5) Duration))) (truth 1))) => (send ?X5 put-end 733701) (add-prop (send ?Y35 get-what) (add-pred Has what [private]) (make-instance of Duration (start ?X3) (end -1.0)) 1))
(defrule 3ab591312db84adcbd14fd2291fe3ebc (logical (object (is-a Proposition) (subject ?X1&:(or (eq (class ?X1) Person) (subclassp (class ?X1) Person))) (predicate ?Y38&:(or (eq (class ?Y38) IsOwner) (subclassp (class ?Y38) IsOwner))&:(or (eq (class (send ?Y38 get-of)) Content) (subclassp (class (send ?Y38 get-of)) Content))) (time ?X3&:(or (eq (class ?X3) Duration) (subclassp (class ?X3) Duration))) (truth 1))) => (add-prop ?X1 (add-pred Can what (add-pred Hide what (send ?Y38 get-of))) ?X3 1))
(reduce-class [john] Person)
(reduce-class [pete] Person)
(reduce-class [jane] Person)
(reduce-class [c1] Content)
(reduce-class [c2] Content)
(add-prop [john] (add-pred Has what [manager]) (make-instance of Duration (start 733701.0) (end -1.0)) 1)
(add-prop [jane] (add-pred Has what [create_perm]) (make-instance of Duration (start 733701.0) (end -1.0)) 1)
(add-prop [jane] (add-pred Wants to (add-pred Create what [c1])) 733701.0 1)
(add-prop [pete] (add-pred Wants to (add-pred Create what [c2])) 733701.0 1)
(find-all-instances ((?prop Proposition) (?Y40 IsOwner) (?Y41 Duration)) (and (eq ?prop:subject [jane]) (eq ?Y40:of [c1]) (eq ?prop:predicate ?Y40) (= ?Y41:start 733701.0) (= ?Y41:end -1.0) (eq ?prop:truth 1)))
0


no
----------running---------------------
----------runned: 33---------------------
(find-all-instances ((?prop Proposition) (?Y42 IsOwner) (?Y43 Duration)) (and (eq ?prop:subject [jane]) (eq ?Y42:of [c1]) (eq ?prop:predicate ?Y42) (= ?Y43:start 733701.0) (= ?Y43:end -1.0) (eq ?prop:truth 1)))
35


jane isowner of c1 at from 733701.0 till -1.0
(find-all-instances ((?prop Proposition) (?Y44 Has) (?Y45 Duration)) (and (eq ?prop:subject [c1]) (eq ?Y44:what [private]) (eq ?prop:predicate ?Y44) (= ?Y45:start 733701.0) (= ?Y45:end -1.0) (eq ?prop:truth 1)))
35


c1 has what private at from 733701.0 till -1.0
(find-all-instances ((?prop Proposition) (?Y46 IsOwner) (?Y47 Duration)) (and (eq ?prop:subject [pete]) (eq ?Y46:of [c2]) (eq ?prop:predicate ?Y46) (= ?Y47:start 733701.0) (= ?Y47:end -1.0) (eq ?prop:truth 1)))
0


no
(add-prop [jane] (add-pred Wants to (add-pred Publish what [c1])) 733701.0 1)
(add-prop [pete] (add-pred Wants to (add-pred Publish what [c2])) 733701.0 1)
----------running---------------------
----------runned: 0---------------------
(find-all-instances ((?prop Proposition) (?Y48 Has) (?Y49 Duration)) (and (eq ?prop:subject [c1]) (eq ?Y48:what [public]) (eq ?prop:predicate ?Y48) (= ?Y49:start 733701.0) (= ?Y49:end -1.0) (eq ?prop:truth 1)))
0


no
(find-all-instances ((?prop Proposition) (?Y50 Has) (?Y51 Duration)) (and (eq ?prop:subject [c2]) (eq ?Y50:what [public]) (eq ?prop:predicate ?Y50) (= ?Y51:start 733701.0) (= ?Y51:end -1.0) (eq ?prop:truth 1)))
0


no
(find-all-instances ((?prop Proposition) (?Y52 Can) (?Y53 View) (?Y54 Duration)) (and (eq ?prop:subject [jane]) (eq ?Y53:what [c1]) (eq ?Y52:what ?Y53) (eq ?prop:predicate ?Y52) (= ?Y54:start 733701.0) (= ?Y54:end -1.0) (eq ?prop:truth 1)))
35


jane can what view what c1 at from 733701.0 till -1.0
(find-all-instances ((?prop Proposition) (?Y55 Can) (?Y56 View) (?Y57 Duration)) (and (eq ?prop:subject [pete]) (eq ?Y56:what [c1]) (eq ?Y55:what ?Y56) (eq ?prop:predicate ?Y55) (= ?Y57:start 733701.0) (= ?Y57:end -1.0) (eq ?prop:truth 1)))
0


no
(add-prop [john] (add-pred Wants to (add-pred Publish what [c1])) 733701.0 1)
----------running---------------------
----------runned: 10---------------------
(find-all-instances ((?prop Proposition) (?Y58 Has) (?Y59 Duration)) (and (eq ?prop:subject [c1]) (eq ?Y58:what [private]) (eq ?prop:predicate ?Y58) (= ?Y59:start 733701.0) (= ?Y59:end -1.0) (eq ?prop:truth 1)))
0


no
(find-all-instances ((?prop Proposition) (?Y60 Has) (?Y61 Duration)) (and (eq ?prop:subject [c1]) (eq ?Y60:what [public]) (eq ?prop:predicate ?Y60) (= ?Y61:start 733701.0) (= ?Y61:end -1.0) (eq ?prop:truth 1)))
41


c1 has what public at from 733701.0 till -1.0
(find-all-instances ((?prop Proposition) (?Y62 Can) (?Y63 View) (?Y64 Duration)) (and (eq ?prop:subject [pete]) (eq ?Y63:what [c1]) (eq ?Y62:what ?Y63) (eq ?prop:predicate ?Y62) (= ?Y64:start 733701.0) (= ?Y64:end -1.0) (eq ?prop:truth 1)))
41


pete can what view what c1 at from 733701.0 till -1.0
(defclass Name (is-a USER))

(deffunction reduce-class (?instance ?class)
    (if (eq (length$
                (find-all-instances ((?a ?class))(eq (instance-name ?a) ?instance)))
             0)
    then (make-instance ?instance of ?class)
    else (return TRUE)))
(defclass Thing (is-a Name))
(defclass Verb (is-a USER))
(defclass State (is-a Verb) )
(set-sequence-operator-recognition TRUE)
(defmessage-handler State set-slots primary ($?slots)
        (while (> (length$ ?slots) 0) do
            (bind ?slot (first$ ?slots))
            (bind ?vslots (rest$ ?slots))
            (bind ?value (first$ ?vslots))
            (bind ?slots (rest$ ?vslots))
            (dynamic-put $?slot $?value))
        (return (instance-name ?self)))


(deffunction add-pred (?class $?slots)
        (bind ?key (str-cat ?class $?slots))
        (bind ?pos (str-index "." ?key))
        (while ?pos do
            (bind ?key (str-cat (sub-string 1 (- ?pos 1) ?key)
                                "_"
                                (sub-string (+ ?pos 1) (str-length ?key) ?key)))
            (bind ?pos (str-index "." ?key)))
        (bind ?key (sym-cat ?key))
        (if (instance-existp ?key) then
            (return (instance-name ?key))
         else
            (make-instance ?key of ?class)
            (return (send (instance-name ?key) set-slots $?slots))))

(defclass Duration (is-a Name) (slot start (type NUMBER) (pattern-match reactive)) (slot end (type NUMBER) (pattern-match reactive)))

(deffunction extract-now ($?instants)
    (if (= (length$ ?instants) 0) then
        (return -1)
    else
        (bind ?count 0)
        (bind ?not-now (create$))
        (while (> (length$ ?instants) 0) do
            (bind ?instant (nth$ 1 ?instants))
            (bind ?instants (rest$ ?instants))
            (if (= ?instant -1)
            then (bind ?count (+ ?count 1))
            else (bind ?not-now (insert$ ?not-now 1 ?instant)))
            )
        (return (insert$ ?not-now 1 ?count))
    )
)


(deffunction maxcomend ($?durations)
    (bind ?ends (create$))
    (while (> (length$ ?durations) 0)
        (bind ?ends (insert$ ?ends 1 (send (nth$ 1 ?durations) get-end)))
        (bind ?durations (rest$ ?durations))
        )
    (bind ?ret (extract-now ?ends))
    (bind ?pasts (rest$ ?ret))
    (if (> (length$ ?pasts) 0)
    then (return (min (expand$ ?pasts)))
    else (return -1)
    )
)


(deffunction mincomstart ($?durations)
    (bind ?starts (create$))
    (while (> (length$ ?durations) 0)
        (bind ?starts (insert$ ?starts 1 (send (nth$ 1 ?durations) get-start)))
        (bind ?durations (rest$ ?durations))
        )
    (bind ?ret (extract-now ?starts))
    (bind ?nows (nth$ 1 ?ret))
    (if (> ?nows 0)
    then (return -1)
    else (bind ?ret (rest$ ?ret))
         (return (max (expand$ ?ret)))
    )
)

(defclass Proposition (is-a Name) (slot truth (type INTEGER) (default 1) (pattern-match reactive)) (slot subject (type INSTANCE) (pattern-match reactive)) (slot predicate (type INSTANCE) (pattern-match reactive)) (slot time (type ?VARIABLE) (pattern-match reactive)))

(deffunction add-prop (?s ?p ?t ?r)
       (bind ?count 0)
       (do-for-all-instances ((?prop Proposition))
                          (and (eq ?prop:subject ?s)
                               (eq ?prop:predicate ?p)
                               (or (and (eq (class ?t) Duration)
                                        (= (send (send ?prop get-time) get-start) (send ?t get-start))
                                        (= (send (send ?prop get-time) get-end) (send ?t get-end)))
                                   (= ?prop:time ?t))
                               (= ?prop:truth ?r))
               (bind ?count (+ ?count 1)))
        (if (= ?count 0)
        then (make-instance of Proposition (subject ?s)
                                           (predicate ?p)
                                           (time ?t)
                                           (truth ?r))
        else (return TRUE)))
(defclass Person (is-a Thing))
(defclass Can (is-a State) (slot what (type INSTANCE) (visibility public) (pattern-match reactive)))
(defclass Wants (is-a State) (slot to (type INSTANCE) (visibility public) (pattern-match reactive)))
(defclass Has (is-a State) (slot what (type INSTANCE) (visibility public) (pattern-match reactive)))
(defclass IsNeeded (is-a State) (slot for_action (type INSTANCE) (visibility public) (pattern-match reactive)))
(defclass IsIn (is-a State) (slot what (type INSTANCE) (visibility public) (pattern-match reactive)))
(defclass Group (is-a Thing))
(defclass Permission (is-a Thing))
(defclass Role (is-a Thing))
(defclass Content (is-a Thing))
(defclass Create (is-a State) (slot what (type INSTANCE) (visibility public) (pattern-match reactive)))
(defclass IsOwner (is-a State) (slot of (type INSTANCE) (visibility public) (pattern-match reactive)))
(defclass Status (is-a Thing))
(defclass View (is-a State) (slot what (type INSTANCE) (visibility public) (pattern-match reactive)))
(defclass Publish (is-a State) (slot what (type INSTANCE) (visibility public) (pattern-match reactive)))
(defclass Hide (is-a State) (slot what (type INSTANCE) (visibility public) (pattern-match reactive)))
(reduce-class [admin] Person)
(reduce-class [member] Role)
(reduce-class [manager] Role)
(reduce-class [basic_perm] Permission)
(reduce-class [manage_perm] Permission)
(reduce-class [create_perm] Permission)
(reduce-class [public] Status)
(reduce-class [private] Status)
(add-prop [admin] (add-pred Has what [manager]) (make-instance of Duration (start 733701.0) (end -1.0)) 1)
(add-prop [member] (add-pred Has what [basic_perm]) (make-instance of Duration (start 733701.0) (end -1.0)) 1)
(defrule 30002c48f216464a9848a7c8dcfcf95c (logical (object (is-a Proposition) (subject ?X1&:(or (eq (class ?X1) Person) (subclassp (class ?X1) Person))) (predicate ?Y1&:(or (eq (class ?Y1) Wants) (subclassp (class ?Y1) Wants))&:(or (eq (class (send ?Y1 get-to)) Create) (subclassp (class (send ?Y1 get-to)) Create))&:(or (eq (class (send (send ?Y1 get-to) get-what)) Thing) (subclassp (class (send (send ?Y1 get-to) get-what)) Thing))) (time ?X2) (truth 1))) (logical (object (is-a Proposition) (subject ?X1) (predicate ?Y2&:(or (eq (class ?Y2) Has) (subclassp (class ?Y2) Has))&:(eq (send ?Y2 get-what) [create_perm])) (time ?X3&:(or (eq (class ?X3) Duration) (subclassp (class ?X3) Duration))) (truth 1))) (test (and (<= (send ?X3 get-start) ?X2) (or (= (send ?X3 get-end) -1) (>= (send ?X3 get-end) ?X2)))) => (add-prop ?X1 (add-pred Create what (send (send ?Y1 get-to) get-what)) ?X2 1))
(defrule 913f54745b24462aba00bfcb641aa5d0 (logical (object (is-a Proposition) (subject ?X1&:(or (eq (class ?X1) Person) (subclassp (class ?X1) Person))) (predicate ?Y4&:(or (eq (class ?Y4) Wants) (subclassp (class ?Y4) Wants))) (time ?X2) (truth 1))) (logical (object (is-a Proposition) (subject ?X1) (predicate ?Y5&:(or (eq (class ?Y5) Can) (subclassp (class ?Y5) Can))&:(eq (send ?Y4 get-to) (send ?Y5 get-what))) (time ?X3&:(or (eq (class ?X3) Duration) (subclassp (class ?X3) Duration))) (truth 1))) (test (and (<= (send ?X3 get-start) ?X2) (or (= (send ?X3 get-end) -1) (>= (send ?X3 get-end) ?X2)))) => (add-prop ?X1 (send ?Y4 get-to) ?X2 1))
(defrule c139397e74c04439ad4f9d688589192b (logical (object (is-a Proposition) (subject ?X2&:(or (eq (class ?X2) Thing) (subclassp (class ?X2) Thing))) (predicate ?Y7&:(or (eq (class ?Y7) IsNeeded) (subclassp (class ?Y7) IsNeeded))) (time ?X3&:(or (eq (class ?X3) Duration) (subclassp (class ?X3) Duration))) (truth 1))) (logical (object (is-a Proposition) (subject ?X1&:(or (eq (class ?X1) Thing) (subclassp (class ?X1) Thing))) (predicate ?Y9&:(or (eq (class ?Y9) Has) (subclassp (class ?Y9) Has))&:(eq (send ?Y9 get-what) ?X2)) (time ?X5&:(or (eq (class ?X5) Duration) (subclassp (class ?X5) Duration))) (truth 1))) 
                (test (or (and (> (mincomstart ?X3 ?X5) -1)
                               (<= (mincomstart ?X3 ?X5) (maxcomend ?X3 ?X5)))
                          (= (maxcomend ?X3 ?X5) -1))
                )
                 => (add-prop ?X1 (add-pred Can what (send ?Y7 get-for_action)) (make-instance of Duration (start (mincomstart ?X3 ?X5)) (end (maxcomend ?X3 ?X5))) 1))
(defrule b174a208b3e443919c3d630b0447b590 (logical (object (is-a Proposition) (subject ?X1&:(or (eq (class ?X1) Thing) (subclassp (class ?X1) Thing))) (predicate ?Y11&:(or (eq (class ?Y11) IsIn) (subclassp (class ?Y11) IsIn))&:(or (eq (class (send ?Y11 get-what)) Thing) (subclassp (class (send ?Y11 get-what)) Thing))) (time ?X4&:(or (eq (class ?X4) Duration) (subclassp (class ?X4) Duration))) (truth 1))) (logical (object (is-a Proposition) (subject ?X2&:(eq ?X2 (send ?Y11 get-what))) (predicate ?Y13&:(or (eq (class ?Y13) IsIn) (subclassp (class ?Y13) IsIn))&:(or (eq (class (send ?Y13 get-what)) Thing) (subclassp (class (send ?Y13 get-what)) Thing))) (time ?X5&:(or (eq (class ?X5) Duration) (subclassp (class ?X5) Duration))) (truth 1))) 
                (test (or (and (> (mincomstart ?X4 ?X5) -1)
                               (<= (mincomstart ?X4 ?X5) (maxcomend ?X4 ?X5)))
                          (= (maxcomend ?X4 ?X5) -1))
                )
                 => (add-prop ?X1 (add-pred IsIn what (send ?Y13 get-what)) (make-instance of Duration (start (mincomstart ?X4 ?X5)) (end (maxcomend ?X4 ?X5))) 1))
(defrule 6d35fbcf681c4da1952c1010d63399ee (logical (object (is-a Proposition) (subject ?X1&:(or (eq (class ?X1) Person) (subclassp (class ?X1) Person))) (predicate ?Y15&:(or (eq (class ?Y15) IsIn) (subclassp (class ?Y15) IsIn))) (time ?X3&:(or (eq (class ?X3) Duration) (subclassp (class ?X3) Duration))) (truth 1))) (logical (object (is-a Proposition) (subject ?X2&:(or (eq (class ?X2) Group) (subclassp (class ?X2) Group))) (predicate ?Y17&:(or (eq (class ?Y17) Has) (subclassp (class ?Y17) Has))&:(or (eq (class (send ?Y17 get-what)) Permission) (subclassp (class (send ?Y17 get-what)) Permission))) (time ?X5&:(or (eq (class ?X5) Duration) (subclassp (class ?X5) Duration))) (truth 1))) 
                (test (or (and (> (mincomstart ?X3 ?X5) -1)
                               (<= (mincomstart ?X3 ?X5) (maxcomend ?X3 ?X5)))
                          (= (maxcomend ?X3 ?X5) -1))
                )
                 => (add-prop ?X1 (add-pred Has what (send ?Y17 get-what)) (make-instance of Duration (start (mincomstart ?X3 ?X5)) (end (maxcomend ?X3 ?X5))) 1))
(defrule aafb51677fee4dcc9b64e0483159ce6e (logical (object (is-a Proposition) (subject ?X1&:(or (eq (class ?X1) Person) (subclassp (class ?X1) Person))) (predicate ?Y19&:(or (eq (class ?Y19) Has) (subclassp (class ?Y19) Has))&:(or (eq (class (send ?Y19 get-what)) Role) (subclassp (class (send ?Y19 get-what)) Role))) (time ?X3&:(or (eq (class ?X3) Duration) (subclassp (class ?X3) Duration))) (truth 1))) (logical (object (is-a Proposition) (subject ?X2&:(eq ?X2 (send ?Y19 get-what))) (predicate ?Y21&:(or (eq (class ?Y21) Has) (subclassp (class ?Y21) Has))&:(or (eq (class (send ?Y21 get-what)) Permission) (subclassp (class (send ?Y21 get-what)) Permission))) (time ?X5&:(or (eq (class ?X5) Duration) (subclassp (class ?X5) Duration))) (truth 1))) 
                (test (or (and (> (mincomstart ?X3 ?X5) -1)
                               (<= (mincomstart ?X3 ?X5) (maxcomend ?X3 ?X5)))
                          (= (maxcomend ?X3 ?X5) -1))
                )
                 => (add-prop ?X1 (add-pred Has what (send ?Y21 get-what)) (make-instance of Duration (start (mincomstart ?X3 ?X5)) (end (maxcomend ?X3 ?X5))) 1))
(defrule 2ed8763494344943a315f7b208da7ee8 (logical (object (is-a Person) (name ?X1))) => (add-prop ?X1 (add-pred Has what [member]) (make-instance of Duration (start 733701.0) (end -1.0)) 1))
(defrule 2444c433c5534ae1b54a390c0f307306 (logical (object (is-a Proposition) (subject ?X1&:(or (eq (class ?X1) Person) (subclassp (class ?X1) Person))) (predicate ?Y23&:(or (eq (class ?Y23) Create) (subclassp (class ?Y23) Create))&:(or (eq (class (send ?Y23 get-what)) Content) (subclassp (class (send ?Y23 get-what)) Content))) (time ?X3) (truth 1))) => (reduce-class (send ?Y23 get-what) Content) (add-prop ?X1 (add-pred IsOwner of (send ?Y23 get-what)) (make-instance of Duration (start ?X3) (end -1.0)) 1) (add-prop (send ?Y23 get-what) (add-pred Has what [private]) (make-instance of Duration (start ?X3) (end -1.0)) 1))
(defrule d906dd1cccb3401aadfa3fd98a8846b5 (logical (object (is-a Permission) (name ?X2))) => (add-prop [manager] (add-pred Has what ?X2) (make-instance of Duration (start 733701.0) (end -1.0)) 1))
(defrule 3ca2f1ed4c3b40e7bd65259a67606926 (logical (object (is-a Proposition) (subject ?X1&:(or (eq (class ?X1) Content) (subclassp (class ?X1) Content))) (predicate ?Y24&:(or (eq (class ?Y24) Has) (subclassp (class ?Y24) Has))&:(eq (send ?Y24 get-what) [public])) (time ?X2&:(or (eq (class ?X2) Duration) (subclassp (class ?X2) Duration))) (truth 1))) => (add-prop [basic_perm] (add-pred IsNeeded for_action (add-pred View what ?X1)) ?X2 1))
(defrule 5324ef8dd6c24f6c91da415f8522847a (logical (object (is-a Proposition) (subject ?X1&:(or (eq (class ?X1) Content) (subclassp (class ?X1) Content))) (predicate ?Y26&:(or (eq (class ?Y26) Has) (subclassp (class ?Y26) Has))&:(eq (send ?Y26 get-what) [private])) (time ?X2&:(or (eq (class ?X2) Duration) (subclassp (class ?X2) Duration))) (truth 1))) => (add-prop [manage_perm] (add-pred IsNeeded for_action (add-pred View what ?X1)) ?X2 1))
(defrule 406f80d187454bc49288988aee9ea770 (logical (object (is-a Proposition) (subject ?X1&:(or (eq (class ?X1) Content) (subclassp (class ?X1) Content))) (predicate ?Y28&:(or (eq (class ?Y28) Has) (subclassp (class ?Y28) Has))&:(eq (send ?Y28 get-what) [private])) (time ?X3&:(or (eq (class ?X3) Duration) (subclassp (class ?X3) Duration))) (truth 1))) (logical (object (is-a Proposition) (subject ?X2&:(or (eq (class ?X2) Person) (subclassp (class ?X2) Person))) (predicate ?Y30&:(or (eq (class ?Y30) IsOwner) (subclassp (class ?Y30) IsOwner))&:(eq (send ?Y30 get-of) ?X1)) (time ?X4&:(or (eq (class ?X4) Duration) (subclassp (class ?X4) Duration))) (truth 1))) 
                (test (or (and (> (mincomstart ?X3 ?X4) -1)
                               (<= (mincomstart ?X3 ?X4) (maxcomend ?X3 ?X4)))
                          (= (maxcomend ?X3 ?X4) -1))
                )
                 => (add-prop ?X2 (add-pred Can what (add-pred View what ?X1)) (make-instance of Duration (start (mincomstart ?X3 ?X4)) (end (maxcomend ?X3 ?X4))) 1))
(defrule 866df1849b9546b19ef923dfe413275c (logical (object (is-a Proposition) (subject ?X1&:(or (eq (class ?X1) Person) (subclassp (class ?X1) Person))) (predicate ?Y32&:(or (eq (class ?Y32) Publish) (subclassp (class ?Y32) Publish))&:(or (eq (class (send ?Y32 get-what)) Content) (subclassp (class (send ?Y32 get-what)) Content))) (time ?X3) (truth 1))) (logical (object (is-a Proposition) (subject ?X2&:(eq ?X2 (send ?Y32 get-what))) (predicate ?Y33&:(or (eq (class ?Y33) Has) (subclassp (class ?Y33) Has))&:(or (eq (class (send ?Y33 get-what)) Status) (subclassp (class (send ?Y33 get-what)) Status))) (time ?X5&:(or (eq (class ?X5) Duration) (subclassp (class ?X5) Duration))) (truth 1))) => (send ?X5 put-end 733701) (add-prop (send ?Y32 get-what) (add-pred Has what [public]) (make-instance of Duration (start ?X3) (end -1.0)) 1))
(defrule b748dc6b2f0b472dadacd14c277061b8 (logical (object (is-a Content) (name ?X1))) => (add-prop [manage_perm] (add-pred IsNeeded for_action (add-pred Publish what ?X1)) (make-instance of Duration (start 733701.0) (end -1.0)) 1))
(defrule 62a996167da54c6eb4e213dedf43ac58 (logical (object (is-a Proposition) (subject ?X1&:(or (eq (class ?X1) Person) (subclassp (class ?X1) Person))) (predicate ?Y35&:(or (eq (class ?Y35) Hide) (subclassp (class ?Y35) Hide))&:(or (eq (class (send ?Y35 get-what)) Content) (subclassp (class (send ?Y35 get-what)) Content))) (time ?X3) (truth 1))) (logical (object (is-a Proposition) (subject ?X2&:(eq ?X2 (send ?Y35 get-what))) (predicate ?Y36&:(or (eq (class ?Y36) Has) (subclassp (class ?Y36) Has))&:(or (eq (class (send ?Y36 get-what)) Status) (subclassp (class (send ?Y36 get-what)) Status))) (time ?X5&:(or (eq (class ?X5) Duration) (subclassp (class ?X5) Duration))) (truth 1))) => (send ?X5 put-end 733701) (add-prop (send ?Y35 get-what) (add-pred Has what [private]) (make-instance of Duration (start ?X3) (end -1.0)) 1))
(defrule a26bcf894fbf4f748fb5aa4fc4b94eb4 (logical (object (is-a Proposition) (subject ?X1&:(or (eq (class ?X1) Person) (subclassp (class ?X1) Person))) (predicate ?Y38&:(or (eq (class ?Y38) IsOwner) (subclassp (class ?Y38) IsOwner))&:(or (eq (class (send ?Y38 get-of)) Content) (subclassp (class (send ?Y38 get-of)) Content))) (time ?X3&:(or (eq (class ?X3) Duration) (subclassp (class ?X3) Duration))) (truth 1))) => (add-prop ?X1 (add-pred Can what (add-pred Hide what (send ?Y38 get-of))) ?X3 1))
(reduce-class [john] Person)
(reduce-class [pete] Person)
(reduce-class [jane] Person)
(reduce-class [c1] Content)
(reduce-class [c2] Content)
(add-prop [john] (add-pred Has what [manager]) (make-instance of Duration (start 733701.0) (end -1.0)) 1)
(add-prop [jane] (add-pred Has what [create_perm]) (make-instance of Duration (start 733701.0) (end -1.0)) 1)
(add-prop [jane] (add-pred Wants to (add-pred Create what [c1])) 733701.0 1)
(add-prop [pete] (add-pred Wants to (add-pred Create what [c2])) 733701.0 1)
(find-all-instances ((?prop Proposition) (?Y40 IsOwner) (?Y41 Duration)) (and (eq ?prop:subject [jane]) (eq ?Y40:of [c1]) (eq ?prop:predicate ?Y40) (= ?Y41:start 733701.0) (= ?Y41:end -1.0) (eq ?prop:truth 1)))
0


no
----------running---------------------
----------runned: 33---------------------
(find-all-instances ((?prop Proposition) (?Y42 IsOwner) (?Y43 Duration)) (and (eq ?prop:subject [jane]) (eq ?Y42:of [c1]) (eq ?prop:predicate ?Y42) (= ?Y43:start 733701.0) (= ?Y43:end -1.0) (eq ?prop:truth 1)))
35


jane isowner of c1 at from 733701.0 till -1.0
(find-all-instances ((?prop Proposition) (?Y44 Has) (?Y45 Duration)) (and (eq ?prop:subject [c1]) (eq ?Y44:what [private]) (eq ?prop:predicate ?Y44) (= ?Y45:start 733701.0) (= ?Y45:end -1.0) (eq ?prop:truth 1)))
35


c1 has what private at from 733701.0 till -1.0
(find-all-instances ((?prop Proposition) (?Y46 IsOwner) (?Y47 Duration)) (and (eq ?prop:subject [pete]) (eq ?Y46:of [c2]) (eq ?prop:predicate ?Y46) (= ?Y47:start 733701.0) (= ?Y47:end -1.0) (eq ?prop:truth 1)))
0


no
(add-prop [jane] (add-pred Wants to (add-pred Publish what [c1])) 733701.0 1)
(add-prop [pete] (add-pred Wants to (add-pred Publish what [c2])) 733701.0 1)
----------running---------------------
----------runned: 0---------------------
(find-all-instances ((?prop Proposition) (?Y48 Has) (?Y49 Duration)) (and (eq ?prop:subject [c1]) (eq ?Y48:what [public]) (eq ?prop:predicate ?Y48) (= ?Y49:start 733701.0) (= ?Y49:end -1.0) (eq ?prop:truth 1)))
0


no
(find-all-instances ((?prop Proposition) (?Y50 Has) (?Y51 Duration)) (and (eq ?prop:subject [c2]) (eq ?Y50:what [public]) (eq ?prop:predicate ?Y50) (= ?Y51:start 733701.0) (= ?Y51:end -1.0) (eq ?prop:truth 1)))
0


no
(find-all-instances ((?prop Proposition) (?Y52 Can) (?Y53 View) (?Y54 Duration)) (and (eq ?prop:subject [jane]) (eq ?Y53:what [c1]) (eq ?Y52:what ?Y53) (eq ?prop:predicate ?Y52) (= ?Y54:start 733701.0) (= ?Y54:end -1.0) (eq ?prop:truth 1)))
35


jane can what view what c1 at from 733701.0 till -1.0
(find-all-instances ((?prop Proposition) (?Y55 Can) (?Y56 View) (?Y57 Duration)) (and (eq ?prop:subject [pete]) (eq ?Y56:what [c1]) (eq ?Y55:what ?Y56) (eq ?prop:predicate ?Y55) (= ?Y57:start 733701.0) (= ?Y57:end -1.0) (eq ?prop:truth 1)))
0


no
(add-prop [john] (add-pred Wants to (add-pred Publish what [c1])) 733701.0 1)
----------running---------------------
----------runned: 10---------------------
(find-all-instances ((?prop Proposition) (?Y58 Has) (?Y59 Duration)) (and (eq ?prop:subject [c1]) (eq ?Y58:what [private]) (eq ?prop:predicate ?Y58) (= ?Y59:start 733701.0) (= ?Y59:end -1.0) (eq ?prop:truth 1)))
0


no
(find-all-instances ((?prop Proposition) (?Y60 Has) (?Y61 Duration)) (and (eq ?prop:subject [c1]) (eq ?Y60:what [public]) (eq ?prop:predicate ?Y60) (= ?Y61:start 733701.0) (= ?Y61:end -1.0) (eq ?prop:truth 1)))
41


c1 has what public at from 733701.0 till -1.0
(find-all-instances ((?prop Proposition) (?Y62 Can) (?Y63 View) (?Y64 Duration)) (and (eq ?prop:subject [pete]) (eq ?Y63:what [c1]) (eq ?Y62:what ?Y63) (eq ?prop:predicate ?Y62) (= ?Y64:start 733701.0) (= ?Y64:end -1.0) (eq ?prop:truth 1)))
41


pete can what view what c1 at from 733701.0 till -1.0
(defclass Name (is-a USER))

(deffunction reduce-class (?instance ?class)
    (if (eq (length$
                (find-all-instances ((?a ?class))(eq (instance-name ?a) ?instance)))
             0)
    then (make-instance ?instance of ?class)
    else (return TRUE)))
(defclass Thing (is-a Name))
(defclass Verb (is-a USER))
(defclass State (is-a Verb) )
(set-sequence-operator-recognition TRUE)
(defmessage-handler State set-slots primary ($?slots)
        (while (> (length$ ?slots) 0) do
            (bind ?slot (first$ ?slots))
            (bind ?vslots (rest$ ?slots))
            (bind ?value (first$ ?vslots))
            (bind ?slots (rest$ ?vslots))
            (dynamic-put $?slot $?value))
        (return (instance-name ?self)))


(deffunction add-pred (?class $?slots)
        (bind ?key (str-cat ?class $?slots))
        (bind ?pos (str-index "." ?key))
        (while ?pos do
            (bind ?key (str-cat (sub-string 1 (- ?pos 1) ?key)
                                "_"
                                (sub-string (+ ?pos 1) (str-length ?key) ?key)))
            (bind ?pos (str-index "." ?key)))
        (bind ?key (sym-cat ?key))
        (if (instance-existp ?key) then
            (return (instance-name ?key))
         else
            (make-instance ?key of ?class)
            (return (send (instance-name ?key) set-slots $?slots))))

(defclass Duration (is-a Name) (slot start (type NUMBER) (pattern-match reactive)) (slot end (type NUMBER) (pattern-match reactive)))

(deffunction extract-now ($?instants)
    (if (= (length$ ?instants) 0) then
        (return -1)
    else
        (bind ?count 0)
        (bind ?not-now (create$))
        (while (> (length$ ?instants) 0) do
            (bind ?instant (nth$ 1 ?instants))
            (bind ?instants (rest$ ?instants))
            (if (= ?instant -1)
            then (bind ?count (+ ?count 1))
            else (bind ?not-now (insert$ ?not-now 1 ?instant)))
            )
        (return (insert$ ?not-now 1 ?count))
    )
)

(defclass Name (is-a USER))

(deffunction reduce-class (?instance ?class)
    (if (eq (length$
                (find-all-instances ((?a ?class))(eq (instance-name ?a) ?instance)))
             0)
    then (make-instance ?instance of ?class)
    else (return TRUE)))
(defclass Thing (is-a Name))
(defclass Verb (is-a USER))
(defclass State (is-a Verb) )
(set-sequence-operator-recognition TRUE)
(defmessage-handler State set-slots primary ($?slots)
        (while (> (length$ ?slots) 0) do
            (bind ?slot (first$ ?slots))
            (bind ?vslots (rest$ ?slots))
            (bind ?value (first$ ?vslots))
            (bind ?slots (rest$ ?vslots))
            (dynamic-put $?slot $?value))
        (return (instance-name ?self)))


(deffunction add-pred (?class $?slots)
        (bind ?key (str-cat ?class $?slots))
        (bind ?pos (str-index "." ?key))
        (while ?pos do
            (bind ?key (str-cat (sub-string 1 (- ?pos 1) ?key)
                                "_"
                                (sub-string (+ ?pos 1) (str-length ?key) ?key)))
            (bind ?pos (str-index "." ?key)))
        (bind ?key (sym-cat ?key))
        (if (instance-existp ?key) then
            (return (instance-name ?key))
         else
            (make-instance ?key of ?class)
            (return (send (instance-name ?key) set-slots $?slots))))

(defclass Duration (is-a Name) (slot start (type NUMBER) (pattern-match reactive)) (slot end (type NUMBER) (pattern-match reactive)))

(deffunction extract-now ($?instants)
    (if (= (length$ ?instants) 0) then
        (return -1)
    else
        (bind ?count 0)
        (bind ?not-now (create$))
        (while (> (length$ ?instants) 0) do
            (bind ?instant (nth$ 1 ?instants))
            (bind ?instants (rest$ ?instants))
            (if (= ?instant -1)
            then (bind ?count (+ ?count 1))
            else (bind ?not-now (insert$ ?not-now 1 ?instant)))
            )
        (return (insert$ ?not-now 1 ?count))
    )
)


(deffunction maxcomend ($?durations)
    (bind ?ends (create$))
    (while (> (length$ ?durations) 0)
        (bind ?ends (insert$ ?ends 1 (send (nth$ 1 ?durations) get-end)))
        (bind ?durations (rest$ ?durations))
        )
    (bind ?ret (extract-now ?ends))
    (bind ?pasts (rest$ ?ret))
    (if (> (length$ ?pasts) 0)
    then (return (min (expand$ ?pasts)))
    else (return -1)
    )
)


(deffunction mincomstart ($?durations)
    (bind ?starts (create$))
    (while (> (length$ ?durations) 0)
        (bind ?starts (insert$ ?starts 1 (send (nth$ 1 ?durations) get-start)))
        (bind ?durations (rest$ ?durations))
        )
    (bind ?ret (extract-now ?starts))
    (bind ?nows (nth$ 1 ?ret))
    (if (> ?nows 0)
    then (return -1)
    else (bind ?ret (rest$ ?ret))
         (return (max (expand$ ?ret)))
    )
)

(defclass Proposition (is-a Name) (slot truth (type INTEGER) (default 1) (pattern-match reactive)) (slot subject (type INSTANCE) (pattern-match reactive)) (slot predicate (type INSTANCE) (pattern-match reactive)) (slot time (type ?VARIABLE) (pattern-match reactive)))

(deffunction add-prop (?s ?p ?t ?r)
       (bind ?count 0)
       (do-for-all-instances ((?prop Proposition))
                          (and (eq ?prop:subject ?s)
                               (eq ?prop:predicate ?p)
                               (or (and (eq (class ?t) Duration)
                                        (= (send (send ?prop get-time) get-start) (send ?t get-start))
                                        (= (send (send ?prop get-time) get-end) (send ?t get-end)))
                                   (= ?prop:time ?t))
                               (= ?prop:truth ?r))
               (bind ?count (+ ?count 1)))
        (if (= ?count 0)
        then (make-instance of Proposition (subject ?s)
                                           (predicate ?p)
                                           (time ?t)
                                           (truth ?r))
        else (return TRUE)))
(defclass Person (is-a Thing))
(defclass Can (is-a State) (slot what (type INSTANCE) (visibility public) (pattern-match reactive)))
(defclass Wants (is-a State) (slot to (type INSTANCE) (visibility public) (pattern-match reactive)))
(defclass Has (is-a State) (slot what (type INSTANCE) (visibility public) (pattern-match reactive)))
(defclass IsNeeded (is-a State) (slot for_action (type INSTANCE) (visibility public) (pattern-match reactive)))
(defclass IsIn (is-a State) (slot what (type INSTANCE) (visibility public) (pattern-match reactive)))
(defclass Group (is-a Thing))
(defclass Permission (is-a Thing))
(defclass Role (is-a Thing))
(defclass Content (is-a Thing))
(defclass Create (is-a State) (slot what (type INSTANCE) (visibility public) (pattern-match reactive)))
(defclass IsOwner (is-a State) (slot of (type INSTANCE) (visibility public) (pattern-match reactive)))
(defclass Status (is-a Thing))
(defclass View (is-a State) (slot what (type INSTANCE) (visibility public) (pattern-match reactive)))
(defclass Publish (is-a State) (slot what (type INSTANCE) (visibility public) (pattern-match reactive)))
(defclass Hide (is-a State) (slot what (type INSTANCE) (visibility public) (pattern-match reactive)))
(reduce-class [admin] Person)
(reduce-class [member] Role)
(reduce-class [manager] Role)
(reduce-class [basic_perm] Permission)
(reduce-class [manage_perm] Permission)
(reduce-class [create_perm] Permission)
(reduce-class [public] Status)
(reduce-class [private] Status)
(add-prop [admin] (add-pred Has what [manager]) (make-instance of Duration (start 733702.0) (end -1.0)) 1)
(add-prop [member] (add-pred Has what [basic_perm]) (make-instance of Duration (start 733702.0) (end -1.0)) 1)
(defrule 73aa6e0c2b784b38afd0a84c00bed045 (logical (object (is-a Proposition) (subject ?X1&:(or (eq (class ?X1) Person) (subclassp (class ?X1) Person))) (predicate ?Y1&:(or (eq (class ?Y1) Wants) (subclassp (class ?Y1) Wants))&:(or (eq (class (send ?Y1 get-to)) Create) (subclassp (class (send ?Y1 get-to)) Create))&:(or (eq (class (send (send ?Y1 get-to) get-what)) Thing) (subclassp (class (send (send ?Y1 get-to) get-what)) Thing))) (time ?X2) (truth 1))) (logical (object (is-a Proposition) (subject ?X1) (predicate ?Y2&:(or (eq (class ?Y2) Has) (subclassp (class ?Y2) Has))&:(eq (send ?Y2 get-what) [create_perm])) (time ?X3&:(or (eq (class ?X3) Duration) (subclassp (class ?X3) Duration))) (truth 1))) (test (and (<= (send ?X3 get-start) ?X2) (or (= (send ?X3 get-end) -1) (>= (send ?X3 get-end) ?X2)))) => (add-prop ?X1 (add-pred Create what (send (send ?Y1 get-to) get-what)) ?X2 1))
(defrule 93dac4f5eedf48c5aedda090ce6c1303 (logical (object (is-a Proposition) (subject ?X1&:(or (eq (class ?X1) Person) (subclassp (class ?X1) Person))) (predicate ?Y4&:(or (eq (class ?Y4) Wants) (subclassp (class ?Y4) Wants))) (time ?X2) (truth 1))) (logical (object (is-a Proposition) (subject ?X1) (predicate ?Y5&:(or (eq (class ?Y5) Can) (subclassp (class ?Y5) Can))&:(eq (send ?Y4 get-to) (send ?Y5 get-what))) (time ?X3&:(or (eq (class ?X3) Duration) (subclassp (class ?X3) Duration))) (truth 1))) (test (and (<= (send ?X3 get-start) ?X2) (or (= (send ?X3 get-end) -1) (>= (send ?X3 get-end) ?X2)))) => (add-prop ?X1 (send ?Y4 get-to) ?X2 1))
(defrule 8fcc0081f60c4e7a8004000bcf4c3d8f (logical (object (is-a Proposition) (subject ?X2&:(or (eq (class ?X2) Thing) (subclassp (class ?X2) Thing))) (predicate ?Y7&:(or (eq (class ?Y7) IsNeeded) (subclassp (class ?Y7) IsNeeded))) (time ?X3&:(or (eq (class ?X3) Duration) (subclassp (class ?X3) Duration))) (truth 1))) (logical (object (is-a Proposition) (subject ?X1&:(or (eq (class ?X1) Thing) (subclassp (class ?X1) Thing))) (predicate ?Y9&:(or (eq (class ?Y9) Has) (subclassp (class ?Y9) Has))&:(eq (send ?Y9 get-what) ?X2)) (time ?X5&:(or (eq (class ?X5) Duration) (subclassp (class ?X5) Duration))) (truth 1))) 
                (test (or (and (> (mincomstart ?X3 ?X5) -1)
                               (<= (mincomstart ?X3 ?X5) (maxcomend ?X3 ?X5)))
                          (= (maxcomend ?X3 ?X5) -1))
                )
                 => (add-prop ?X1 (add-pred Can what (send ?Y7 get-for_action)) (make-instance of Duration (start (mincomstart ?X3 ?X5)) (end (maxcomend ?X3 ?X5))) 1))
(defrule ff28bd2ea2f54564a05ea901ead580e1 (logical (object (is-a Proposition) (subject ?X1&:(or (eq (class ?X1) Thing) (subclassp (class ?X1) Thing))) (predicate ?Y11&:(or (eq (class ?Y11) IsIn) (subclassp (class ?Y11) IsIn))&:(or (eq (class (send ?Y11 get-what)) Thing) (subclassp (class (send ?Y11 get-what)) Thing))) (time ?X4&:(or (eq (class ?X4) Duration) (subclassp (class ?X4) Duration))) (truth 1))) (logical (object (is-a Proposition) (subject ?X2&:(eq ?X2 (send ?Y11 get-what))) (predicate ?Y13&:(or (eq (class ?Y13) IsIn) (subclassp (class ?Y13) IsIn))&:(or (eq (class (send ?Y13 get-what)) Thing) (subclassp (class (send ?Y13 get-what)) Thing))) (time ?X5&:(or (eq (class ?X5) Duration) (subclassp (class ?X5) Duration))) (truth 1))) 
                (test (or (and (> (mincomstart ?X4 ?X5) -1)
                               (<= (mincomstart ?X4 ?X5) (maxcomend ?X4 ?X5)))
                          (= (maxcomend ?X4 ?X5) -1))
                )
                 => (add-prop ?X1 (add-pred IsIn what (send ?Y13 get-what)) (make-instance of Duration (start (mincomstart ?X4 ?X5)) (end (maxcomend ?X4 ?X5))) 1))
(defrule 8ae8df77dc074ad8aaf86d82906e8084 (logical (object (is-a Proposition) (subject ?X1&:(or (eq (class ?X1) Person) (subclassp (class ?X1) Person))) (predicate ?Y15&:(or (eq (class ?Y15) IsIn) (subclassp (class ?Y15) IsIn))) (time ?X3&:(or (eq (class ?X3) Duration) (subclassp (class ?X3) Duration))) (truth 1))) (logical (object (is-a Proposition) (subject ?X2&:(or (eq (class ?X2) Group) (subclassp (class ?X2) Group))) (predicate ?Y17&:(or (eq (class ?Y17) Has) (subclassp (class ?Y17) Has))&:(or (eq (class (send ?Y17 get-what)) Permission) (subclassp (class (send ?Y17 get-what)) Permission))) (time ?X5&:(or (eq (class ?X5) Duration) (subclassp (class ?X5) Duration))) (truth 1))) 
                (test (or (and (> (mincomstart ?X3 ?X5) -1)
                               (<= (mincomstart ?X3 ?X5) (maxcomend ?X3 ?X5)))
                          (= (maxcomend ?X3 ?X5) -1))
                )
                 => (add-prop ?X1 (add-pred Has what (send ?Y17 get-what)) (make-instance of Duration (start (mincomstart ?X3 ?X5)) (end (maxcomend ?X3 ?X5))) 1))
(defrule 4631860e2d8f438d8514dedc32ad0591 (logical (object (is-a Proposition) (subject ?X1&:(or (eq (class ?X1) Person) (subclassp (class ?X1) Person))) (predicate ?Y19&:(or (eq (class ?Y19) Has) (subclassp (class ?Y19) Has))&:(or (eq (class (send ?Y19 get-what)) Role) (subclassp (class (send ?Y19 get-what)) Role))) (time ?X3&:(or (eq (class ?X3) Duration) (subclassp (class ?X3) Duration))) (truth 1))) (logical (object (is-a Proposition) (subject ?X2&:(eq ?X2 (send ?Y19 get-what))) (predicate ?Y21&:(or (eq (class ?Y21) Has) (subclassp (class ?Y21) Has))&:(or (eq (class (send ?Y21 get-what)) Permission) (subclassp (class (send ?Y21 get-what)) Permission))) (time ?X5&:(or (eq (class ?X5) Duration) (subclassp (class ?X5) Duration))) (truth 1))) 
                (test (or (and (> (mincomstart ?X3 ?X5) -1)
                               (<= (mincomstart ?X3 ?X5) (maxcomend ?X3 ?X5)))
                          (= (maxcomend ?X3 ?X5) -1))
                )
                 => (add-prop ?X1 (add-pred Has what (send ?Y21 get-what)) (make-instance of Duration (start (mincomstart ?X3 ?X5)) (end (maxcomend ?X3 ?X5))) 1))
(defrule b0565e8e070e4510a38f06743e59ba00 (logical (object (is-a Person) (name ?X1))) => (add-prop ?X1 (add-pred Has what [member]) (make-instance of Duration (start 733702.0) (end -1.0)) 1))
(defrule 6ca211344af34c3cae1e22d5bb92b021 (logical (object (is-a Proposition) (subject ?X1&:(or (eq (class ?X1) Person) (subclassp (class ?X1) Person))) (predicate ?Y23&:(or (eq (class ?Y23) Create) (subclassp (class ?Y23) Create))&:(or (eq (class (send ?Y23 get-what)) Content) (subclassp (class (send ?Y23 get-what)) Content))) (time ?X3) (truth 1))) => (reduce-class (send ?Y23 get-what) Content) (add-prop ?X1 (add-pred IsOwner of (send ?Y23 get-what)) (make-instance of Duration (start ?X3) (end -1.0)) 1) (add-prop (send ?Y23 get-what) (add-pred Has what [private]) (make-instance of Duration (start ?X3) (end -1.0)) 1))
(defrule d546fc60aa834813b1d53313564e1c31 (logical (object (is-a Permission) (name ?X2))) => (add-prop [manager] (add-pred Has what ?X2) (make-instance of Duration (start 733702.0) (end -1.0)) 1))
(defrule 689af711a9ce431dabc17c384e9d620e (logical (object (is-a Proposition) (subject ?X1&:(or (eq (class ?X1) Content) (subclassp (class ?X1) Content))) (predicate ?Y24&:(or (eq (class ?Y24) Has) (subclassp (class ?Y24) Has))&:(eq (send ?Y24 get-what) [public])) (time ?X2&:(or (eq (class ?X2) Duration) (subclassp (class ?X2) Duration))) (truth 1))) => (add-prop [basic_perm] (add-pred IsNeeded for_action (add-pred View what ?X1)) ?X2 1))
(defrule 6b444017630945318151eedc4e1a789d (logical (object (is-a Proposition) (subject ?X1&:(or (eq (class ?X1) Content) (subclassp (class ?X1) Content))) (predicate ?Y26&:(or (eq (class ?Y26) Has) (subclassp (class ?Y26) Has))&:(eq (send ?Y26 get-what) [private])) (time ?X2&:(or (eq (class ?X2) Duration) (subclassp (class ?X2) Duration))) (truth 1))) => (add-prop [manage_perm] (add-pred IsNeeded for_action (add-pred View what ?X1)) ?X2 1))
(defrule f93e89df07eb4e5081c2ef593d6a7758 (logical (object (is-a Proposition) (subject ?X1&:(or (eq (class ?X1) Content) (subclassp (class ?X1) Content))) (predicate ?Y28&:(or (eq (class ?Y28) Has) (subclassp (class ?Y28) Has))&:(eq (send ?Y28 get-what) [private])) (time ?X3&:(or (eq (class ?X3) Duration) (subclassp (class ?X3) Duration))) (truth 1))) (logical (object (is-a Proposition) (subject ?X2&:(or (eq (class ?X2) Person) (subclassp (class ?X2) Person))) (predicate ?Y30&:(or (eq (class ?Y30) IsOwner) (subclassp (class ?Y30) IsOwner))&:(eq (send ?Y30 get-of) ?X1)) (time ?X4&:(or (eq (class ?X4) Duration) (subclassp (class ?X4) Duration))) (truth 1))) 
                (test (or (and (> (mincomstart ?X3 ?X4) -1)
                               (<= (mincomstart ?X3 ?X4) (maxcomend ?X3 ?X4)))
                          (= (maxcomend ?X3 ?X4) -1))
                )
                 => (add-prop ?X2 (add-pred Can what (add-pred View what ?X1)) (make-instance of Duration (start (mincomstart ?X3 ?X4)) (end (maxcomend ?X3 ?X4))) 1))
(defrule b4c3cd54cc7e4d31978c4f48a4f6b22f (logical (object (is-a Proposition) (subject ?X1&:(or (eq (class ?X1) Person) (subclassp (class ?X1) Person))) (predicate ?Y32&:(or (eq (class ?Y32) Publish) (subclassp (class ?Y32) Publish))&:(or (eq (class (send ?Y32 get-what)) Content) (subclassp (class (send ?Y32 get-what)) Content))) (time ?X3) (truth 1))) (logical (object (is-a Proposition) (subject ?X2&:(eq ?X2 (send ?Y32 get-what))) (predicate ?Y33&:(or (eq (class ?Y33) Has) (subclassp (class ?Y33) Has))&:(or (eq (class (send ?Y33 get-what)) Status) (subclassp (class (send ?Y33 get-what)) Status))) (time ?X5&:(or (eq (class ?X5) Duration) (subclassp (class ?X5) Duration))) (truth 1))) => (send ?X5 put-end 733702) (add-prop (send ?Y32 get-what) (add-pred Has what [public]) (make-instance of Duration (start ?X3) (end -1.0)) 1))
(defrule 8f860cf104394bc59829178ac031aff3 (logical (object (is-a Content) (name ?X1))) => (add-prop [manage_perm] (add-pred IsNeeded for_action (add-pred Publish what ?X1)) (make-instance of Duration (start 733702.0) (end -1.0)) 1))
(defrule 3da73adee7274578b780c79b40b5de8c (logical (object (is-a Proposition) (subject ?X1&:(or (eq (class ?X1) Person) (subclassp (class ?X1) Person))) (predicate ?Y35&:(or (eq (class ?Y35) Hide) (subclassp (class ?Y35) Hide))&:(or (eq (class (send ?Y35 get-what)) Content) (subclassp (class (send ?Y35 get-what)) Content))) (time ?X3) (truth 1))) (logical (object (is-a Proposition) (subject ?X2&:(eq ?X2 (send ?Y35 get-what))) (predicate ?Y36&:(or (eq (class ?Y36) Has) (subclassp (class ?Y36) Has))&:(or (eq (class (send ?Y36 get-what)) Status) (subclassp (class (send ?Y36 get-what)) Status))) (time ?X5&:(or (eq (class ?X5) Duration) (subclassp (class ?X5) Duration))) (truth 1))) => (send ?X5 put-end 733702) (add-prop (send ?Y35 get-what) (add-pred Has what [private]) (make-instance of Duration (start ?X3) (end -1.0)) 1))
(defrule 1ed6d9083c8e4393a235379c90d3370f (logical (object (is-a Proposition) (subject ?X1&:(or (eq (class ?X1) Person) (subclassp (class ?X1) Person))) (predicate ?Y38&:(or (eq (class ?Y38) IsOwner) (subclassp (class ?Y38) IsOwner))&:(or (eq (class (send ?Y38 get-of)) Content) (subclassp (class (send ?Y38 get-of)) Content))) (time ?X3&:(or (eq (class ?X3) Duration) (subclassp (class ?X3) Duration))) (truth 1))) => (add-prop ?X1 (add-pred Can what (add-pred Hide what (send ?Y38 get-of))) ?X3 1))
(reduce-class [john] Person)
(reduce-class [pete] Person)
(reduce-class [jane] Person)
(reduce-class [c1] Content)
(reduce-class [c2] Content)
(add-prop [john] (add-pred Has what [manager]) (make-instance of Duration (start 733702.0) (end -1.0)) 1)
(add-prop [jane] (add-pred Has what [create_perm]) (make-instance of Duration (start 733702.0) (end -1.0)) 1)
(add-prop [jane] (add-pred Wants to (add-pred Create what [c1])) 733702.0 1)
(add-prop [pete] (add-pred Wants to (add-pred Create what [c2])) 733702.0 1)
(find-all-instances ((?prop Proposition) (?Y40 IsOwner) (?Y41 Duration)) (and (eq ?prop:subject [jane]) (eq ?Y40:of [c1]) (eq ?prop:predicate ?Y40) (= ?Y41:start 733702.0) (= ?Y41:end -1.0) (eq ?prop:truth 1)))
0


no
----------running---------------------
----------runned: 33---------------------
(find-all-instances ((?prop Proposition) (?Y42 IsOwner) (?Y43 Duration)) (and (eq ?prop:subject [jane]) (eq ?Y42:of [c1]) (eq ?prop:predicate ?Y42) (= ?Y43:start 733702.0) (= ?Y43:end -1.0) (eq ?prop:truth 1)))
35


jane isowner of c1 at from 733702.0 till -1.0
(find-all-instances ((?prop Proposition) (?Y44 Has) (?Y45 Duration)) (and (eq ?prop:subject [c1]) (eq ?Y44:what [private]) (eq ?prop:predicate ?Y44) (= ?Y45:start 733702.0) (= ?Y45:end -1.0) (eq ?prop:truth 1)))
35


c1 has what private at from 733702.0 till -1.0
(find-all-instances ((?prop Proposition) (?Y46 IsOwner) (?Y47 Duration)) (and (eq ?prop:subject [pete]) (eq ?Y46:of [c2]) (eq ?prop:predicate ?Y46) (= ?Y47:start 733702.0) (= ?Y47:end -1.0) (eq ?prop:truth 1)))
0


no
(add-prop [jane] (add-pred Wants to (add-pred Publish what [c1])) 733702.0 1)
(add-prop [pete] (add-pred Wants to (add-pred Publish what [c2])) 733702.0 1)
----------running---------------------
----------runned: 0---------------------
(find-all-instances ((?prop Proposition) (?Y48 Has) (?Y49 Duration)) (and (eq ?prop:subject [c1]) (eq ?Y48:what [public]) (eq ?prop:predicate ?Y48) (= ?Y49:start 733702.0) (= ?Y49:end -1.0) (eq ?prop:truth 1)))
0


no
(find-all-instances ((?prop Proposition) (?Y50 Has) (?Y51 Duration)) (and (eq ?prop:subject [c2]) (eq ?Y50:what [public]) (eq ?prop:predicate ?Y50) (= ?Y51:start 733702.0) (= ?Y51:end -1.0) (eq ?prop:truth 1)))
0


no
(find-all-instances ((?prop Proposition) (?Y52 Can) (?Y53 View) (?Y54 Duration)) (and (eq ?prop:subject [jane]) (eq ?Y53:what [c1]) (eq ?Y52:what ?Y53) (eq ?prop:predicate ?Y52) (= ?Y54:start 733702.0) (= ?Y54:end -1.0) (eq ?prop:truth 1)))
35


jane can what view what c1 at from 733702.0 till -1.0
(find-all-instances ((?prop Proposition) (?Y55 Can) (?Y56 View) (?Y57 Duration)) (and (eq ?prop:subject [pete]) (eq ?Y56:what [c1]) (eq ?Y55:what ?Y56) (eq ?prop:predicate ?Y55) (= ?Y57:start 733702.0) (= ?Y57:end -1.0) (eq ?prop:truth 1)))
0


no
(add-prop [john] (add-pred Wants to (add-pred Publish what [c1])) 733702.0 1)
----------running---------------------
----------runned: 10---------------------
(find-all-instances ((?prop Proposition) (?Y58 Has) (?Y59 Duration)) (and (eq ?prop:subject [c1]) (eq ?Y58:what [private]) (eq ?prop:predicate ?Y58) (= ?Y59:start 733702.0) (= ?Y59:end -1.0) (eq ?prop:truth 1)))
0


no
(find-all-instances ((?prop Proposition) (?Y60 Has) (?Y61 Duration)) (and (eq ?prop:subject [c1]) (eq ?Y60:what [public]) (eq ?prop:predicate ?Y60) (= ?Y61:start 733702.0) (= ?Y61:end -1.0) (eq ?prop:truth 1)))
41


c1 has what public at from 733702.0 till -1.0
(find-all-instances ((?prop Proposition) (?Y62 Can) (?Y63 View) (?Y64 Duration)) (and (eq ?prop:subject [pete]) (eq ?Y63:what [c1]) (eq ?Y62:what ?Y63) (eq ?prop:predicate ?Y62) (= ?Y64:start 733702.0) (= ?Y64:end -1.0) (eq ?prop:truth 1)))
41


pete can what view what c1 at from 733702.0 till -1.0
(defclass Name (is-a USER))

(deffunction reduce-class (?instance ?class)
    (if (eq (length$
                (find-all-instances ((?a ?class))(eq (instance-name ?a) ?instance)))
             0)
    then (make-instance ?instance of ?class)
    else (return TRUE)))
(defclass Thing (is-a Name))
(defclass Verb (is-a USER))
(defclass State (is-a Verb) )
(set-sequence-operator-recognition TRUE)
(defmessage-handler State set-slots primary ($?slots)
        (while (> (length$ ?slots) 0) do
            (bind ?slot (first$ ?slots))
            (bind ?vslots (rest$ ?slots))
            (bind ?value (first$ ?vslots))
            (bind ?slots (rest$ ?vslots))
            (dynamic-put $?slot $?value))
        (return (instance-name ?self)))


(deffunction add-pred (?class $?slots)
        (bind ?key (str-cat ?class $?slots))
        (bind ?pos (str-index "." ?key))
        (while ?pos do
            (bind ?key (str-cat (sub-string 1 (- ?pos 1) ?key)
                                "_"
                                (sub-string (+ ?pos 1) (str-length ?key) ?key)))
            (bind ?pos (str-index "." ?key)))
        (bind ?key (sym-cat ?key))
        (if (instance-existp ?key) then
            (return (instance-name ?key))
         else
            (make-instance ?key of ?class)
            (return (send (instance-name ?key) set-slots $?slots))))

(defclass Duration (is-a Name) (slot start (type NUMBER) (pattern-match reactive)) (slot end (type NUMBER) (pattern-match reactive)))

(deffunction extract-now ($?instants)
    (if (= (length$ ?instants) 0) then
        (return -1)
    else
        (bind ?count 0)
        (bind ?not-now (create$))
        (while (> (length$ ?instants) 0) do
            (bind ?instant (nth$ 1 ?instants))
            (bind ?instants (rest$ ?instants))
            (if (= ?instant -1)
            then (bind ?count (+ ?count 1))
            else (bind ?not-now (insert$ ?not-now 1 ?instant)))
            )
        (return (insert$ ?not-now 1 ?count))
    )
)


(deffunction maxcomend ($?durations)
    (bind ?ends (create$))
    (while (> (length$ ?durations) 0)
        (bind ?ends (insert$ ?ends 1 (send (nth$ 1 ?durations) get-end)))
        (bind ?durations (rest$ ?durations))
        )
    (bind ?ret (extract-now ?ends))
    (bind ?pasts (rest$ ?ret))
    (if (> (length$ ?pasts) 0)
    then (return (min (expand$ ?pasts)))
    else (return -1)
    )
)


(deffunction mincomstart ($?durations)
    (bind ?starts (create$))
    (while (> (length$ ?durations) 0)
        (bind ?starts (insert$ ?starts 1 (send (nth$ 1 ?durations) get-start)))
        (bind ?durations (rest$ ?durations))
        )
    (bind ?ret (extract-now ?starts))
    (bind ?nows (nth$ 1 ?ret))
    (if (> ?nows 0)
    then (return -1)
    else (bind ?ret (rest$ ?ret))
         (return (max (expand$ ?ret)))
    )
)

(defclass Proposition (is-a Name) (slot truth (type INTEGER) (default 1) (pattern-match reactive)) (slot subject (type INSTANCE) (pattern-match reactive)) (slot predicate (type INSTANCE) (pattern-match reactive)) (slot time (type ?VARIABLE) (pattern-match reactive)))

(deffunction add-prop (?s ?p ?t ?r)
       (bind ?count 0)
       (do-for-all-instances ((?prop Proposition))
                          (and (eq ?prop:subject ?s)
                               (eq ?prop:predicate ?p)
                               (or (and (eq (class ?t) Duration)
                                        (= (send (send ?prop get-time) get-start) (send ?t get-start))
                                        (= (send (send ?prop get-time) get-end) (send ?t get-end)))
                                   (= ?prop:time ?t))
                               (= ?prop:truth ?r))
               (bind ?count (+ ?count 1)))
        (if (= ?count 0)
        then (make-instance of Proposition (subject ?s)
                                           (predicate ?p)
                                           (time ?t)
                                           (truth ?r))
        else (return TRUE)))
(defclass Person (is-a Thing))
(defclass Can (is-a State) (slot what (type INSTANCE) (visibility public) (pattern-match reactive)))
(defclass Wants (is-a State) (slot to (type INSTANCE) (visibility public) (pattern-match reactive)))
(defclass Has (is-a State) (slot what (type INSTANCE) (visibility public) (pattern-match reactive)))
(defclass IsNeeded (is-a State) (slot for_action (type INSTANCE) (visibility public) (pattern-match reactive)))
(defclass IsIn (is-a State) (slot what (type INSTANCE) (visibility public) (pattern-match reactive)))
(defclass Group (is-a Thing))
(defclass Permission (is-a Thing))
(defclass Role (is-a Thing))
(defclass Content (is-a Thing))
(defclass Create (is-a State) (slot what (type INSTANCE) (visibility public) (pattern-match reactive)))
(defclass IsOwner (is-a State) (slot of (type INSTANCE) (visibility public) (pattern-match reactive)))
(defclass Status (is-a Thing))
(defclass View (is-a State) (slot what (type INSTANCE) (visibility public) (pattern-match reactive)))
(defclass Publish (is-a State) (slot what (type INSTANCE) (visibility public) (pattern-match reactive)))
(defclass Hide (is-a State) (slot what (type INSTANCE) (visibility public) (pattern-match reactive)))
(reduce-class [admin] Person)
(reduce-class [member] Role)
(reduce-class [manager] Role)
(reduce-class [basic_perm] Permission)
(reduce-class [manage_perm] Permission)
(reduce-class [create_perm] Permission)
(reduce-class [public] Status)
(reduce-class [private] Status)
(add-prop [admin] (add-pred Has what [manager]) (make-instance of Duration (start 733702.0) (end -1.0)) 1)
(add-prop [member] (add-pred Has what [basic_perm]) (make-instance of Duration (start 733702.0) (end -1.0)) 1)
(defrule a54bc4b88a1e498e861b0cf6beddf4e6 (logical (object (is-a Proposition) (subject ?X1&:(or (eq (class ?X1) Person) (subclassp (class ?X1) Person))) (predicate ?Y1&:(or (eq (class ?Y1) Wants) (subclassp (class ?Y1) Wants))&:(or (eq (class (send ?Y1 get-to)) Create) (subclassp (class (send ?Y1 get-to)) Create))&:(or (eq (class (send (send ?Y1 get-to) get-what)) Thing) (subclassp (class (send (send ?Y1 get-to) get-what)) Thing))) (time ?X2) (truth 1))) (logical (object (is-a Proposition) (subject ?X1) (predicate ?Y2&:(or (eq (class ?Y2) Has) (subclassp (class ?Y2) Has))&:(eq (send ?Y2 get-what) [create_perm])) (time ?X3&:(or (eq (class ?X3) Duration) (subclassp (class ?X3) Duration))) (truth 1))) (test (and (<= (send ?X3 get-start) ?X2) (or (= (send ?X3 get-end) -1) (>= (send ?X3 get-end) ?X2)))) => (add-prop ?X1 (add-pred Create what (send (send ?Y1 get-to) get-what)) ?X2 1))
(defrule f7148bc836ae45eaaca758554dbf04e6 (logical (object (is-a Proposition) (subject ?X1&:(or (eq (class ?X1) Person) (subclassp (class ?X1) Person))) (predicate ?Y4&:(or (eq (class ?Y4) Wants) (subclassp (class ?Y4) Wants))) (time ?X2) (truth 1))) (logical (object (is-a Proposition) (subject ?X1) (predicate ?Y5&:(or (eq (class ?Y5) Can) (subclassp (class ?Y5) Can))&:(eq (send ?Y4 get-to) (send ?Y5 get-what))) (time ?X3&:(or (eq (class ?X3) Duration) (subclassp (class ?X3) Duration))) (truth 1))) (test (and (<= (send ?X3 get-start) ?X2) (or (= (send ?X3 get-end) -1) (>= (send ?X3 get-end) ?X2)))) => (add-prop ?X1 (send ?Y4 get-to) ?X2 1))
(defrule 311df07d122349929cb0dd936084699f (logical (object (is-a Proposition) (subject ?X2&:(or (eq (class ?X2) Thing) (subclassp (class ?X2) Thing))) (predicate ?Y7&:(or (eq (class ?Y7) IsNeeded) (subclassp (class ?Y7) IsNeeded))) (time ?X3&:(or (eq (class ?X3) Duration) (subclassp (class ?X3) Duration))) (truth 1))) (logical (object (is-a Proposition) (subject ?X1&:(or (eq (class ?X1) Thing) (subclassp (class ?X1) Thing))) (predicate ?Y9&:(or (eq (class ?Y9) Has) (subclassp (class ?Y9) Has))&:(eq (send ?Y9 get-what) ?X2)) (time ?X5&:(or (eq (class ?X5) Duration) (subclassp (class ?X5) Duration))) (truth 1))) 
                (test (or (and (> (mincomstart ?X3 ?X5) -1)
                               (<= (mincomstart ?X3 ?X5) (maxcomend ?X3 ?X5)))
                          (= (maxcomend ?X3 ?X5) -1))
                )
                 => (add-prop ?X1 (add-pred Can what (send ?Y7 get-for_action)) (make-instance of Duration (start (mincomstart ?X3 ?X5)) (end (maxcomend ?X3 ?X5))) 1))
(defrule 4556617c102d4a338ffa37daea3601ce (logical (object (is-a Proposition) (subject ?X1&:(or (eq (class ?X1) Thing) (subclassp (class ?X1) Thing))) (predicate ?Y11&:(or (eq (class ?Y11) IsIn) (subclassp (class ?Y11) IsIn))&:(or (eq (class (send ?Y11 get-what)) Thing) (subclassp (class (send ?Y11 get-what)) Thing))) (time ?X4&:(or (eq (class ?X4) Duration) (subclassp (class ?X4) Duration))) (truth 1))) (logical (object (is-a Proposition) (subject ?X2&:(eq ?X2 (send ?Y11 get-what))) (predicate ?Y13&:(or (eq (class ?Y13) IsIn) (subclassp (class ?Y13) IsIn))&:(or (eq (class (send ?Y13 get-what)) Thing) (subclassp (class (send ?Y13 get-what)) Thing))) (time ?X5&:(or (eq (class ?X5) Duration) (subclassp (class ?X5) Duration))) (truth 1))) 
                (test (or (and (> (mincomstart ?X4 ?X5) -1)
                               (<= (mincomstart ?X4 ?X5) (maxcomend ?X4 ?X5)))
                          (= (maxcomend ?X4 ?X5) -1))
                )
                 => (add-prop ?X1 (add-pred IsIn what (send ?Y13 get-what)) (make-instance of Duration (start (mincomstart ?X4 ?X5)) (end (maxcomend ?X4 ?X5))) 1))
(defrule ef33a41d7cab4e9a862759c6b49d6cdc (logical (object (is-a Proposition) (subject ?X1&:(or (eq (class ?X1) Person) (subclassp (class ?X1) Person))) (predicate ?Y15&:(or (eq (class ?Y15) IsIn) (subclassp (class ?Y15) IsIn))) (time ?X3&:(or (eq (class ?X3) Duration) (subclassp (class ?X3) Duration))) (truth 1))) (logical (object (is-a Proposition) (subject ?X2&:(or (eq (class ?X2) Group) (subclassp (class ?X2) Group))) (predicate ?Y17&:(or (eq (class ?Y17) Has) (subclassp (class ?Y17) Has))&:(or (eq (class (send ?Y17 get-what)) Permission) (subclassp (class (send ?Y17 get-what)) Permission))) (time ?X5&:(or (eq (class ?X5) Duration) (subclassp (class ?X5) Duration))) (truth 1))) 
                (test (or (and (> (mincomstart ?X3 ?X5) -1)
                               (<= (mincomstart ?X3 ?X5) (maxcomend ?X3 ?X5)))
                          (= (maxcomend ?X3 ?X5) -1))
                )
                 => (add-prop ?X1 (add-pred Has what (send ?Y17 get-what)) (make-instance of Duration (start (mincomstart ?X3 ?X5)) (end (maxcomend ?X3 ?X5))) 1))
(defrule 6d68836c9d5644d896f5e15ff9ec4e5f (logical (object (is-a Proposition) (subject ?X1&:(or (eq (class ?X1) Person) (subclassp (class ?X1) Person))) (predicate ?Y19&:(or (eq (class ?Y19) Has) (subclassp (class ?Y19) Has))&:(or (eq (class (send ?Y19 get-what)) Role) (subclassp (class (send ?Y19 get-what)) Role))) (time ?X3&:(or (eq (class ?X3) Duration) (subclassp (class ?X3) Duration))) (truth 1))) (logical (object (is-a Proposition) (subject ?X2&:(eq ?X2 (send ?Y19 get-what))) (predicate ?Y21&:(or (eq (class ?Y21) Has) (subclassp (class ?Y21) Has))&:(or (eq (class (send ?Y21 get-what)) Permission) (subclassp (class (send ?Y21 get-what)) Permission))) (time ?X5&:(or (eq (class ?X5) Duration) (subclassp (class ?X5) Duration))) (truth 1))) 
                (test (or (and (> (mincomstart ?X3 ?X5) -1)
                               (<= (mincomstart ?X3 ?X5) (maxcomend ?X3 ?X5)))
                          (= (maxcomend ?X3 ?X5) -1))
                )
                 => (add-prop ?X1 (add-pred Has what (send ?Y21 get-what)) (make-instance of Duration (start (mincomstart ?X3 ?X5)) (end (maxcomend ?X3 ?X5))) 1))
(defrule 9c2a874c37eb40989e5c0021e83de0cf (logical (object (is-a Person) (name ?X1))) => (add-prop ?X1 (add-pred Has what [member]) (make-instance of Duration (start 733702.0) (end -1.0)) 1))
(defrule 5417c1e4e86949d99c54187d2ef06b75 (logical (object (is-a Proposition) (subject ?X1&:(or (eq (class ?X1) Person) (subclassp (class ?X1) Person))) (predicate ?Y23&:(or (eq (class ?Y23) Create) (subclassp (class ?Y23) Create))&:(or (eq (class (send ?Y23 get-what)) Content) (subclassp (class (send ?Y23 get-what)) Content))) (time ?X3) (truth 1))) => (reduce-class (send ?Y23 get-what) Content) (add-prop ?X1 (add-pred IsOwner of (send ?Y23 get-what)) (make-instance of Duration (start ?X3) (end -1.0)) 1) (add-prop (send ?Y23 get-what) (add-pred Has what [private]) (make-instance of Duration (start ?X3) (end -1.0)) 1))
(defrule f4899f39a4f8438d9c0f6a0052c77d25 (logical (object (is-a Permission) (name ?X2))) => (add-prop [manager] (add-pred Has what ?X2) (make-instance of Duration (start 733702.0) (end -1.0)) 1))
(defrule 01b2d0bf8a874b58adc133b9432e4954 (logical (object (is-a Proposition) (subject ?X1&:(or (eq (class ?X1) Content) (subclassp (class ?X1) Content))) (predicate ?Y24&:(or (eq (class ?Y24) Has) (subclassp (class ?Y24) Has))&:(eq (send ?Y24 get-what) [public])) (time ?X2&:(or (eq (class ?X2) Duration) (subclassp (class ?X2) Duration))) (truth 1))) => (add-prop [basic_perm] (add-pred IsNeeded for_action (add-pred View what ?X1)) ?X2 1))
(defrule 50c4967bf89743d4a7852eb566ad32ff (logical (object (is-a Proposition) (subject ?X1&:(or (eq (class ?X1) Content) (subclassp (class ?X1) Content))) (predicate ?Y26&:(or (eq (class ?Y26) Has) (subclassp (class ?Y26) Has))&:(eq (send ?Y26 get-what) [private])) (time ?X2&:(or (eq (class ?X2) Duration) (subclassp (class ?X2) Duration))) (truth 1))) => (add-prop [manage_perm] (add-pred IsNeeded for_action (add-pred View what ?X1)) ?X2 1))
(defrule ddabbd972e6f4ffe865406cbbf2c7ce1 (logical (object (is-a Proposition) (subject ?X1&:(or (eq (class ?X1) Content) (subclassp (class ?X1) Content))) (predicate ?Y28&:(or (eq (class ?Y28) Has) (subclassp (class ?Y28) Has))&:(eq (send ?Y28 get-what) [private])) (time ?X3&:(or (eq (class ?X3) Duration) (subclassp (class ?X3) Duration))) (truth 1))) (logical (object (is-a Proposition) (subject ?X2&:(or (eq (class ?X2) Person) (subclassp (class ?X2) Person))) (predicate ?Y30&:(or (eq (class ?Y30) IsOwner) (subclassp (class ?Y30) IsOwner))&:(eq (send ?Y30 get-of) ?X1)) (time ?X4&:(or (eq (class ?X4) Duration) (subclassp (class ?X4) Duration))) (truth 1))) 
                (test (or (and (> (mincomstart ?X3 ?X4) -1)
                               (<= (mincomstart ?X3 ?X4) (maxcomend ?X3 ?X4)))
                          (= (maxcomend ?X3 ?X4) -1))
                )
                 => (add-prop ?X2 (add-pred Can what (add-pred View what ?X1)) (make-instance of Duration (start (mincomstart ?X3 ?X4)) (end (maxcomend ?X3 ?X4))) 1))
(defrule 7921cb2de6124fdda53d73841bb2f326 (logical (object (is-a Proposition) (subject ?X1&:(or (eq (class ?X1) Person) (subclassp (class ?X1) Person))) (predicate ?Y32&:(or (eq (class ?Y32) Publish) (subclassp (class ?Y32) Publish))&:(or (eq (class (send ?Y32 get-what)) Content) (subclassp (class (send ?Y32 get-what)) Content))) (time ?X3) (truth 1))) (logical (object (is-a Proposition) (subject ?X2&:(eq ?X2 (send ?Y32 get-what))) (predicate ?Y33&:(or (eq (class ?Y33) Has) (subclassp (class ?Y33) Has))&:(or (eq (class (send ?Y33 get-what)) Status) (subclassp (class (send ?Y33 get-what)) Status))) (time ?X5&:(or (eq (class ?X5) Duration) (subclassp (class ?X5) Duration))) (truth 1))) => (send ?X5 put-end 733702) (add-prop (send ?Y32 get-what) (add-pred Has what [public]) (make-instance of Duration (start ?X3) (end -1.0)) 1))
(defrule c53430b1d92e497089448b475320ee55 (logical (object (is-a Content) (name ?X1))) => (add-prop [manage_perm] (add-pred IsNeeded for_action (add-pred Publish what ?X1)) (make-instance of Duration (start 733702.0) (end -1.0)) 1))
(defrule c6da8d58ebcb47f0b96e151e3059b5f4 (logical (object (is-a Proposition) (subject ?X1&:(or (eq (class ?X1) Person) (subclassp (class ?X1) Person))) (predicate ?Y35&:(or (eq (class ?Y35) Hide) (subclassp (class ?Y35) Hide))&:(or (eq (class (send ?Y35 get-what)) Content) (subclassp (class (send ?Y35 get-what)) Content))) (time ?X3) (truth 1))) (logical (object (is-a Proposition) (subject ?X2&:(eq ?X2 (send ?Y35 get-what))) (predicate ?Y36&:(or (eq (class ?Y36) Has) (subclassp (class ?Y36) Has))&:(or (eq (class (send ?Y36 get-what)) Status) (subclassp (class (send ?Y36 get-what)) Status))) (time ?X5&:(or (eq (class ?X5) Duration) (subclassp (class ?X5) Duration))) (truth 1))) => (send ?X5 put-end 733702) (add-prop (send ?Y35 get-what) (add-pred Has what [private]) (make-instance of Duration (start ?X3) (end -1.0)) 1))
(defrule c7eb62a9024a4757ba291a20932799be (logical (object (is-a Proposition) (subject ?X1&:(or (eq (class ?X1) Person) (subclassp (class ?X1) Person))) (predicate ?Y38&:(or (eq (class ?Y38) IsOwner) (subclassp (class ?Y38) IsOwner))&:(or (eq (class (send ?Y38 get-of)) Content) (subclassp (class (send ?Y38 get-of)) Content))) (time ?X3&:(or (eq (class ?X3) Duration) (subclassp (class ?X3) Duration))) (truth 1))) => (add-prop ?X1 (add-pred Can what (add-pred Hide what (send ?Y38 get-of))) ?X3 1))
(reduce-class [john] Person)
(reduce-class [pete] Person)
(reduce-class [jane] Person)
(reduce-class [c1] Content)
(reduce-class [c2] Content)
(add-prop [john] (add-pred Has what [manager]) (make-instance of Duration (start 733702.0) (end -1.0)) 1)
(add-prop [jane] (add-pred Has what [create_perm]) (make-instance of Duration (start 733702.0) (end -1.0)) 1)
(add-prop [jane] (add-pred Wants to (add-pred Create what [c1])) 733702.0 1)
(add-prop [pete] (add-pred Wants to (add-pred Create what [c2])) 733702.0 1)
(find-all-instances ((?prop Proposition) (?Y40 IsOwner) (?Y41 Duration)) (and (eq ?prop:subject [jane]) (eq ?Y40:of [c1]) (eq ?prop:predicate ?Y40) (= ?Y41:start 733702.0) (= ?Y41:end -1.0) (eq ?prop:truth 1)))
0


no
----------running---------------------
----------runned: 33---------------------
(find-all-instances ((?prop Proposition) (?Y42 IsOwner) (?Y43 Duration)) (and (eq ?prop:subject [jane]) (eq ?Y42:of [c1]) (eq ?prop:predicate ?Y42) (= ?Y43:start 733702.0) (= ?Y43:end -1.0) (eq ?prop:truth 1)))
35


jane isowner of c1 at from 733702.0 till -1.0
(find-all-instances ((?prop Proposition) (?Y44 Has) (?Y45 Duration)) (and (eq ?prop:subject [c1]) (eq ?Y44:what [private]) (eq ?prop:predicate ?Y44) (= ?Y45:start 733702.0) (= ?Y45:end -1.0) (eq ?prop:truth 1)))
35


c1 has what private at from 733702.0 till -1.0
(find-all-instances ((?prop Proposition) (?Y46 IsOwner) (?Y47 Duration)) (and (eq ?prop:subject [pete]) (eq ?Y46:of [c2]) (eq ?prop:predicate ?Y46) (= ?Y47:start 733702.0) (= ?Y47:end -1.0) (eq ?prop:truth 1)))
0


no
(add-prop [jane] (add-pred Wants to (add-pred Publish what [c1])) 733702.0 1)
(add-prop [pete] (add-pred Wants to (add-pred Publish what [c2])) 733702.0 1)
----------running---------------------
----------runned: 0---------------------
(find-all-instances ((?prop Proposition) (?Y48 Has) (?Y49 Duration)) (and (eq ?prop:subject [c1]) (eq ?Y48:what [public]) (eq ?prop:predicate ?Y48) (= ?Y49:start 733702.0) (= ?Y49:end -1.0) (eq ?prop:truth 1)))
0


no
(find-all-instances ((?prop Proposition) (?Y50 Has) (?Y51 Duration)) (and (eq ?prop:subject [c2]) (eq ?Y50:what [public]) (eq ?prop:predicate ?Y50) (= ?Y51:start 733702.0) (= ?Y51:end -1.0) (eq ?prop:truth 1)))
0


no
(find-all-instances ((?prop Proposition) (?Y52 Can) (?Y53 View) (?Y54 Duration)) (and (eq ?prop:subject [jane]) (eq ?Y53:what [c1]) (eq ?Y52:what ?Y53) (eq ?prop:predicate ?Y52) (= ?Y54:start 733702.0) (= ?Y54:end -1.0) (eq ?prop:truth 1)))
35


jane can what view what c1 at from 733702.0 till -1.0
(find-all-instances ((?prop Proposition) (?Y55 Can) (?Y56 View) (?Y57 Duration)) (and (eq ?prop:subject [pete]) (eq ?Y56:what [c1]) (eq ?Y55:what ?Y56) (eq ?prop:predicate ?Y55) (= ?Y57:start 733702.0) (= ?Y57:end -1.0) (eq ?prop:truth 1)))
0


no
(add-prop [john] (add-pred Wants to (add-pred Publish what [c1])) 733702.0 1)
----------running---------------------
----------runned: 10---------------------
(find-all-instances ((?prop Proposition) (?Y58 Has) (?Y59 Duration)) (and (eq ?prop:subject [c1]) (eq ?Y58:what [private]) (eq ?prop:predicate ?Y58) (= ?Y59:start 733702.0) (= ?Y59:end -1.0) (eq ?prop:truth 1)))
0


no
(find-all-instances ((?prop Proposition) (?Y60 Has) (?Y61 Duration)) (and (eq ?prop:subject [c1]) (eq ?Y60:what [public]) (eq ?prop:predicate ?Y60) (= ?Y61:start 733702.0) (= ?Y61:end -1.0) (eq ?prop:truth 1)))
41


c1 has what public at from 733702.0 till -1.0
(find-all-instances ((?prop Proposition) (?Y62 Can) (?Y63 View) (?Y64 Duration)) (and (eq ?prop:subject [pete]) (eq ?Y63:what [c1]) (eq ?Y62:what ?Y63) (eq ?prop:predicate ?Y62) (= ?Y64:start 733702.0) (= ?Y64:end -1.0) (eq ?prop:truth 1)))
41


pete can what view what c1 at from 733702.0 till -1.0

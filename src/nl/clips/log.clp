(defclass Name (is-a USER))
(deffunction reduce-class (?instance ?class) (if (eq (length$ (find-instance ((?a ?class)) (eq (instance-name ?a) ?instance))) 0) then (make-instance ?instance of ?class) (python-call tonl ?class ?instance)))
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


(deffunction mincomstart (?dur1 ?dur2)
    (return (max (send ?dur1 get-start) (send ?dur2 get-start)))
)



(deffunction maxcomend (?dur1 ?dur2)
    (bind ?e1 (send ?dur1 get-end))
    (bind ?e2 (send ?dur2 get-end))
    (if (= ?e1 ?e2) then (return ?e1))
    (if (= ?e2 -1) then
        (return ?e1)
    )
    (if (= ?e1 -1) then
        (return ?e2)
    )
    (return (min ?e1 ?e2))
)

(defclass Proposition (is-a Name) (slot truth (type INTEGER) (default 1) (pattern-match reactive)) (slot subject (type INSTANCE) (pattern-match reactive)) (slot predicate (type INSTANCE) (pattern-match reactive)) (slot time (type ?VARIABLE) (pattern-match reactive)))

(deffunction add-prop (?s ?p ?t ?r)
       (if (and (python-call ptonl ?s ?p ?t ?r)
                (= (+ (length$ (find-instance ((?prop Proposition) (?dur Duration))
                          (and (eq ?prop:subject ?s)
                               (eq ?prop:predicate ?p)
                               (eq ?prop:time ?dur)
                               (= ?dur:start (send ?t get-start))
                               (= ?dur:end (send ?t get-end))
                               (eq ?prop:truth ?r))))
                      (length$ (find-instance ((?prop Proposition))
                          (and (eq ?prop:subject ?s)
                               (eq ?prop:predicate ?p)
                               (= ?prop:time ?t)
                               (eq ?prop:truth ?r)))))
                 0))
        then (make-instance of Proposition (subject ?s)
                                           (predicate ?p)
                                           (time ?t)
                                           (truth ?r))
        else (return TRUE)))
------------OPEN---------------------
----------F-OPEN---------------------
(defclass Person (is-a Thing))
(defclass Can (is-a State) (slot what (type INSTANCE) (visibility public) (pattern-match reactive)))
(defclass Wants (is-a State) (slot to (type INSTANCE) (visibility public) (pattern-match reactive)))
(defclass Has (is-a State) (slot what (type INSTANCE) (visibility public) (pattern-match reactive)))
(defclass IsNeeded (is-a State) (slot for_action (type INSTANCE) (visibility public) (pattern-match reactive)))
(defclass IsIn (is-a State) (slot what (type INSTANCE) (visibility public) (pattern-match reactive)))
(defclass Group (is-a Thing))
(defclass Permission (is-a Thing))
(defclass Role (is-a Person))
(defclass Content (is-a Thing))
(defclass Create (is-a State) (slot what (type INSTANCE) (visibility public) (pattern-match reactive)))
(defclass IsOwner (is-a State) (slot of (type INSTANCE) (visibility public) (pattern-match reactive)))
(defclass Status (is-a Thing))
(defclass View (is-a State) (slot what (type INSTANCE) (visibility public) (pattern-match reactive)))
(defclass Publish (is-a State) (slot what (type INSTANCE) (visibility public) (pattern-match reactive)))
(defclass Hide (is-a State) (slot what (type INSTANCE) (visibility public) (pattern-match reactive)))
(reduce-class [admin] Person)
admin
(reduce-class [member] Role)
member
(reduce-class [manager] Role)
manager
(reduce-class [basic_perm] Permission)
basic_perm
(reduce-class [manage_perm] Permission)
manage_perm
(reduce-class [create_perm] Permission)
create_perm
(reduce-class [public] Status)
public
(reduce-class [private] Status)
private
(add-prop [admin] (add-pred Has what [manager]) (make-instance of Duration (start 733697.0) (end -1.0)) 1)
admin has what manager at from 733697.0 till -1.0
(add-prop [member] (add-pred Has what [basic_perm]) (make-instance of Duration (start 733697.0) (end -1.0)) 1)
member has what basic_perm at from 733697.0 till -1.0
(defrule aa943330042e41c39325cd468e704bf3 (logical (object (is-a Proposition) (subject ?X1&:(or (eq (class ?X1) Person) (subclassp (class ?X1) Person))) (predicate ?Y1&:(or (eq (class ?Y1) Wants) (subclassp (class ?Y1) Wants))&:(or (eq (class (send ?Y1 get-to)) Create) (subclassp (class (send ?Y1 get-to)) Create))&:(or (eq (class (send (send ?Y1 get-to) get-what)) Thing) (subclassp (class (send (send ?Y1 get-to) get-what)) Thing))) (time ?X2) (truth 1))) (logical (object (is-a Proposition) (subject ?X1) (predicate ?Y2&:(or (eq (class ?Y2) Has) (subclassp (class ?Y2) Has))&:(eq (send ?Y2 get-what) [create_perm])) (time ?X3&:(or (eq (class ?X3) Duration) (subclassp (class ?X3) Duration))) (truth 1))) (test (and (<= (send ?X3 get-start) ?X2) (or (= (send ?X3 get-end) -1) (>= (send ?X3 get-end) ?X2)))) => (add-prop ?X1 (add-pred Create what (send (send ?Y1 get-to) get-what)) ?X2 1))
(defrule 70ef061a94274489902fdfd73e488eea (logical (object (is-a Proposition) (subject ?X1&:(or (eq (class ?X1) Person) (subclassp (class ?X1) Person))) (predicate ?Y4&:(or (eq (class ?Y4) Wants) (subclassp (class ?Y4) Wants))) (time ?X2) (truth 1))) (logical (object (is-a Proposition) (subject ?X1) (predicate ?Y5&:(or (eq (class ?Y5) Can) (subclassp (class ?Y5) Can))&:(eq (send ?Y4 get-to) (send ?Y5 get-what))) (time ?X3&:(or (eq (class ?X3) Duration) (subclassp (class ?X3) Duration))) (truth 1))) (test (and (<= (send ?X3 get-start) ?X2) (or (= (send ?X3 get-end) -1) (>= (send ?X3 get-end) ?X2)))) => (add-prop ?X1 (send ?Y4 get-to) ?X2 1))
(defrule 2eba80dffa5b4ee692500e421f66a0f8 (logical (object (is-a Proposition) (subject ?X2&:(or (eq (class ?X2) Thing) (subclassp (class ?X2) Thing))) (predicate ?Y7&:(or (eq (class ?Y7) IsNeeded) (subclassp (class ?Y7) IsNeeded))) (time ?X3&:(or (eq (class ?X3) Duration) (subclassp (class ?X3) Duration))) (truth 1))) (logical (object (is-a Proposition) (subject ?X1&:(or (eq (class ?X1) Thing) (subclassp (class ?X1) Thing))) (predicate ?Y9&:(or (eq (class ?Y9) Has) (subclassp (class ?Y9) Has))&:(eq (send ?Y9 get-what) ?X2)) (time ?X5&:(or (eq (class ?X5) Duration) (subclassp (class ?X5) Duration))) (truth 1))) (test (or (and (>= (send ?X3 get-start) (send ?X3 get-start)) (or (<= (send ?X3 get-start) (send ?X3 get-end)) (= (send ?X3 get-end) -1))) (and (>= (send ?X3 get-start) (send ?X3 get-start)) (or (<= (send ?X3 get-start) (send ?X3 get-end)) (= (send ?X3 get-end) -1))))) => (add-prop ?X1 (add-pred Can what (send ?Y7 get-for_action)) (make-instance of Duration (start (mincomstart ?X3 ?X5)) (end (maxcomend ?X3 ?X5))) 1))
(defrule 1f0c26096d2e4248a1c8d5f8cb80e873 (logical (object (is-a Proposition) (subject ?X1&:(or (eq (class ?X1) Thing) (subclassp (class ?X1) Thing))) (predicate ?Y11&:(or (eq (class ?Y11) IsIn) (subclassp (class ?Y11) IsIn))&:(or (eq (class (send ?Y11 get-what)) Thing) (subclassp (class (send ?Y11 get-what)) Thing))) (time ?X4&:(or (eq (class ?X4) Duration) (subclassp (class ?X4) Duration))) (truth 1))) (logical (object (is-a Proposition) (subject ?X2&:(eq ?X2 (send ?Y11 get-what))) (predicate ?Y13&:(or (eq (class ?Y13) IsIn) (subclassp (class ?Y13) IsIn))&:(or (eq (class (send ?Y13 get-what)) Thing) (subclassp (class (send ?Y13 get-what)) Thing))) (time ?X5&:(or (eq (class ?X5) Duration) (subclassp (class ?X5) Duration))) (truth 1))) (test (or (and (>= (send ?X4 get-start) (send ?X4 get-start)) (or (<= (send ?X4 get-start) (send ?X4 get-end)) (= (send ?X4 get-end) -1))) (and (>= (send ?X4 get-start) (send ?X4 get-start)) (or (<= (send ?X4 get-start) (send ?X4 get-end)) (= (send ?X4 get-end) -1))))) => (add-prop ?X1 (add-pred IsIn what (send ?Y13 get-what)) (make-instance of Duration (start (mincomstart ?X4 ?X5)) (end (maxcomend ?X4 ?X5))) 1))
(defrule 70b95e7b5ddb47bc87c5a57d878bf9e7 (logical (object (is-a Proposition) (subject ?X1&:(or (eq (class ?X1) Person) (subclassp (class ?X1) Person))) (predicate ?Y15&:(or (eq (class ?Y15) IsIn) (subclassp (class ?Y15) IsIn))) (time ?X3&:(or (eq (class ?X3) Duration) (subclassp (class ?X3) Duration))) (truth 1))) (logical (object (is-a Proposition) (subject ?X2&:(or (eq (class ?X2) Group) (subclassp (class ?X2) Group))) (predicate ?Y17&:(or (eq (class ?Y17) Has) (subclassp (class ?Y17) Has))&:(or (eq (class (send ?Y17 get-what)) Permission) (subclassp (class (send ?Y17 get-what)) Permission))) (time ?X5&:(or (eq (class ?X5) Duration) (subclassp (class ?X5) Duration))) (truth 1))) (test (or (and (>= (send ?X3 get-start) (send ?X3 get-start)) (or (<= (send ?X3 get-start) (send ?X3 get-end)) (= (send ?X3 get-end) -1))) (and (>= (send ?X3 get-start) (send ?X3 get-start)) (or (<= (send ?X3 get-start) (send ?X3 get-end)) (= (send ?X3 get-end) -1))))) => (add-prop ?X1 (add-pred Has what (send ?Y17 get-what)) (make-instance of Duration (start (mincomstart ?X3 ?X5)) (end (maxcomend ?X3 ?X5))) 1))
(defrule efb04afad47248ff8856308afb7598e3 (logical (object (is-a Proposition) (subject ?X1&:(or (eq (class ?X1) Person) (subclassp (class ?X1) Person))) (predicate ?Y19&:(or (eq (class ?Y19) Has) (subclassp (class ?Y19) Has))&:(or (eq (class (send ?Y19 get-what)) Role) (subclassp (class (send ?Y19 get-what)) Role))) (time ?X3&:(or (eq (class ?X3) Duration) (subclassp (class ?X3) Duration))) (truth 1))) (logical (object (is-a Proposition) (subject ?X2&:(eq ?X2 (send ?Y19 get-what))) (predicate ?Y21&:(or (eq (class ?Y21) Has) (subclassp (class ?Y21) Has))&:(or (eq (class (send ?Y21 get-what)) Permission) (subclassp (class (send ?Y21 get-what)) Permission))) (time ?X5&:(or (eq (class ?X5) Duration) (subclassp (class ?X5) Duration))) (truth 1))) (test (or (and (>= (send ?X3 get-start) (send ?X3 get-start)) (or (<= (send ?X3 get-start) (send ?X3 get-end)) (= (send ?X3 get-end) -1))) (and (>= (send ?X3 get-start) (send ?X3 get-start)) (or (<= (send ?X3 get-start) (send ?X3 get-end)) (= (send ?X3 get-end) -1))))) => (add-prop ?X1 (add-pred Has what (send ?Y21 get-what)) (make-instance of Duration (start (mincomstart ?X3 ?X5)) (end (maxcomend ?X3 ?X5))) 1))
(defrule 4733cee327c242a08ca9c7b752711206 (logical (object (is-a Person) (name ?X1))) => (add-prop ?X1 (add-pred Has what [member]) (make-instance of Duration (start 733697.0) (end -1.0)) 1))
(defrule 5cf295fddef9407f9a4f8775360aabeb (logical (object (is-a Proposition) (subject ?X1&:(or (eq (class ?X1) Person) (subclassp (class ?X1) Person))) (predicate ?Y23&:(or (eq (class ?Y23) Create) (subclassp (class ?Y23) Create))&:(or (eq (class (send ?Y23 get-what)) Content) (subclassp (class (send ?Y23 get-what)) Content))) (time ?X3) (truth 1))) => (reduce-class (send ?Y23 get-what) Content) (add-prop ?X1 (add-pred IsOwner of (send ?Y23 get-what)) (make-instance of Duration (start ?X3) (end -1.0)) 1) (add-prop (send ?Y23 get-what) (add-pred Has what [private]) (make-instance of Duration (start ?X3) (end -1.0)) 1))
(defrule 68418bdaa0634de5bc034ad4a38758da (logical (object (is-a Permission) (name ?X2))) => (add-prop [manager] (add-pred Has what ?X2) (make-instance of Duration (start 733697.0) (end -1.0)) 1))
(defrule a8a262fbf0f140159ee44cbe69cef505 (logical (object (is-a Proposition) (subject ?X1&:(or (eq (class ?X1) Content) (subclassp (class ?X1) Content))) (predicate ?Y24&:(or (eq (class ?Y24) Has) (subclassp (class ?Y24) Has))&:(eq (send ?Y24 get-what) [public])) (time ?X2&:(or (eq (class ?X2) Duration) (subclassp (class ?X2) Duration))) (truth 1))) => (add-prop [basic_perm] (add-pred IsNeeded for_action (add-pred View what ?X1)) ?X2 1))
(defrule b33b6de7d7c94a58ba69b63509ddc545 (logical (object (is-a Proposition) (subject ?X1&:(or (eq (class ?X1) Content) (subclassp (class ?X1) Content))) (predicate ?Y26&:(or (eq (class ?Y26) Has) (subclassp (class ?Y26) Has))&:(eq (send ?Y26 get-what) [private])) (time ?X2&:(or (eq (class ?X2) Duration) (subclassp (class ?X2) Duration))) (truth 1))) => (add-prop [manage_perm] (add-pred IsNeeded for_action (add-pred View what ?X1)) ?X2 1))
(defrule 2a4416938b454db88fc6112ee9106a57 (logical (object (is-a Proposition) (subject ?X1&:(or (eq (class ?X1) Content) (subclassp (class ?X1) Content))) (predicate ?Y28&:(or (eq (class ?Y28) Has) (subclassp (class ?Y28) Has))&:(eq (send ?Y28 get-what) [private])) (time ?X3&:(or (eq (class ?X3) Duration) (subclassp (class ?X3) Duration))) (truth 1))) (logical (object (is-a Proposition) (subject ?X2&:(or (eq (class ?X2) Person) (subclassp (class ?X2) Person))) (predicate ?Y30&:(or (eq (class ?Y30) IsOwner) (subclassp (class ?Y30) IsOwner))&:(eq (send ?Y30 get-of) ?X1)) (time ?X4&:(or (eq (class ?X4) Duration) (subclassp (class ?X4) Duration))) (truth 1))) (test (or (and (>= (send ?X3 get-start) (send ?X3 get-start)) (or (<= (send ?X3 get-start) (send ?X3 get-end)) (= (send ?X3 get-end) -1))) (and (>= (send ?X3 get-start) (send ?X3 get-start)) (or (<= (send ?X3 get-start) (send ?X3 get-end)) (= (send ?X3 get-end) -1))))) => (add-prop ?X2 (add-pred Can what (add-pred View what ?X1)) (make-instance of Duration (start (mincomstart ?X3 ?X4)) (end (maxcomend ?X3 ?X4))) 1))
(defrule 9fe8ce0451a7491c9bcc02313558dc35 (logical (object (is-a Proposition) (subject ?X1&:(or (eq (class ?X1) Person) (subclassp (class ?X1) Person))) (predicate ?Y32&:(or (eq (class ?Y32) Publish) (subclassp (class ?Y32) Publish))&:(or (eq (class (send ?Y32 get-what)) Content) (subclassp (class (send ?Y32 get-what)) Content))) (time ?X3) (truth 1))) (logical (object (is-a Proposition) (subject ?X2&:(eq ?X2 (send ?Y32 get-what))) (predicate ?Y33&:(or (eq (class ?Y33) Has) (subclassp (class ?Y33) Has))&:(or (eq (class (send ?Y33 get-what)) Status) (subclassp (class (send ?Y33 get-what)) Status))) (time ?X5&:(or (eq (class ?X5) Duration) (subclassp (class ?X5) Duration))) (truth 1))) => (send ?X5 put-end 733697) (add-prop (send ?Y32 get-what) (add-pred Has what [public]) (make-instance of Duration (start ?X3) (end -1.0)) 1))
(defrule d79ff1c8ff064aadae10f7a8b60f02df (logical (object (is-a Content) (name ?X1))) => (add-prop [manage_perm] (add-pred IsNeeded for_action (add-pred Publish what ?X1)) (make-instance of Duration (start 733697.0) (end -1.0)) 1))
(defrule 02d974fe0f20499994b68d3a231e6c54 (logical (object (is-a Proposition) (subject ?X1&:(or (eq (class ?X1) Person) (subclassp (class ?X1) Person))) (predicate ?Y35&:(or (eq (class ?Y35) Hide) (subclassp (class ?Y35) Hide))&:(or (eq (class (send ?Y35 get-what)) Content) (subclassp (class (send ?Y35 get-what)) Content))) (time ?X3) (truth 1))) (logical (object (is-a Proposition) (subject ?X2&:(eq ?X2 (send ?Y35 get-what))) (predicate ?Y36&:(or (eq (class ?Y36) Has) (subclassp (class ?Y36) Has))&:(or (eq (class (send ?Y36 get-what)) Status) (subclassp (class (send ?Y36 get-what)) Status))) (time ?X5&:(or (eq (class ?X5) Duration) (subclassp (class ?X5) Duration))) (truth 1))) => (send ?X5 put-end 733697) (add-prop (send ?Y35 get-what) (add-pred Has what [private]) (make-instance of Duration (start ?X3) (end -1.0)) 1))
(defrule 044186ca230e470eb3333fda5d79fbf2 (logical (object (is-a Proposition) (subject ?X1&:(or (eq (class ?X1) Person) (subclassp (class ?X1) Person))) (predicate ?Y38&:(or (eq (class ?Y38) IsOwner) (subclassp (class ?Y38) IsOwner))&:(or (eq (class (send ?Y38 get-of)) Content) (subclassp (class (send ?Y38 get-of)) Content))) (time ?X3&:(or (eq (class ?X3) Duration) (subclassp (class ?X3) Duration))) (truth 1))) => (add-prop ?X1 (add-pred Can what (add-pred Hide what (send ?Y38 get-of))) ?X3 1))
(reduce-class [john] Person)
john
(reduce-class [pete] Person)
pete
(reduce-class [jane] Person)
jane
(reduce-class [c1] Content)
c1
(reduce-class [c2] Content)
c2
(add-prop [john] (add-pred Has what [manager]) (make-instance of Duration (start 733697.0) (end -1.0)) 1)
john has what manager at from 733697.0 till -1.0
(add-prop [jane] (add-pred Has what [create_perm]) (make-instance of Duration (start 733697.0) (end -1.0)) 1)
jane has what create_perm at from 733697.0 till -1.0
(add-prop [jane] (add-pred Wants to (add-pred Create what [c1])) 733697.0 1)
jane wants to create what c1 at 733697.0
(add-prop [pete] (add-pred Wants to (add-pred Create what [c2])) 733697.0 1)
pete wants to create what c2 at 733697.0
----------running---------------------
jane create what c1 at 733697.0
jane isowner of c1 at from 733697.0 till -1.0
c1 has what private at from 733697.0 till -1.0
jane can what view what c1 at from 733697.0 till -1.0
manage_perm isneeded for_action view what c1 at from 733697.0 till -1.0
jane can what hide what c1 at from 733697.0 till -1.0
manage_perm isneeded for_action publish what c2 at from 733697.0 till -1.0
manage_perm isneeded for_action publish what c1 at from 733697.0 till -1.0
jane has what member at from 733697.0 till -1.0
jane has what basic_perm at from 733697.0 till -1.0
pete has what member at from 733697.0 till -1.0
pete has what basic_perm at from 733697.0 till -1.0
john has what member at from 733697.0 till -1.0
john has what basic_perm at from 733697.0 till -1.0
manager has what create_perm at from 733697.0 till -1.0
admin has what create_perm at from 733697.0 till -1.0
john has what create_perm at from 733697.0 till -1.0
manager has what manage_perm at from 733697.0 till -1.0
manager can what view what c1 at from 733697.0 till -1.0
manager can what publish what c2 at from 733697.0 till -1.0
manager can what publish what c1 at from 733697.0 till -1.0
admin has what manage_perm at from 733697.0 till -1.0
admin can what view what c1 at from 733697.0 till -1.0
admin can what publish what c2 at from 733697.0 till -1.0
admin can what publish what c1 at from 733697.0 till -1.0
john has what manage_perm at from 733697.0 till -1.0
john can what view what c1 at from 733697.0 till -1.0
john can what publish what c2 at from 733697.0 till -1.0
john can what publish what c1 at from 733697.0 till -1.0
manager has what basic_perm at from 733697.0 till -1.0
admin has what basic_perm at from 733697.0 till -1.0
manager has what member at from 733697.0 till -1.0
member has what member at from 733697.0 till -1.0
admin has what member at from 733697.0 till -1.0
----------run---------------------
(find-all-instances ((?prop Proposition) (?Y40 IsOwner) (?Y41 Duration)) (and (eq ?prop:subject [jane]) (eq ?Y40:of [c1]) (eq ?prop:predicate ?Y40) (= ?Y41:start 733697.0) (= ?Y41:end -1.0) (eq ?prop:truth 1)))
39


jane isowner of c1 at from 733697.0 till -1.0
jane isowner of c1 at from 733697.0 till -1.0
jane isowner of c1 at from 733697.0 till -1.0
jane isowner of c1 at from 733697.0 till -1.0
jane isowner of c1 at from 733697.0 till -1.0
jane isowner of c1 at from 733697.0 till -1.0
jane isowner of c1 at from 733697.0 till -1.0
jane isowner of c1 at from 733697.0 till -1.0
jane isowner of c1 at from 733697.0 till -1.0
jane isowner of c1 at from 733697.0 till -1.0
jane isowner of c1 at from 733697.0 till -1.0
jane isowner of c1 at from 733697.0 till -1.0
jane isowner of c1 at from 733697.0 till -1.0
jane isowner of c1 at from 733697.0 till -1.0
jane isowner of c1 at from 733697.0 till -1.0
jane isowner of c1 at from 733697.0 till -1.0
jane isowner of c1 at from 733697.0 till -1.0
jane isowner of c1 at from 733697.0 till -1.0
jane isowner of c1 at from 733697.0 till -1.0
jane isowner of c1 at from 733697.0 till -1.0
jane isowner of c1 at from 733697.0 till -1.0
jane isowner of c1 at from 733697.0 till -1.0
jane isowner of c1 at from 733697.0 till -1.0
jane isowner of c1 at from 733697.0 till -1.0
jane isowner of c1 at from 733697.0 till -1.0
jane isowner of c1 at from 733697.0 till -1.0
jane isowner of c1 at from 733697.0 till -1.0
jane isowner of c1 at from 733697.0 till -1.0
jane isowner of c1 at from 733697.0 till -1.0
jane isowner of c1 at from 733697.0 till -1.0
jane isowner of c1 at from 733697.0 till -1.0
jane isowner of c1 at from 733697.0 till -1.0
jane isowner of c1 at from 733697.0 till -1.0
jane isowner of c1 at from 733697.0 till -1.0
jane isowner of c1 at from 733697.0 till -1.0
jane isowner of c1 at from 733697.0 till -1.0
jane isowner of c1 at from 733697.0 till -1.0
jane isowner of c1 at from 733697.0 till -1.0
jane isowner of c1 at from 733697.0 till -1.0
(add-prop [jane] (add-pred Wants to (add-pred Publish what [c1])) 733697.0 1)
jane wants to publish what c1 at 733697.0
(add-prop [pete] (add-pred Wants to (add-pred Publish what [c2])) 733697.0 1)
pete wants to publish what c2 at 733697.0
----------running---------------------
----------run---------------------
(add-prop [john] (add-pred Wants to (add-pred Publish what [c1])) 733697.0 1)
john wants to publish what c1 at 733697.0
----------running---------------------
john publish what c1 at 733697.0
c1 has what public at from 733697.0 till -1.0
basic_perm isneeded for_action view what c1 at from 733697.0 till -1.0
pete can what view what c1 at from 733697.0 till -1.0
member can what view what c1 at from 733697.0 till -1.0
----------run---------------------
(defclass Name (is-a USER))
(deffunction reduce-class (?instance ?class) (if (eq (length$ (find-instance ((?a ?class)) (eq (instance-name ?a) ?instance))) 0) then (make-instance ?instance of ?class) (python-call tonl ?class ?instance)))
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


(deffunction mincomstart (?dur1 ?dur2)
    (return (max (send ?dur1 get-start) (send ?dur2 get-start)))
)



(deffunction maxcomend (?dur1 ?dur2)
    (bind ?e1 (send ?dur1 get-end))
    (bind ?e2 (send ?dur2 get-end))
    (if (= ?e1 ?e2) then (return ?e1))
    (if (= ?e2 -1) then
        (return ?e1)
    )
    (if (= ?e1 -1) then
        (return ?e2)
    )
    (return (min ?e1 ?e2))
)

(defclass Proposition (is-a Name) (slot truth (type INTEGER) (default 1) (pattern-match reactive)) (slot subject (type INSTANCE) (pattern-match reactive)) (slot predicate (type INSTANCE) (pattern-match reactive)) (slot time (type ?VARIABLE) (pattern-match reactive)))

(deffunction add-prop (?s ?p ?t ?r)
       (if (and (python-call ptonl ?s ?p ?t ?r)
                (= (+ (length$ (find-instance ((?prop Proposition) (?dur Duration))
                          (and (eq ?prop:subject ?s)
                               (eq ?prop:predicate ?p)
                               (eq ?prop:time ?dur)
                               (= ?dur:start (send ?t get-start))
                               (= ?dur:end (send ?t get-end))
                               (eq ?prop:truth ?r))))
                      (length$ (find-instance ((?prop Proposition))
                          (and (eq ?prop:subject ?s)
                               (eq ?prop:predicate ?p)
                               (= ?prop:time ?t)
                               (eq ?prop:truth ?r)))))
                 0))
        then (make-instance of Proposition (subject ?s)
                                           (predicate ?p)
                                           (time ?t)
                                           (truth ?r))
        else (return TRUE)))
------------OPEN---------------------
----------F-OPEN---------------------
(defclass Person (is-a Thing))
(defclass Can (is-a State) (slot what (type INSTANCE) (visibility public) (pattern-match reactive)))
(defclass Wants (is-a State) (slot to (type INSTANCE) (visibility public) (pattern-match reactive)))
(defclass Has (is-a State) (slot what (type INSTANCE) (visibility public) (pattern-match reactive)))
(defclass IsNeeded (is-a State) (slot for_action (type INSTANCE) (visibility public) (pattern-match reactive)))
(defclass IsIn (is-a State) (slot what (type INSTANCE) (visibility public) (pattern-match reactive)))
(defclass Group (is-a Thing))
(defclass Permission (is-a Thing))
(defclass Role (is-a Person))
(defclass Content (is-a Thing))
(defclass Create (is-a State) (slot what (type INSTANCE) (visibility public) (pattern-match reactive)))
(defclass IsOwner (is-a State) (slot of (type INSTANCE) (visibility public) (pattern-match reactive)))
(defclass Status (is-a Thing))
(defclass View (is-a State) (slot what (type INSTANCE) (visibility public) (pattern-match reactive)))
(defclass Publish (is-a State) (slot what (type INSTANCE) (visibility public) (pattern-match reactive)))
(defclass Hide (is-a State) (slot what (type INSTANCE) (visibility public) (pattern-match reactive)))
(reduce-class [admin] Person)
admin
(reduce-class [member] Role)
member
(reduce-class [manager] Role)
manager
(reduce-class [basic_perm] Permission)
basic_perm
(reduce-class [manage_perm] Permission)
manage_perm
(reduce-class [create_perm] Permission)
create_perm
(reduce-class [public] Status)
public
(reduce-class [private] Status)
private
(add-prop [admin] (add-pred Has what [manager]) (make-instance of Duration (start 733697.0) (end -1.0)) 1)
admin has what manager at from 733697.0 till -1.0
(add-prop [member] (add-pred Has what [basic_perm]) (make-instance of Duration (start 733697.0) (end -1.0)) 1)
member has what basic_perm at from 733697.0 till -1.0
(defrule e67c240f173d434e82ac4aecb0f68841 (logical (object (is-a Proposition) (subject ?X1&:(or (eq (class ?X1) Person) (subclassp (class ?X1) Person))) (predicate ?Y1&:(or (eq (class ?Y1) Wants) (subclassp (class ?Y1) Wants))&:(or (eq (class (send ?Y1 get-to)) Create) (subclassp (class (send ?Y1 get-to)) Create))&:(or (eq (class (send (send ?Y1 get-to) get-what)) Thing) (subclassp (class (send (send ?Y1 get-to) get-what)) Thing))) (time ?X2) (truth 1))) (logical (object (is-a Proposition) (subject ?X1) (predicate ?Y2&:(or (eq (class ?Y2) Has) (subclassp (class ?Y2) Has))&:(eq (send ?Y2 get-what) [create_perm])) (time ?X3&:(or (eq (class ?X3) Duration) (subclassp (class ?X3) Duration))) (truth 1))) (test (and (<= (send ?X3 get-start) ?X2) (or (= (send ?X3 get-end) -1) (>= (send ?X3 get-end) ?X2)))) => (add-prop ?X1 (add-pred Create what (send (send ?Y1 get-to) get-what)) ?X2 1))
(defrule f0bdef93ff1d45249b8e83ec43449522 (logical (object (is-a Proposition) (subject ?X1&:(or (eq (class ?X1) Person) (subclassp (class ?X1) Person))) (predicate ?Y4&:(or (eq (class ?Y4) Wants) (subclassp (class ?Y4) Wants))) (time ?X2) (truth 1))) (logical (object (is-a Proposition) (subject ?X1) (predicate ?Y5&:(or (eq (class ?Y5) Can) (subclassp (class ?Y5) Can))&:(eq (send ?Y4 get-to) (send ?Y5 get-what))) (time ?X3&:(or (eq (class ?X3) Duration) (subclassp (class ?X3) Duration))) (truth 1))) (test (and (<= (send ?X3 get-start) ?X2) (or (= (send ?X3 get-end) -1) (>= (send ?X3 get-end) ?X2)))) => (add-prop ?X1 (send ?Y4 get-to) ?X2 1))
(defrule 2ac192d886a049568dec3b7526002a20 (logical (object (is-a Proposition) (subject ?X2&:(or (eq (class ?X2) Thing) (subclassp (class ?X2) Thing))) (predicate ?Y7&:(or (eq (class ?Y7) IsNeeded) (subclassp (class ?Y7) IsNeeded))) (time ?X3&:(or (eq (class ?X3) Duration) (subclassp (class ?X3) Duration))) (truth 1))) (logical (object (is-a Proposition) (subject ?X1&:(or (eq (class ?X1) Thing) (subclassp (class ?X1) Thing))) (predicate ?Y9&:(or (eq (class ?Y9) Has) (subclassp (class ?Y9) Has))&:(eq (send ?Y9 get-what) ?X2)) (time ?X5&:(or (eq (class ?X5) Duration) (subclassp (class ?X5) Duration))) (truth 1))) (test (or (and (>= (send ?X3 get-start) (send ?X3 get-start)) (or (<= (send ?X3 get-start) (send ?X3 get-end)) (= (send ?X3 get-end) -1))) (and (>= (send ?X3 get-start) (send ?X3 get-start)) (or (<= (send ?X3 get-start) (send ?X3 get-end)) (= (send ?X3 get-end) -1))))) => (add-prop ?X1 (add-pred Can what (send ?Y7 get-for_action)) (make-instance of Duration (start (mincomstart ?X3 ?X5)) (end (maxcomend ?X3 ?X5))) 1))
(defrule 1f25292f6a0e4944a4aabe75db8b5c7a (logical (object (is-a Proposition) (subject ?X1&:(or (eq (class ?X1) Thing) (subclassp (class ?X1) Thing))) (predicate ?Y11&:(or (eq (class ?Y11) IsIn) (subclassp (class ?Y11) IsIn))&:(or (eq (class (send ?Y11 get-what)) Thing) (subclassp (class (send ?Y11 get-what)) Thing))) (time ?X4&:(or (eq (class ?X4) Duration) (subclassp (class ?X4) Duration))) (truth 1))) (logical (object (is-a Proposition) (subject ?X2&:(eq ?X2 (send ?Y11 get-what))) (predicate ?Y13&:(or (eq (class ?Y13) IsIn) (subclassp (class ?Y13) IsIn))&:(or (eq (class (send ?Y13 get-what)) Thing) (subclassp (class (send ?Y13 get-what)) Thing))) (time ?X5&:(or (eq (class ?X5) Duration) (subclassp (class ?X5) Duration))) (truth 1))) (test (or (and (>= (send ?X4 get-start) (send ?X4 get-start)) (or (<= (send ?X4 get-start) (send ?X4 get-end)) (= (send ?X4 get-end) -1))) (and (>= (send ?X4 get-start) (send ?X4 get-start)) (or (<= (send ?X4 get-start) (send ?X4 get-end)) (= (send ?X4 get-end) -1))))) => (add-prop ?X1 (add-pred IsIn what (send ?Y13 get-what)) (make-instance of Duration (start (mincomstart ?X4 ?X5)) (end (maxcomend ?X4 ?X5))) 1))
(defrule 6cd6b8a069e84a65b7aedfa1d436cd03 (logical (object (is-a Proposition) (subject ?X1&:(or (eq (class ?X1) Person) (subclassp (class ?X1) Person))) (predicate ?Y15&:(or (eq (class ?Y15) IsIn) (subclassp (class ?Y15) IsIn))) (time ?X3&:(or (eq (class ?X3) Duration) (subclassp (class ?X3) Duration))) (truth 1))) (logical (object (is-a Proposition) (subject ?X2&:(or (eq (class ?X2) Group) (subclassp (class ?X2) Group))) (predicate ?Y17&:(or (eq (class ?Y17) Has) (subclassp (class ?Y17) Has))&:(or (eq (class (send ?Y17 get-what)) Permission) (subclassp (class (send ?Y17 get-what)) Permission))) (time ?X5&:(or (eq (class ?X5) Duration) (subclassp (class ?X5) Duration))) (truth 1))) (test (or (and (>= (send ?X3 get-start) (send ?X3 get-start)) (or (<= (send ?X3 get-start) (send ?X3 get-end)) (= (send ?X3 get-end) -1))) (and (>= (send ?X3 get-start) (send ?X3 get-start)) (or (<= (send ?X3 get-start) (send ?X3 get-end)) (= (send ?X3 get-end) -1))))) => (add-prop ?X1 (add-pred Has what (send ?Y17 get-what)) (make-instance of Duration (start (mincomstart ?X3 ?X5)) (end (maxcomend ?X3 ?X5))) 1))
(defrule 91848d35868c411a9500a5d5db3c372e (logical (object (is-a Proposition) (subject ?X1&:(or (eq (class ?X1) Person) (subclassp (class ?X1) Person))) (predicate ?Y19&:(or (eq (class ?Y19) Has) (subclassp (class ?Y19) Has))&:(or (eq (class (send ?Y19 get-what)) Role) (subclassp (class (send ?Y19 get-what)) Role))) (time ?X3&:(or (eq (class ?X3) Duration) (subclassp (class ?X3) Duration))) (truth 1))) (logical (object (is-a Proposition) (subject ?X2&:(eq ?X2 (send ?Y19 get-what))) (predicate ?Y21&:(or (eq (class ?Y21) Has) (subclassp (class ?Y21) Has))&:(or (eq (class (send ?Y21 get-what)) Permission) (subclassp (class (send ?Y21 get-what)) Permission))) (time ?X5&:(or (eq (class ?X5) Duration) (subclassp (class ?X5) Duration))) (truth 1))) (test (or (and (>= (send ?X3 get-start) (send ?X3 get-start)) (or (<= (send ?X3 get-start) (send ?X3 get-end)) (= (send ?X3 get-end) -1))) (and (>= (send ?X3 get-start) (send ?X3 get-start)) (or (<= (send ?X3 get-start) (send ?X3 get-end)) (= (send ?X3 get-end) -1))))) => (add-prop ?X1 (add-pred Has what (send ?Y21 get-what)) (make-instance of Duration (start (mincomstart ?X3 ?X5)) (end (maxcomend ?X3 ?X5))) 1))
(defrule 9c2b5263e941450b840eac1a3e504367 (logical (object (is-a Person) (name ?X1))) => (add-prop ?X1 (add-pred Has what [member]) (make-instance of Duration (start 733697.0) (end -1.0)) 1))
(defrule 65d5ac160a4b4ac09ae13473dd788aae (logical (object (is-a Proposition) (subject ?X1&:(or (eq (class ?X1) Person) (subclassp (class ?X1) Person))) (predicate ?Y23&:(or (eq (class ?Y23) Create) (subclassp (class ?Y23) Create))&:(or (eq (class (send ?Y23 get-what)) Content) (subclassp (class (send ?Y23 get-what)) Content))) (time ?X3) (truth 1))) => (reduce-class (send ?Y23 get-what) Content) (add-prop ?X1 (add-pred IsOwner of (send ?Y23 get-what)) (make-instance of Duration (start ?X3) (end -1.0)) 1) (add-prop (send ?Y23 get-what) (add-pred Has what [private]) (make-instance of Duration (start ?X3) (end -1.0)) 1))
(defrule 6b385dd3fb2f41128725e9240f4a71f1 (logical (object (is-a Permission) (name ?X2))) => (add-prop [manager] (add-pred Has what ?X2) (make-instance of Duration (start 733697.0) (end -1.0)) 1))
(defrule b85b5684d4494fe095d7ccb604facee5 (logical (object (is-a Proposition) (subject ?X1&:(or (eq (class ?X1) Content) (subclassp (class ?X1) Content))) (predicate ?Y24&:(or (eq (class ?Y24) Has) (subclassp (class ?Y24) Has))&:(eq (send ?Y24 get-what) [public])) (time ?X2&:(or (eq (class ?X2) Duration) (subclassp (class ?X2) Duration))) (truth 1))) => (add-prop [basic_perm] (add-pred IsNeeded for_action (add-pred View what ?X1)) ?X2 1))
(defrule e9277f94f8644684882c7cd0585e4ecc (logical (object (is-a Proposition) (subject ?X1&:(or (eq (class ?X1) Content) (subclassp (class ?X1) Content))) (predicate ?Y26&:(or (eq (class ?Y26) Has) (subclassp (class ?Y26) Has))&:(eq (send ?Y26 get-what) [private])) (time ?X2&:(or (eq (class ?X2) Duration) (subclassp (class ?X2) Duration))) (truth 1))) => (add-prop [manage_perm] (add-pred IsNeeded for_action (add-pred View what ?X1)) ?X2 1))
(defrule 663603a4017046a09b4fae8d1a804d66 (logical (object (is-a Proposition) (subject ?X1&:(or (eq (class ?X1) Content) (subclassp (class ?X1) Content))) (predicate ?Y28&:(or (eq (class ?Y28) Has) (subclassp (class ?Y28) Has))&:(eq (send ?Y28 get-what) [private])) (time ?X3&:(or (eq (class ?X3) Duration) (subclassp (class ?X3) Duration))) (truth 1))) (logical (object (is-a Proposition) (subject ?X2&:(or (eq (class ?X2) Person) (subclassp (class ?X2) Person))) (predicate ?Y30&:(or (eq (class ?Y30) IsOwner) (subclassp (class ?Y30) IsOwner))&:(eq (send ?Y30 get-of) ?X1)) (time ?X4&:(or (eq (class ?X4) Duration) (subclassp (class ?X4) Duration))) (truth 1))) (test (or (and (>= (send ?X3 get-start) (send ?X3 get-start)) (or (<= (send ?X3 get-start) (send ?X3 get-end)) (= (send ?X3 get-end) -1))) (and (>= (send ?X3 get-start) (send ?X3 get-start)) (or (<= (send ?X3 get-start) (send ?X3 get-end)) (= (send ?X3 get-end) -1))))) => (add-prop ?X2 (add-pred Can what (add-pred View what ?X1)) (make-instance of Duration (start (mincomstart ?X3 ?X4)) (end (maxcomend ?X3 ?X4))) 1))
(defrule 919df010492d4bbc93fd107284b5acaf (logical (object (is-a Proposition) (subject ?X1&:(or (eq (class ?X1) Person) (subclassp (class ?X1) Person))) (predicate ?Y32&:(or (eq (class ?Y32) Publish) (subclassp (class ?Y32) Publish))&:(or (eq (class (send ?Y32 get-what)) Content) (subclassp (class (send ?Y32 get-what)) Content))) (time ?X3) (truth 1))) (logical (object (is-a Proposition) (subject ?X2&:(eq ?X2 (send ?Y32 get-what))) (predicate ?Y33&:(or (eq (class ?Y33) Has) (subclassp (class ?Y33) Has))&:(or (eq (class (send ?Y33 get-what)) Status) (subclassp (class (send ?Y33 get-what)) Status))) (time ?X5&:(or (eq (class ?X5) Duration) (subclassp (class ?X5) Duration))) (truth 1))) => (send ?X5 put-end 733697) (add-prop (send ?Y32 get-what) (add-pred Has what [public]) (make-instance of Duration (start ?X3) (end -1.0)) 1))
(defrule 98bc565012064378ac43daf9f9611a18 (logical (object (is-a Content) (name ?X1))) => (add-prop [manage_perm] (add-pred IsNeeded for_action (add-pred Publish what ?X1)) (make-instance of Duration (start 733697.0) (end -1.0)) 1))
(defrule cc6413b9e0b648678c36f19cc8b2ae59 (logical (object (is-a Proposition) (subject ?X1&:(or (eq (class ?X1) Person) (subclassp (class ?X1) Person))) (predicate ?Y35&:(or (eq (class ?Y35) Hide) (subclassp (class ?Y35) Hide))&:(or (eq (class (send ?Y35 get-what)) Content) (subclassp (class (send ?Y35 get-what)) Content))) (time ?X3) (truth 1))) (logical (object (is-a Proposition) (subject ?X2&:(eq ?X2 (send ?Y35 get-what))) (predicate ?Y36&:(or (eq (class ?Y36) Has) (subclassp (class ?Y36) Has))&:(or (eq (class (send ?Y36 get-what)) Status) (subclassp (class (send ?Y36 get-what)) Status))) (time ?X5&:(or (eq (class ?X5) Duration) (subclassp (class ?X5) Duration))) (truth 1))) => (send ?X5 put-end 733697) (add-prop (send ?Y35 get-what) (add-pred Has what [private]) (make-instance of Duration (start ?X3) (end -1.0)) 1))
(defrule 3401c79b9ca64a46b6aee0462c368180 (logical (object (is-a Proposition) (subject ?X1&:(or (eq (class ?X1) Person) (subclassp (class ?X1) Person))) (predicate ?Y38&:(or (eq (class ?Y38) IsOwner) (subclassp (class ?Y38) IsOwner))&:(or (eq (class (send ?Y38 get-of)) Content) (subclassp (class (send ?Y38 get-of)) Content))) (time ?X3&:(or (eq (class ?X3) Duration) (subclassp (class ?X3) Duration))) (truth 1))) => (add-prop ?X1 (add-pred Can what (add-pred Hide what (send ?Y38 get-of))) ?X3 1))
(reduce-class [john] Person)
john
(reduce-class [pete] Person)
pete
(reduce-class [jane] Person)
jane
(reduce-class [c1] Content)
c1
(reduce-class [c2] Content)
c2
(add-prop [john] (add-pred Has what [manager]) (make-instance of Duration (start 733697.0) (end -1.0)) 1)
john has what manager at from 733697.0 till -1.0
(add-prop [jane] (add-pred Has what [create_perm]) (make-instance of Duration (start 733697.0) (end -1.0)) 1)
jane has what create_perm at from 733697.0 till -1.0
(add-prop [jane] (add-pred Wants to (add-pred Create what [c1])) 733697.0 1)
jane wants to create what c1 at 733697.0
(add-prop [pete] (add-pred Wants to (add-pred Create what [c2])) 733697.0 1)
pete wants to create what c2 at 733697.0
----------running---------------------
jane create what c1 at 733697.0
jane isowner of c1 at from 733697.0 till -1.0
c1 has what private at from 733697.0 till -1.0
jane can what view what c1 at from 733697.0 till -1.0
manage_perm isneeded for_action view what c1 at from 733697.0 till -1.0
jane can what hide what c1 at from 733697.0 till -1.0
manage_perm isneeded for_action publish what c2 at from 733697.0 till -1.0
manage_perm isneeded for_action publish what c1 at from 733697.0 till -1.0
jane has what member at from 733697.0 till -1.0
jane has what basic_perm at from 733697.0 till -1.0
pete has what member at from 733697.0 till -1.0
pete has what basic_perm at from 733697.0 till -1.0
john has what member at from 733697.0 till -1.0
john has what basic_perm at from 733697.0 till -1.0
manager has what create_perm at from 733697.0 till -1.0
admin has what create_perm at from 733697.0 till -1.0
john has what create_perm at from 733697.0 till -1.0
manager has what manage_perm at from 733697.0 till -1.0
manager can what view what c1 at from 733697.0 till -1.0
manager can what publish what c2 at from 733697.0 till -1.0
manager can what publish what c1 at from 733697.0 till -1.0
admin has what manage_perm at from 733697.0 till -1.0
admin can what view what c1 at from 733697.0 till -1.0
admin can what publish what c2 at from 733697.0 till -1.0
admin can what publish what c1 at from 733697.0 till -1.0
john has what manage_perm at from 733697.0 till -1.0
john can what view what c1 at from 733697.0 till -1.0
john can what publish what c2 at from 733697.0 till -1.0
john can what publish what c1 at from 733697.0 till -1.0
manager has what basic_perm at from 733697.0 till -1.0
admin has what basic_perm at from 733697.0 till -1.0
manager has what member at from 733697.0 till -1.0
member has what member at from 733697.0 till -1.0
admin has what member at from 733697.0 till -1.0
----------run---------------------
(find-all-instances ((?prop Proposition) (?Y40 IsOwner) (?Y41 Duration)) (and (eq ?prop:subject [jane]) (eq ?Y40:of [c1]) (eq ?prop:predicate ?Y40) (= ?Y41:start 733697.0) (= ?Y41:end -1.0) (eq ?prop:truth 1)))
39


jane isowner of c1 at from 733697.0 till -1.0
jane isowner of c1 at from 733697.0 till -1.0
jane isowner of c1 at from 733697.0 till -1.0
jane isowner of c1 at from 733697.0 till -1.0
jane isowner of c1 at from 733697.0 till -1.0
jane isowner of c1 at from 733697.0 till -1.0
jane isowner of c1 at from 733697.0 till -1.0
jane isowner of c1 at from 733697.0 till -1.0
jane isowner of c1 at from 733697.0 till -1.0
jane isowner of c1 at from 733697.0 till -1.0
jane isowner of c1 at from 733697.0 till -1.0
jane isowner of c1 at from 733697.0 till -1.0
jane isowner of c1 at from 733697.0 till -1.0
jane isowner of c1 at from 733697.0 till -1.0
jane isowner of c1 at from 733697.0 till -1.0
jane isowner of c1 at from 733697.0 till -1.0
jane isowner of c1 at from 733697.0 till -1.0
jane isowner of c1 at from 733697.0 till -1.0
jane isowner of c1 at from 733697.0 till -1.0
jane isowner of c1 at from 733697.0 till -1.0
jane isowner of c1 at from 733697.0 till -1.0
jane isowner of c1 at from 733697.0 till -1.0
jane isowner of c1 at from 733697.0 till -1.0
jane isowner of c1 at from 733697.0 till -1.0
jane isowner of c1 at from 733697.0 till -1.0
jane isowner of c1 at from 733697.0 till -1.0
jane isowner of c1 at from 733697.0 till -1.0
jane isowner of c1 at from 733697.0 till -1.0
jane isowner of c1 at from 733697.0 till -1.0
jane isowner of c1 at from 733697.0 till -1.0
jane isowner of c1 at from 733697.0 till -1.0
jane isowner of c1 at from 733697.0 till -1.0
jane isowner of c1 at from 733697.0 till -1.0
jane isowner of c1 at from 733697.0 till -1.0
jane isowner of c1 at from 733697.0 till -1.0
jane isowner of c1 at from 733697.0 till -1.0
jane isowner of c1 at from 733697.0 till -1.0
jane isowner of c1 at from 733697.0 till -1.0
jane isowner of c1 at from 733697.0 till -1.0
(add-prop [jane] (add-pred Wants to (add-pred Publish what [c1])) 733697.0 1)
jane wants to publish what c1 at 733697.0
(add-prop [pete] (add-pred Wants to (add-pred Publish what [c2])) 733697.0 1)
pete wants to publish what c2 at 733697.0
----------running---------------------
----------run---------------------
(add-prop [john] (add-pred Wants to (add-pred Publish what [c1])) 733697.0 1)
john wants to publish what c1 at 733697.0
----------running---------------------
john publish what c1 at 733697.0
c1 has what public at from 733697.0 till -1.0
basic_perm isneeded for_action view what c1 at from 733697.0 till -1.0
pete can what view what c1 at from 733697.0 till -1.0
member can what view what c1 at from 733697.0 till -1.0
----------run---------------------
(defclass Name (is-a USER))
(deffunction reduce-class (?instance ?class) (if (eq (length$ (find-instance ((?a ?class)) (eq (instance-name ?a) ?instance))) 0) then (make-instance ?instance of ?class) (python-call tonl ?class ?instance)))
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


(deffunction mincomstart (?dur1 ?dur2)
    (return (max (send ?dur1 get-start) (send ?dur2 get-start)))
)



(deffunction maxcomend (?dur1 ?dur2)
    (bind ?e1 (send ?dur1 get-end))
    (bind ?e2 (send ?dur2 get-end))
    (if (= ?e1 ?e2) then (return ?e1))
    (if (= ?e2 -1) then
        (return ?e1)
    )
    (if (= ?e1 -1) then
        (return ?e2)
    )
    (return (min ?e1 ?e2))
)

(defclass Proposition (is-a Name) (slot truth (type INTEGER) (default 1) (pattern-match reactive)) (slot subject (type INSTANCE) (pattern-match reactive)) (slot predicate (type INSTANCE) (pattern-match reactive)) (slot time (type ?VARIABLE) (pattern-match reactive)))

(deffunction add-prop (?s ?p ?t ?r)
       (if (and (python-call ptonl ?s ?p ?t ?r)
                (= (+ (length$ (find-instance ((?prop Proposition) (?dur Duration))
                          (and (eq ?prop:subject ?s)
                               (eq ?prop:predicate ?p)
                               (eq ?prop:time ?dur)
                               (= ?dur:start (send ?t get-start))
                               (= ?dur:end (send ?t get-end))
                               (eq ?prop:truth ?r))))
                      (length$ (find-instance ((?prop Proposition))
                          (and (eq ?prop:subject ?s)
                               (eq ?prop:predicate ?p)
                               (= ?prop:time ?t)
                               (eq ?prop:truth ?r)))))
                 0))
        then (make-instance of Proposition (subject ?s)
                                           (predicate ?p)
                                           (time ?t)
                                           (truth ?r))
        else (return TRUE)))
------------OPEN---------------------
----------F-OPEN---------------------
(defclass Person (is-a Thing))
(defclass Can (is-a State) (slot what (type INSTANCE) (visibility public) (pattern-match reactive)))
(defclass Wants (is-a State) (slot to (type INSTANCE) (visibility public) (pattern-match reactive)))
(defclass Has (is-a State) (slot what (type INSTANCE) (visibility public) (pattern-match reactive)))
(defclass IsNeeded (is-a State) (slot for_action (type INSTANCE) (visibility public) (pattern-match reactive)))
(defclass IsIn (is-a State) (slot what (type INSTANCE) (visibility public) (pattern-match reactive)))
(defclass Group (is-a Thing))
(defclass Permission (is-a Thing))
(defclass Role (is-a Person))
(defclass Content (is-a Thing))
(defclass Create (is-a State) (slot what (type INSTANCE) (visibility public) (pattern-match reactive)))
(defclass IsOwner (is-a State) (slot of (type INSTANCE) (visibility public) (pattern-match reactive)))
(defclass Status (is-a Thing))
(defclass View (is-a State) (slot what (type INSTANCE) (visibility public) (pattern-match reactive)))
(defclass Publish (is-a State) (slot what (type INSTANCE) (visibility public) (pattern-match reactive)))
(defclass Hide (is-a State) (slot what (type INSTANCE) (visibility public) (pattern-match reactive)))
(reduce-class [admin] Person)
admin
(reduce-class [member] Role)
member
(reduce-class [manager] Role)
manager
(reduce-class [basic_perm] Permission)
basic_perm
(reduce-class [manage_perm] Permission)
manage_perm
(reduce-class [create_perm] Permission)
create_perm
(reduce-class [public] Status)
public
(reduce-class [private] Status)
private
(add-prop [admin] (add-pred Has what [manager]) (make-instance of Duration (start 733697.0) (end -1.0)) 1)
admin has what manager at from 733697.0 till -1.0
(add-prop [member] (add-pred Has what [basic_perm]) (make-instance of Duration (start 733697.0) (end -1.0)) 1)
member has what basic_perm at from 733697.0 till -1.0
(defrule d607334725984e9ab20f6e5f8f5f743a (logical (object (is-a Proposition) (subject ?X1&:(or (eq (class ?X1) Person) (subclassp (class ?X1) Person))) (predicate ?Y1&:(or (eq (class ?Y1) Wants) (subclassp (class ?Y1) Wants))&:(or (eq (class (send ?Y1 get-to)) Create) (subclassp (class (send ?Y1 get-to)) Create))&:(or (eq (class (send (send ?Y1 get-to) get-what)) Thing) (subclassp (class (send (send ?Y1 get-to) get-what)) Thing))) (time ?X2) (truth 1))) (logical (object (is-a Proposition) (subject ?X1) (predicate ?Y2&:(or (eq (class ?Y2) Has) (subclassp (class ?Y2) Has))&:(eq (send ?Y2 get-what) [create_perm])) (time ?X3&:(or (eq (class ?X3) Duration) (subclassp (class ?X3) Duration))) (truth 1))) (test (and (<= (send ?X3 get-start) ?X2) (or (= (send ?X3 get-end) -1) (>= (send ?X3 get-end) ?X2)))) => (add-prop ?X1 (add-pred Create what (send (send ?Y1 get-to) get-what)) ?X2 1))
(defrule eb8966ae7ed04f0b8a0442f71cdfffb8 (logical (object (is-a Proposition) (subject ?X1&:(or (eq (class ?X1) Person) (subclassp (class ?X1) Person))) (predicate ?Y4&:(or (eq (class ?Y4) Wants) (subclassp (class ?Y4) Wants))) (time ?X2) (truth 1))) (logical (object (is-a Proposition) (subject ?X1) (predicate ?Y5&:(or (eq (class ?Y5) Can) (subclassp (class ?Y5) Can))&:(eq (send ?Y4 get-to) (send ?Y5 get-what))) (time ?X3&:(or (eq (class ?X3) Duration) (subclassp (class ?X3) Duration))) (truth 1))) (test (and (<= (send ?X3 get-start) ?X2) (or (= (send ?X3 get-end) -1) (>= (send ?X3 get-end) ?X2)))) => (add-prop ?X1 (send ?Y4 get-to) ?X2 1))
(defrule aa698f9895ec4e438b5302f4fbd7c1d0 (logical (object (is-a Proposition) (subject ?X2&:(or (eq (class ?X2) Thing) (subclassp (class ?X2) Thing))) (predicate ?Y7&:(or (eq (class ?Y7) IsNeeded) (subclassp (class ?Y7) IsNeeded))) (time ?X3&:(or (eq (class ?X3) Duration) (subclassp (class ?X3) Duration))) (truth 1))) (logical (object (is-a Proposition) (subject ?X1&:(or (eq (class ?X1) Thing) (subclassp (class ?X1) Thing))) (predicate ?Y9&:(or (eq (class ?Y9) Has) (subclassp (class ?Y9) Has))&:(eq (send ?Y9 get-what) ?X2)) (time ?X5&:(or (eq (class ?X5) Duration) (subclassp (class ?X5) Duration))) (truth 1))) (test (or (and (>= (send ?X3 get-start) (send ?X3 get-start)) (or (<= (send ?X3 get-start) (send ?X3 get-end)) (= (send ?X3 get-end) -1))) (and (>= (send ?X3 get-start) (send ?X3 get-start)) (or (<= (send ?X3 get-start) (send ?X3 get-end)) (= (send ?X3 get-end) -1))))) => (add-prop ?X1 (add-pred Can what (send ?Y7 get-for_action)) (make-instance of Duration (start (mincomstart ?X3 ?X5)) (end (maxcomend ?X3 ?X5))) 1))
(defrule 58ef48e6f0d941f58ca655a2ce0f5ac5 (logical (object (is-a Proposition) (subject ?X1&:(or (eq (class ?X1) Thing) (subclassp (class ?X1) Thing))) (predicate ?Y11&:(or (eq (class ?Y11) IsIn) (subclassp (class ?Y11) IsIn))&:(or (eq (class (send ?Y11 get-what)) Thing) (subclassp (class (send ?Y11 get-what)) Thing))) (time ?X4&:(or (eq (class ?X4) Duration) (subclassp (class ?X4) Duration))) (truth 1))) (logical (object (is-a Proposition) (subject ?X2&:(eq ?X2 (send ?Y11 get-what))) (predicate ?Y13&:(or (eq (class ?Y13) IsIn) (subclassp (class ?Y13) IsIn))&:(or (eq (class (send ?Y13 get-what)) Thing) (subclassp (class (send ?Y13 get-what)) Thing))) (time ?X5&:(or (eq (class ?X5) Duration) (subclassp (class ?X5) Duration))) (truth 1))) (test (or (and (>= (send ?X4 get-start) (send ?X4 get-start)) (or (<= (send ?X4 get-start) (send ?X4 get-end)) (= (send ?X4 get-end) -1))) (and (>= (send ?X4 get-start) (send ?X4 get-start)) (or (<= (send ?X4 get-start) (send ?X4 get-end)) (= (send ?X4 get-end) -1))))) => (add-prop ?X1 (add-pred IsIn what (send ?Y13 get-what)) (make-instance of Duration (start (mincomstart ?X4 ?X5)) (end (maxcomend ?X4 ?X5))) 1))
(defrule 8ff5ed356a17486e8660fa64592cbe05 (logical (object (is-a Proposition) (subject ?X1&:(or (eq (class ?X1) Person) (subclassp (class ?X1) Person))) (predicate ?Y15&:(or (eq (class ?Y15) IsIn) (subclassp (class ?Y15) IsIn))) (time ?X3&:(or (eq (class ?X3) Duration) (subclassp (class ?X3) Duration))) (truth 1))) (logical (object (is-a Proposition) (subject ?X2&:(or (eq (class ?X2) Group) (subclassp (class ?X2) Group))) (predicate ?Y17&:(or (eq (class ?Y17) Has) (subclassp (class ?Y17) Has))&:(or (eq (class (send ?Y17 get-what)) Permission) (subclassp (class (send ?Y17 get-what)) Permission))) (time ?X5&:(or (eq (class ?X5) Duration) (subclassp (class ?X5) Duration))) (truth 1))) (test (or (and (>= (send ?X3 get-start) (send ?X3 get-start)) (or (<= (send ?X3 get-start) (send ?X3 get-end)) (= (send ?X3 get-end) -1))) (and (>= (send ?X3 get-start) (send ?X3 get-start)) (or (<= (send ?X3 get-start) (send ?X3 get-end)) (= (send ?X3 get-end) -1))))) => (add-prop ?X1 (add-pred Has what (send ?Y17 get-what)) (make-instance of Duration (start (mincomstart ?X3 ?X5)) (end (maxcomend ?X3 ?X5))) 1))
(defrule b1d299ac8c61463a94cfb1e76bc36da7 (logical (object (is-a Proposition) (subject ?X1&:(or (eq (class ?X1) Person) (subclassp (class ?X1) Person))) (predicate ?Y19&:(or (eq (class ?Y19) Has) (subclassp (class ?Y19) Has))&:(or (eq (class (send ?Y19 get-what)) Role) (subclassp (class (send ?Y19 get-what)) Role))) (time ?X3&:(or (eq (class ?X3) Duration) (subclassp (class ?X3) Duration))) (truth 1))) (logical (object (is-a Proposition) (subject ?X2&:(eq ?X2 (send ?Y19 get-what))) (predicate ?Y21&:(or (eq (class ?Y21) Has) (subclassp (class ?Y21) Has))&:(or (eq (class (send ?Y21 get-what)) Permission) (subclassp (class (send ?Y21 get-what)) Permission))) (time ?X5&:(or (eq (class ?X5) Duration) (subclassp (class ?X5) Duration))) (truth 1))) (test (or (and (>= (send ?X3 get-start) (send ?X3 get-start)) (or (<= (send ?X3 get-start) (send ?X3 get-end)) (= (send ?X3 get-end) -1))) (and (>= (send ?X3 get-start) (send ?X3 get-start)) (or (<= (send ?X3 get-start) (send ?X3 get-end)) (= (send ?X3 get-end) -1))))) => (add-prop ?X1 (add-pred Has what (send ?Y21 get-what)) (make-instance of Duration (start (mincomstart ?X3 ?X5)) (end (maxcomend ?X3 ?X5))) 1))
(defrule c9bd5be3db9f4e61980311582415139f (logical (object (is-a Person) (name ?X1))) => (add-prop ?X1 (add-pred Has what [member]) (make-instance of Duration (start 733697.0) (end -1.0)) 1))
(defrule 5de3d81bbf864b5182495471d322f9d2 (logical (object (is-a Proposition) (subject ?X1&:(or (eq (class ?X1) Person) (subclassp (class ?X1) Person))) (predicate ?Y23&:(or (eq (class ?Y23) Create) (subclassp (class ?Y23) Create))&:(or (eq (class (send ?Y23 get-what)) Content) (subclassp (class (send ?Y23 get-what)) Content))) (time ?X3) (truth 1))) => (reduce-class (send ?Y23 get-what) Content) (add-prop ?X1 (add-pred IsOwner of (send ?Y23 get-what)) (make-instance of Duration (start ?X3) (end -1.0)) 1) (add-prop (send ?Y23 get-what) (add-pred Has what [private]) (make-instance of Duration (start ?X3) (end -1.0)) 1))
(defrule 350c3095c98e4009b00c90341d964a8a (logical (object (is-a Permission) (name ?X2))) => (add-prop [manager] (add-pred Has what ?X2) (make-instance of Duration (start 733697.0) (end -1.0)) 1))
(defrule 0a341c7fdace40ca97db18642f64ed96 (logical (object (is-a Proposition) (subject ?X1&:(or (eq (class ?X1) Content) (subclassp (class ?X1) Content))) (predicate ?Y24&:(or (eq (class ?Y24) Has) (subclassp (class ?Y24) Has))&:(eq (send ?Y24 get-what) [public])) (time ?X2&:(or (eq (class ?X2) Duration) (subclassp (class ?X2) Duration))) (truth 1))) => (add-prop [basic_perm] (add-pred IsNeeded for_action (add-pred View what ?X1)) ?X2 1))
(defrule a6b6b2589d064fc1b081cbf8a90f6d13 (logical (object (is-a Proposition) (subject ?X1&:(or (eq (class ?X1) Content) (subclassp (class ?X1) Content))) (predicate ?Y26&:(or (eq (class ?Y26) Has) (subclassp (class ?Y26) Has))&:(eq (send ?Y26 get-what) [private])) (time ?X2&:(or (eq (class ?X2) Duration) (subclassp (class ?X2) Duration))) (truth 1))) => (add-prop [manage_perm] (add-pred IsNeeded for_action (add-pred View what ?X1)) ?X2 1))
(defrule af4e97dc93674d3199c4d1d4f3e3a063 (logical (object (is-a Proposition) (subject ?X1&:(or (eq (class ?X1) Content) (subclassp (class ?X1) Content))) (predicate ?Y28&:(or (eq (class ?Y28) Has) (subclassp (class ?Y28) Has))&:(eq (send ?Y28 get-what) [private])) (time ?X3&:(or (eq (class ?X3) Duration) (subclassp (class ?X3) Duration))) (truth 1))) (logical (object (is-a Proposition) (subject ?X2&:(or (eq (class ?X2) Person) (subclassp (class ?X2) Person))) (predicate ?Y30&:(or (eq (class ?Y30) IsOwner) (subclassp (class ?Y30) IsOwner))&:(eq (send ?Y30 get-of) ?X1)) (time ?X4&:(or (eq (class ?X4) Duration) (subclassp (class ?X4) Duration))) (truth 1))) (test (or (and (>= (send ?X3 get-start) (send ?X3 get-start)) (or (<= (send ?X3 get-start) (send ?X3 get-end)) (= (send ?X3 get-end) -1))) (and (>= (send ?X3 get-start) (send ?X3 get-start)) (or (<= (send ?X3 get-start) (send ?X3 get-end)) (= (send ?X3 get-end) -1))))) => (add-prop ?X2 (add-pred Can what (add-pred View what ?X1)) (make-instance of Duration (start (mincomstart ?X3 ?X4)) (end (maxcomend ?X3 ?X4))) 1))
(defrule e3e5eb36fcff4fd490f169d16825eb41 (logical (object (is-a Proposition) (subject ?X1&:(or (eq (class ?X1) Person) (subclassp (class ?X1) Person))) (predicate ?Y32&:(or (eq (class ?Y32) Publish) (subclassp (class ?Y32) Publish))&:(or (eq (class (send ?Y32 get-what)) Content) (subclassp (class (send ?Y32 get-what)) Content))) (time ?X3) (truth 1))) (logical (object (is-a Proposition) (subject ?X2&:(eq ?X2 (send ?Y32 get-what))) (predicate ?Y33&:(or (eq (class ?Y33) Has) (subclassp (class ?Y33) Has))&:(or (eq (class (send ?Y33 get-what)) Status) (subclassp (class (send ?Y33 get-what)) Status))) (time ?X5&:(or (eq (class ?X5) Duration) (subclassp (class ?X5) Duration))) (truth 1))) => (send ?X5 put-end 733697) (add-prop (send ?Y32 get-what) (add-pred Has what [public]) (make-instance of Duration (start ?X3) (end -1.0)) 1))
(defrule e6871c91e04547e791516256a17aeec0 (logical (object (is-a Content) (name ?X1))) => (add-prop [manage_perm] (add-pred IsNeeded for_action (add-pred Publish what ?X1)) (make-instance of Duration (start 733697.0) (end -1.0)) 1))
(defrule 15d2104a490246c891c32e333b05820f (logical (object (is-a Proposition) (subject ?X1&:(or (eq (class ?X1) Person) (subclassp (class ?X1) Person))) (predicate ?Y35&:(or (eq (class ?Y35) Hide) (subclassp (class ?Y35) Hide))&:(or (eq (class (send ?Y35 get-what)) Content) (subclassp (class (send ?Y35 get-what)) Content))) (time ?X3) (truth 1))) (logical (object (is-a Proposition) (subject ?X2&:(eq ?X2 (send ?Y35 get-what))) (predicate ?Y36&:(or (eq (class ?Y36) Has) (subclassp (class ?Y36) Has))&:(or (eq (class (send ?Y36 get-what)) Status) (subclassp (class (send ?Y36 get-what)) Status))) (time ?X5&:(or (eq (class ?X5) Duration) (subclassp (class ?X5) Duration))) (truth 1))) => (send ?X5 put-end 733697) (add-prop (send ?Y35 get-what) (add-pred Has what [private]) (make-instance of Duration (start ?X3) (end -1.0)) 1))
(defrule 406385101e93409c8a17d8cc598a8889 (logical (object (is-a Proposition) (subject ?X1&:(or (eq (class ?X1) Person) (subclassp (class ?X1) Person))) (predicate ?Y38&:(or (eq (class ?Y38) IsOwner) (subclassp (class ?Y38) IsOwner))&:(or (eq (class (send ?Y38 get-of)) Content) (subclassp (class (send ?Y38 get-of)) Content))) (time ?X3&:(or (eq (class ?X3) Duration) (subclassp (class ?X3) Duration))) (truth 1))) => (add-prop ?X1 (add-pred Can what (add-pred Hide what (send ?Y38 get-of))) ?X3 1))
(reduce-class [john] Person)
john
(reduce-class [pete] Person)
pete
(reduce-class [jane] Person)
jane
(reduce-class [c1] Content)
c1
(reduce-class [c2] Content)
c2
(add-prop [john] (add-pred Has what [manager]) (make-instance of Duration (start 733697.0) (end -1.0)) 1)
john has what manager at from 733697.0 till -1.0
(add-prop [jane] (add-pred Has what [create_perm]) (make-instance of Duration (start 733697.0) (end -1.0)) 1)
jane has what create_perm at from 733697.0 till -1.0
(add-prop [jane] (add-pred Wants to (add-pred Create what [c1])) 733697.0 1)
jane wants to create what c1 at 733697.0
(add-prop [pete] (add-pred Wants to (add-pred Create what [c2])) 733697.0 1)
pete wants to create what c2 at 733697.0
----------running---------------------
jane create what c1 at 733697.0
jane isowner of c1 at from 733697.0 till -1.0
c1 has what private at from 733697.0 till -1.0
jane can what view what c1 at from 733697.0 till -1.0
manage_perm isneeded for_action view what c1 at from 733697.0 till -1.0
jane can what hide what c1 at from 733697.0 till -1.0
manage_perm isneeded for_action publish what c2 at from 733697.0 till -1.0
manage_perm isneeded for_action publish what c1 at from 733697.0 till -1.0
jane has what member at from 733697.0 till -1.0
jane has what basic_perm at from 733697.0 till -1.0
pete has what member at from 733697.0 till -1.0
pete has what basic_perm at from 733697.0 till -1.0
john has what member at from 733697.0 till -1.0
john has what basic_perm at from 733697.0 till -1.0
manager has what create_perm at from 733697.0 till -1.0
admin has what create_perm at from 733697.0 till -1.0
john has what create_perm at from 733697.0 till -1.0
manager has what manage_perm at from 733697.0 till -1.0
manager can what view what c1 at from 733697.0 till -1.0
manager can what publish what c2 at from 733697.0 till -1.0
manager can what publish what c1 at from 733697.0 till -1.0
admin has what manage_perm at from 733697.0 till -1.0
admin can what view what c1 at from 733697.0 till -1.0
admin can what publish what c2 at from 733697.0 till -1.0
admin can what publish what c1 at from 733697.0 till -1.0
john has what manage_perm at from 733697.0 till -1.0
john can what view what c1 at from 733697.0 till -1.0
john can what publish what c2 at from 733697.0 till -1.0
john can what publish what c1 at from 733697.0 till -1.0
manager has what basic_perm at from 733697.0 till -1.0
admin has what basic_perm at from 733697.0 till -1.0
manager has what member at from 733697.0 till -1.0
member has what member at from 733697.0 till -1.0
admin has what member at from 733697.0 till -1.0
----------run---------------------
(find-all-instances ((?prop Proposition) (?Y40 IsOwner) (?Y41 Duration)) (and (eq ?prop:subject [jane]) (eq ?Y40:of [c1]) (eq ?prop:predicate ?Y40) (= ?Y41:start 733697.0) (= ?Y41:end -1.0) (eq ?prop:truth 1)))
39


jane isowner of c1 at from 733697.0 till -1.0
jane isowner of c1 at from 733697.0 till -1.0
jane isowner of c1 at from 733697.0 till -1.0
jane isowner of c1 at from 733697.0 till -1.0
jane isowner of c1 at from 733697.0 till -1.0
jane isowner of c1 at from 733697.0 till -1.0
jane isowner of c1 at from 733697.0 till -1.0
jane isowner of c1 at from 733697.0 till -1.0
jane isowner of c1 at from 733697.0 till -1.0
jane isowner of c1 at from 733697.0 till -1.0
jane isowner of c1 at from 733697.0 till -1.0
jane isowner of c1 at from 733697.0 till -1.0
jane isowner of c1 at from 733697.0 till -1.0
jane isowner of c1 at from 733697.0 till -1.0
jane isowner of c1 at from 733697.0 till -1.0
jane isowner of c1 at from 733697.0 till -1.0
jane isowner of c1 at from 733697.0 till -1.0
jane isowner of c1 at from 733697.0 till -1.0
jane isowner of c1 at from 733697.0 till -1.0
jane isowner of c1 at from 733697.0 till -1.0
jane isowner of c1 at from 733697.0 till -1.0
jane isowner of c1 at from 733697.0 till -1.0
jane isowner of c1 at from 733697.0 till -1.0
jane isowner of c1 at from 733697.0 till -1.0
jane isowner of c1 at from 733697.0 till -1.0
jane isowner of c1 at from 733697.0 till -1.0
jane isowner of c1 at from 733697.0 till -1.0
jane isowner of c1 at from 733697.0 till -1.0
jane isowner of c1 at from 733697.0 till -1.0
jane isowner of c1 at from 733697.0 till -1.0
jane isowner of c1 at from 733697.0 till -1.0
jane isowner of c1 at from 733697.0 till -1.0
jane isowner of c1 at from 733697.0 till -1.0
jane isowner of c1 at from 733697.0 till -1.0
jane isowner of c1 at from 733697.0 till -1.0
jane isowner of c1 at from 733697.0 till -1.0
jane isowner of c1 at from 733697.0 till -1.0
jane isowner of c1 at from 733697.0 till -1.0
jane isowner of c1 at from 733697.0 till -1.0
(add-prop [jane] (add-pred Wants to (add-pred Publish what [c1])) 733697.0 1)
jane wants to publish what c1 at 733697.0
(add-prop [pete] (add-pred Wants to (add-pred Publish what [c2])) 733697.0 1)
pete wants to publish what c2 at 733697.0
----------running---------------------
----------run---------------------
(add-prop [john] (add-pred Wants to (add-pred Publish what [c1])) 733697.0 1)
john wants to publish what c1 at 733697.0
----------running---------------------
john publish what c1 at 733697.0
c1 has what public at from 733697.0 till -1.0
basic_perm isneeded for_action view what c1 at from 733697.0 till -1.0
pete can what view what c1 at from 733697.0 till -1.0
member can what view what c1 at from 733697.0 till -1.0
----------run---------------------
(find-all-instances ((?prop Proposition) (?Y42 Has) (?Y43 Duration)) (and (eq ?prop:subject [c1]) (eq ?Y42:what [private]) (eq ?prop:predicate ?Y42) (= ?Y43:start 733697.0) (= ?Y43:end -1.0) (eq ?prop:truth 1)))
45


c1 has what private at from 733697.0 till 733697.0
c1 has what private at from 733697.0 till 733697.0
c1 has what private at from 733697.0 till 733697.0
c1 has what private at from 733697.0 till 733697.0
c1 has what private at from 733697.0 till 733697.0
c1 has what private at from 733697.0 till 733697.0
c1 has what private at from 733697.0 till 733697.0
c1 has what private at from 733697.0 till 733697.0
c1 has what private at from 733697.0 till 733697.0
c1 has what private at from 733697.0 till 733697.0
c1 has what private at from 733697.0 till 733697.0
c1 has what private at from 733697.0 till 733697.0
c1 has what private at from 733697.0 till 733697.0
c1 has what private at from 733697.0 till 733697.0
c1 has what private at from 733697.0 till 733697.0
c1 has what private at from 733697.0 till 733697.0
c1 has what private at from 733697.0 till 733697.0
c1 has what private at from 733697.0 till 733697.0
c1 has what private at from 733697.0 till 733697.0
c1 has what private at from 733697.0 till 733697.0
c1 has what private at from 733697.0 till 733697.0
c1 has what private at from 733697.0 till 733697.0
c1 has what private at from 733697.0 till 733697.0
c1 has what private at from 733697.0 till 733697.0
c1 has what private at from 733697.0 till 733697.0
c1 has what private at from 733697.0 till 733697.0
c1 has what private at from 733697.0 till 733697.0
c1 has what private at from 733697.0 till 733697.0
c1 has what private at from 733697.0 till 733697.0
c1 has what private at from 733697.0 till 733697.0
c1 has what private at from 733697.0 till 733697.0
c1 has what private at from 733697.0 till 733697.0
c1 has what private at from 733697.0 till 733697.0
c1 has what private at from 733697.0 till 733697.0
c1 has what private at from 733697.0 till 733697.0
c1 has what private at from 733697.0 till 733697.0
c1 has what private at from 733697.0 till 733697.0
c1 has what private at from 733697.0 till 733697.0
c1 has what private at from 733697.0 till 733697.0
c1 has what private at from 733697.0 till 733697.0
c1 has what private at from 733697.0 till 733697.0
c1 has what private at from 733697.0 till 733697.0
c1 has what private at from 733697.0 till 733697.0
c1 has what private at from 733697.0 till 733697.0
c1 has what private at from 733697.0 till 733697.0
(defclass Name (is-a USER))
(deffunction reduce-class (?instance ?class) (if (eq (length$ (find-instance ((?a ?class)) (eq (instance-name ?a) ?instance))) 0) then (make-instance ?instance of ?class) (python-call tonl ?class ?instance)))
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


(deffunction mincomstart (?dur1 ?dur2)
    (return (max (send ?dur1 get-start) (send ?dur2 get-start)))
)



(deffunction maxcomend (?dur1 ?dur2)
    (bind ?e1 (send ?dur1 get-end))
    (bind ?e2 (send ?dur2 get-end))
    (if (= ?e1 ?e2) then (return ?e1))
    (if (= ?e2 -1) then
        (return ?e1)
    )
    (if (= ?e1 -1) then
        (return ?e2)
    )
    (return (min ?e1 ?e2))
)

(defclass Proposition (is-a Name) (slot truth (type INTEGER) (default 1) (pattern-match reactive)) (slot subject (type INSTANCE) (pattern-match reactive)) (slot predicate (type INSTANCE) (pattern-match reactive)) (slot time (type ?VARIABLE) (pattern-match reactive)))

(deffunction add-prop (?s ?p ?t ?r)
       (if (and (python-call ptonl ?s ?p ?t ?r)
                (= (+ (length$ (find-instance ((?prop Proposition) (?dur Duration))
                          (and (eq ?prop:subject ?s)
                               (eq ?prop:predicate ?p)
                               (eq ?prop:time ?dur)
                               (= ?dur:start (send ?t get-start))
                               (= ?dur:end (send ?t get-end))
                               (eq ?prop:truth ?r))))
                      (length$ (find-instance ((?prop Proposition))
                          (and (eq ?prop:subject ?s)
                               (eq ?prop:predicate ?p)
                               (= ?prop:time ?t)
                               (eq ?prop:truth ?r)))))
                 0))
        then (make-instance of Proposition (subject ?s)
                                           (predicate ?p)
                                           (time ?t)
                                           (truth ?r))
        else (return TRUE)))
------------OPEN---------------------
----------F-OPEN---------------------
(defclass Person (is-a Thing))
(defclass Can (is-a State) (slot what (type INSTANCE) (visibility public) (pattern-match reactive)))
(defclass Wants (is-a State) (slot to (type INSTANCE) (visibility public) (pattern-match reactive)))
(defclass Has (is-a State) (slot what (type INSTANCE) (visibility public) (pattern-match reactive)))
(defclass IsNeeded (is-a State) (slot for_action (type INSTANCE) (visibility public) (pattern-match reactive)))
(defclass IsIn (is-a State) (slot what (type INSTANCE) (visibility public) (pattern-match reactive)))
(defclass Group (is-a Thing))
(defclass Permission (is-a Thing))
(defclass Role (is-a Person))
(defclass Content (is-a Thing))
(defclass Create (is-a State) (slot what (type INSTANCE) (visibility public) (pattern-match reactive)))
(defclass IsOwner (is-a State) (slot of (type INSTANCE) (visibility public) (pattern-match reactive)))
(defclass Status (is-a Thing))
(defclass View (is-a State) (slot what (type INSTANCE) (visibility public) (pattern-match reactive)))
(defclass Publish (is-a State) (slot what (type INSTANCE) (visibility public) (pattern-match reactive)))
(defclass Hide (is-a State) (slot what (type INSTANCE) (visibility public) (pattern-match reactive)))
(reduce-class [admin] Person)
admin
(reduce-class [member] Role)
member
(reduce-class [manager] Role)
manager
(reduce-class [basic_perm] Permission)
basic_perm
(reduce-class [manage_perm] Permission)
manage_perm
(reduce-class [create_perm] Permission)
create_perm
(reduce-class [public] Status)
public
(reduce-class [private] Status)
private
(add-prop [admin] (add-pred Has what [manager]) (make-instance of Duration (start 733697.0) (end -1.0)) 1)
admin has what manager at from 733697.0 till -1.0
(add-prop [member] (add-pred Has what [basic_perm]) (make-instance of Duration (start 733697.0) (end -1.0)) 1)
member has what basic_perm at from 733697.0 till -1.0
(defrule 5f6e6098a19e467285c46064e02ef6f7 (logical (object (is-a Proposition) (subject ?X1&:(or (eq (class ?X1) Person) (subclassp (class ?X1) Person))) (predicate ?Y1&:(or (eq (class ?Y1) Wants) (subclassp (class ?Y1) Wants))&:(or (eq (class (send ?Y1 get-to)) Create) (subclassp (class (send ?Y1 get-to)) Create))&:(or (eq (class (send (send ?Y1 get-to) get-what)) Thing) (subclassp (class (send (send ?Y1 get-to) get-what)) Thing))) (time ?X2) (truth 1))) (logical (object (is-a Proposition) (subject ?X1) (predicate ?Y2&:(or (eq (class ?Y2) Has) (subclassp (class ?Y2) Has))&:(eq (send ?Y2 get-what) [create_perm])) (time ?X3&:(or (eq (class ?X3) Duration) (subclassp (class ?X3) Duration))) (truth 1))) (test (and (<= (send ?X3 get-start) ?X2) (or (= (send ?X3 get-end) -1) (>= (send ?X3 get-end) ?X2)))) => (add-prop ?X1 (add-pred Create what (send (send ?Y1 get-to) get-what)) ?X2 1))
(defrule 9c0f5519286e446980cb37c36a11f2c1 (logical (object (is-a Proposition) (subject ?X1&:(or (eq (class ?X1) Person) (subclassp (class ?X1) Person))) (predicate ?Y4&:(or (eq (class ?Y4) Wants) (subclassp (class ?Y4) Wants))) (time ?X2) (truth 1))) (logical (object (is-a Proposition) (subject ?X1) (predicate ?Y5&:(or (eq (class ?Y5) Can) (subclassp (class ?Y5) Can))&:(eq (send ?Y4 get-to) (send ?Y5 get-what))) (time ?X3&:(or (eq (class ?X3) Duration) (subclassp (class ?X3) Duration))) (truth 1))) (test (and (<= (send ?X3 get-start) ?X2) (or (= (send ?X3 get-end) -1) (>= (send ?X3 get-end) ?X2)))) => (add-prop ?X1 (send ?Y4 get-to) ?X2 1))
(defrule 3955c2d4a7b04e038e81ee208e0eb8f0 (logical (object (is-a Proposition) (subject ?X2&:(or (eq (class ?X2) Thing) (subclassp (class ?X2) Thing))) (predicate ?Y7&:(or (eq (class ?Y7) IsNeeded) (subclassp (class ?Y7) IsNeeded))) (time ?X3&:(or (eq (class ?X3) Duration) (subclassp (class ?X3) Duration))) (truth 1))) (logical (object (is-a Proposition) (subject ?X1&:(or (eq (class ?X1) Thing) (subclassp (class ?X1) Thing))) (predicate ?Y9&:(or (eq (class ?Y9) Has) (subclassp (class ?Y9) Has))&:(eq (send ?Y9 get-what) ?X2)) (time ?X5&:(or (eq (class ?X5) Duration) (subclassp (class ?X5) Duration))) (truth 1))) (test (or (and (>= (send ?X3 get-start) (send ?X3 get-start)) (or (<= (send ?X3 get-start) (send ?X3 get-end)) (= (send ?X3 get-end) -1))) (and (>= (send ?X3 get-start) (send ?X3 get-start)) (or (<= (send ?X3 get-start) (send ?X3 get-end)) (= (send ?X3 get-end) -1))))) => (add-prop ?X1 (add-pred Can what (send ?Y7 get-for_action)) (make-instance of Duration (start (mincomstart ?X3 ?X5)) (end (maxcomend ?X3 ?X5))) 1))
(defrule f927b5541ae944d88f44cb2e007c8a20 (logical (object (is-a Proposition) (subject ?X1&:(or (eq (class ?X1) Thing) (subclassp (class ?X1) Thing))) (predicate ?Y11&:(or (eq (class ?Y11) IsIn) (subclassp (class ?Y11) IsIn))&:(or (eq (class (send ?Y11 get-what)) Thing) (subclassp (class (send ?Y11 get-what)) Thing))) (time ?X4&:(or (eq (class ?X4) Duration) (subclassp (class ?X4) Duration))) (truth 1))) (logical (object (is-a Proposition) (subject ?X2&:(eq ?X2 (send ?Y11 get-what))) (predicate ?Y13&:(or (eq (class ?Y13) IsIn) (subclassp (class ?Y13) IsIn))&:(or (eq (class (send ?Y13 get-what)) Thing) (subclassp (class (send ?Y13 get-what)) Thing))) (time ?X5&:(or (eq (class ?X5) Duration) (subclassp (class ?X5) Duration))) (truth 1))) (test (or (and (>= (send ?X4 get-start) (send ?X4 get-start)) (or (<= (send ?X4 get-start) (send ?X4 get-end)) (= (send ?X4 get-end) -1))) (and (>= (send ?X4 get-start) (send ?X4 get-start)) (or (<= (send ?X4 get-start) (send ?X4 get-end)) (= (send ?X4 get-end) -1))))) => (add-prop ?X1 (add-pred IsIn what (send ?Y13 get-what)) (make-instance of Duration (start (mincomstart ?X4 ?X5)) (end (maxcomend ?X4 ?X5))) 1))
(defrule 6e70b0ec62304a8381100d576208381e (logical (object (is-a Proposition) (subject ?X1&:(or (eq (class ?X1) Person) (subclassp (class ?X1) Person))) (predicate ?Y15&:(or (eq (class ?Y15) IsIn) (subclassp (class ?Y15) IsIn))) (time ?X3&:(or (eq (class ?X3) Duration) (subclassp (class ?X3) Duration))) (truth 1))) (logical (object (is-a Proposition) (subject ?X2&:(or (eq (class ?X2) Group) (subclassp (class ?X2) Group))) (predicate ?Y17&:(or (eq (class ?Y17) Has) (subclassp (class ?Y17) Has))&:(or (eq (class (send ?Y17 get-what)) Permission) (subclassp (class (send ?Y17 get-what)) Permission))) (time ?X5&:(or (eq (class ?X5) Duration) (subclassp (class ?X5) Duration))) (truth 1))) (test (or (and (>= (send ?X3 get-start) (send ?X3 get-start)) (or (<= (send ?X3 get-start) (send ?X3 get-end)) (= (send ?X3 get-end) -1))) (and (>= (send ?X3 get-start) (send ?X3 get-start)) (or (<= (send ?X3 get-start) (send ?X3 get-end)) (= (send ?X3 get-end) -1))))) => (add-prop ?X1 (add-pred Has what (send ?Y17 get-what)) (make-instance of Duration (start (mincomstart ?X3 ?X5)) (end (maxcomend ?X3 ?X5))) 1))
(defrule 7e39eda9516242ac9641b17dba99e7b1 (logical (object (is-a Proposition) (subject ?X1&:(or (eq (class ?X1) Person) (subclassp (class ?X1) Person))) (predicate ?Y19&:(or (eq (class ?Y19) Has) (subclassp (class ?Y19) Has))&:(or (eq (class (send ?Y19 get-what)) Role) (subclassp (class (send ?Y19 get-what)) Role))) (time ?X3&:(or (eq (class ?X3) Duration) (subclassp (class ?X3) Duration))) (truth 1))) (logical (object (is-a Proposition) (subject ?X2&:(eq ?X2 (send ?Y19 get-what))) (predicate ?Y21&:(or (eq (class ?Y21) Has) (subclassp (class ?Y21) Has))&:(or (eq (class (send ?Y21 get-what)) Permission) (subclassp (class (send ?Y21 get-what)) Permission))) (time ?X5&:(or (eq (class ?X5) Duration) (subclassp (class ?X5) Duration))) (truth 1))) (test (or (and (>= (send ?X3 get-start) (send ?X3 get-start)) (or (<= (send ?X3 get-start) (send ?X3 get-end)) (= (send ?X3 get-end) -1))) (and (>= (send ?X3 get-start) (send ?X3 get-start)) (or (<= (send ?X3 get-start) (send ?X3 get-end)) (= (send ?X3 get-end) -1))))) => (add-prop ?X1 (add-pred Has what (send ?Y21 get-what)) (make-instance of Duration (start (mincomstart ?X3 ?X5)) (end (maxcomend ?X3 ?X5))) 1))
(defrule b1c3efa1368b47d7b92c4c5160aaf1ce (logical (object (is-a Person) (name ?X1))) => (add-prop ?X1 (add-pred Has what [member]) (make-instance of Duration (start 733697.0) (end -1.0)) 1))
(defrule 45ed7deab8ae4c80a1967d85462e2a8b (logical (object (is-a Proposition) (subject ?X1&:(or (eq (class ?X1) Person) (subclassp (class ?X1) Person))) (predicate ?Y23&:(or (eq (class ?Y23) Create) (subclassp (class ?Y23) Create))&:(or (eq (class (send ?Y23 get-what)) Content) (subclassp (class (send ?Y23 get-what)) Content))) (time ?X3) (truth 1))) => (reduce-class (send ?Y23 get-what) Content) (add-prop ?X1 (add-pred IsOwner of (send ?Y23 get-what)) (make-instance of Duration (start ?X3) (end -1.0)) 1) (add-prop (send ?Y23 get-what) (add-pred Has what [private]) (make-instance of Duration (start ?X3) (end -1.0)) 1))
(defrule a51ba9d53a7743a9acea13c53e0f5079 (logical (object (is-a Permission) (name ?X2))) => (add-prop [manager] (add-pred Has what ?X2) (make-instance of Duration (start 733697.0) (end -1.0)) 1))
(defrule 340ffc58ebdc495fb54260fb9bf5ced7 (logical (object (is-a Proposition) (subject ?X1&:(or (eq (class ?X1) Content) (subclassp (class ?X1) Content))) (predicate ?Y24&:(or (eq (class ?Y24) Has) (subclassp (class ?Y24) Has))&:(eq (send ?Y24 get-what) [public])) (time ?X2&:(or (eq (class ?X2) Duration) (subclassp (class ?X2) Duration))) (truth 1))) => (add-prop [basic_perm] (add-pred IsNeeded for_action (add-pred View what ?X1)) ?X2 1))
(defrule fad616d0adda44439b871aca7912b67e (logical (object (is-a Proposition) (subject ?X1&:(or (eq (class ?X1) Content) (subclassp (class ?X1) Content))) (predicate ?Y26&:(or (eq (class ?Y26) Has) (subclassp (class ?Y26) Has))&:(eq (send ?Y26 get-what) [private])) (time ?X2&:(or (eq (class ?X2) Duration) (subclassp (class ?X2) Duration))) (truth 1))) => (add-prop [manage_perm] (add-pred IsNeeded for_action (add-pred View what ?X1)) ?X2 1))
(defrule 00d7277909c14945bf280ba54151447c (logical (object (is-a Proposition) (subject ?X1&:(or (eq (class ?X1) Content) (subclassp (class ?X1) Content))) (predicate ?Y28&:(or (eq (class ?Y28) Has) (subclassp (class ?Y28) Has))&:(eq (send ?Y28 get-what) [private])) (time ?X3&:(or (eq (class ?X3) Duration) (subclassp (class ?X3) Duration))) (truth 1))) (logical (object (is-a Proposition) (subject ?X2&:(or (eq (class ?X2) Person) (subclassp (class ?X2) Person))) (predicate ?Y30&:(or (eq (class ?Y30) IsOwner) (subclassp (class ?Y30) IsOwner))&:(eq (send ?Y30 get-of) ?X1)) (time ?X4&:(or (eq (class ?X4) Duration) (subclassp (class ?X4) Duration))) (truth 1))) (test (or (and (>= (send ?X3 get-start) (send ?X3 get-start)) (or (<= (send ?X3 get-start) (send ?X3 get-end)) (= (send ?X3 get-end) -1))) (and (>= (send ?X3 get-start) (send ?X3 get-start)) (or (<= (send ?X3 get-start) (send ?X3 get-end)) (= (send ?X3 get-end) -1))))) => (add-prop ?X2 (add-pred Can what (add-pred View what ?X1)) (make-instance of Duration (start (mincomstart ?X3 ?X4)) (end (maxcomend ?X3 ?X4))) 1))
(defrule 54ce76933c914a06933be3a6661b3b27 (logical (object (is-a Proposition) (subject ?X1&:(or (eq (class ?X1) Person) (subclassp (class ?X1) Person))) (predicate ?Y32&:(or (eq (class ?Y32) Publish) (subclassp (class ?Y32) Publish))&:(or (eq (class (send ?Y32 get-what)) Content) (subclassp (class (send ?Y32 get-what)) Content))) (time ?X3) (truth 1))) (logical (object (is-a Proposition) (subject ?X2&:(eq ?X2 (send ?Y32 get-what))) (predicate ?Y33&:(or (eq (class ?Y33) Has) (subclassp (class ?Y33) Has))&:(or (eq (class (send ?Y33 get-what)) Status) (subclassp (class (send ?Y33 get-what)) Status))) (time ?X5&:(or (eq (class ?X5) Duration) (subclassp (class ?X5) Duration))) (truth 1))) => (add-prop (send ?Y32 get-what) (add-pred Has what [public]) (make-instance of Duration (start ?X3) (end -1.0)) 1))
(defrule 9c30753870fc416ba9c4840dafd86bcf (logical (object (is-a Content) (name ?X1))) => (add-prop [manage_perm] (add-pred IsNeeded for_action (add-pred Publish what ?X1)) (make-instance of Duration (start 733697.0) (end -1.0)) 1))
(defrule 5100aeca35e74034b363da7d0e913ff4 (logical (object (is-a Proposition) (subject ?X1&:(or (eq (class ?X1) Person) (subclassp (class ?X1) Person))) (predicate ?Y35&:(or (eq (class ?Y35) Hide) (subclassp (class ?Y35) Hide))&:(or (eq (class (send ?Y35 get-what)) Content) (subclassp (class (send ?Y35 get-what)) Content))) (time ?X3) (truth 1))) (logical (object (is-a Proposition) (subject ?X2&:(eq ?X2 (send ?Y35 get-what))) (predicate ?Y36&:(or (eq (class ?Y36) Has) (subclassp (class ?Y36) Has))&:(or (eq (class (send ?Y36 get-what)) Status) (subclassp (class (send ?Y36 get-what)) Status))) (time ?X5&:(or (eq (class ?X5) Duration) (subclassp (class ?X5) Duration))) (truth 1))) => (send ?X5 put-end 733697) (add-prop (send ?Y35 get-what) (add-pred Has what [private]) (make-instance of Duration (start ?X3) (end -1.0)) 1))
(defrule c9c40941a9284f4b8b61e84b28a0528f (logical (object (is-a Proposition) (subject ?X1&:(or (eq (class ?X1) Person) (subclassp (class ?X1) Person))) (predicate ?Y38&:(or (eq (class ?Y38) IsOwner) (subclassp (class ?Y38) IsOwner))&:(or (eq (class (send ?Y38 get-of)) Content) (subclassp (class (send ?Y38 get-of)) Content))) (time ?X3&:(or (eq (class ?X3) Duration) (subclassp (class ?X3) Duration))) (truth 1))) => (add-prop ?X1 (add-pred Can what (add-pred Hide what (send ?Y38 get-of))) ?X3 1))
(reduce-class [john] Person)
john
(reduce-class [pete] Person)
pete
(reduce-class [jane] Person)
jane
(reduce-class [c1] Content)
c1
(reduce-class [c2] Content)
c2
(add-prop [john] (add-pred Has what [manager]) (make-instance of Duration (start 733697.0) (end -1.0)) 1)
john has what manager at from 733697.0 till -1.0
(add-prop [jane] (add-pred Has what [create_perm]) (make-instance of Duration (start 733697.0) (end -1.0)) 1)
jane has what create_perm at from 733697.0 till -1.0
(add-prop [jane] (add-pred Wants to (add-pred Create what [c1])) 733697.0 1)
jane wants to create what c1 at 733697.0
(add-prop [pete] (add-pred Wants to (add-pred Create what [c2])) 733697.0 1)
pete wants to create what c2 at 733697.0
----------running---------------------
jane create what c1 at 733697.0
jane isowner of c1 at from 733697.0 till -1.0
c1 has what private at from 733697.0 till -1.0
jane can what view what c1 at from 733697.0 till -1.0
manage_perm isneeded for_action view what c1 at from 733697.0 till -1.0
jane can what hide what c1 at from 733697.0 till -1.0
manage_perm isneeded for_action publish what c2 at from 733697.0 till -1.0
manage_perm isneeded for_action publish what c1 at from 733697.0 till -1.0
jane has what member at from 733697.0 till -1.0
jane has what basic_perm at from 733697.0 till -1.0
pete has what member at from 733697.0 till -1.0
pete has what basic_perm at from 733697.0 till -1.0
john has what member at from 733697.0 till -1.0
john has what basic_perm at from 733697.0 till -1.0
manager has what create_perm at from 733697.0 till -1.0
admin has what create_perm at from 733697.0 till -1.0
john has what create_perm at from 733697.0 till -1.0
manager has what manage_perm at from 733697.0 till -1.0
manager can what view what c1 at from 733697.0 till -1.0
manager can what publish what c2 at from 733697.0 till -1.0
manager can what publish what c1 at from 733697.0 till -1.0
admin has what manage_perm at from 733697.0 till -1.0
admin can what view what c1 at from 733697.0 till -1.0
admin can what publish what c2 at from 733697.0 till -1.0
admin can what publish what c1 at from 733697.0 till -1.0
john has what manage_perm at from 733697.0 till -1.0
john can what view what c1 at from 733697.0 till -1.0
john can what publish what c2 at from 733697.0 till -1.0
john can what publish what c1 at from 733697.0 till -1.0
manager has what basic_perm at from 733697.0 till -1.0
admin has what basic_perm at from 733697.0 till -1.0
manager has what member at from 733697.0 till -1.0
member has what member at from 733697.0 till -1.0
admin has what member at from 733697.0 till -1.0
----------run---------------------
(find-all-instances ((?prop Proposition) (?Y40 IsOwner) (?Y41 Duration)) (and (eq ?prop:subject [jane]) (eq ?Y40:of [c1]) (eq ?prop:predicate ?Y40) (= ?Y41:start 733697.0) (= ?Y41:end -1.0) (eq ?prop:truth 1)))
39


jane isowner of c1 at from 733697.0 till -1.0
jane isowner of c1 at from 733697.0 till -1.0
jane isowner of c1 at from 733697.0 till -1.0
jane isowner of c1 at from 733697.0 till -1.0
jane isowner of c1 at from 733697.0 till -1.0
jane isowner of c1 at from 733697.0 till -1.0
jane isowner of c1 at from 733697.0 till -1.0
jane isowner of c1 at from 733697.0 till -1.0
jane isowner of c1 at from 733697.0 till -1.0
jane isowner of c1 at from 733697.0 till -1.0
jane isowner of c1 at from 733697.0 till -1.0
jane isowner of c1 at from 733697.0 till -1.0
jane isowner of c1 at from 733697.0 till -1.0
jane isowner of c1 at from 733697.0 till -1.0
jane isowner of c1 at from 733697.0 till -1.0
jane isowner of c1 at from 733697.0 till -1.0
jane isowner of c1 at from 733697.0 till -1.0
jane isowner of c1 at from 733697.0 till -1.0
jane isowner of c1 at from 733697.0 till -1.0
jane isowner of c1 at from 733697.0 till -1.0
jane isowner of c1 at from 733697.0 till -1.0
jane isowner of c1 at from 733697.0 till -1.0
jane isowner of c1 at from 733697.0 till -1.0
jane isowner of c1 at from 733697.0 till -1.0
jane isowner of c1 at from 733697.0 till -1.0
jane isowner of c1 at from 733697.0 till -1.0
jane isowner of c1 at from 733697.0 till -1.0
jane isowner of c1 at from 733697.0 till -1.0
jane isowner of c1 at from 733697.0 till -1.0
jane isowner of c1 at from 733697.0 till -1.0
jane isowner of c1 at from 733697.0 till -1.0
jane isowner of c1 at from 733697.0 till -1.0
jane isowner of c1 at from 733697.0 till -1.0
jane isowner of c1 at from 733697.0 till -1.0
jane isowner of c1 at from 733697.0 till -1.0
jane isowner of c1 at from 733697.0 till -1.0
jane isowner of c1 at from 733697.0 till -1.0
jane isowner of c1 at from 733697.0 till -1.0
jane isowner of c1 at from 733697.0 till -1.0
(add-prop [jane] (add-pred Wants to (add-pred Publish what [c1])) 733697.0 1)
jane wants to publish what c1 at 733697.0
(add-prop [pete] (add-pred Wants to (add-pred Publish what [c2])) 733697.0 1)
pete wants to publish what c2 at 733697.0
----------running---------------------
----------run---------------------
(add-prop [john] (add-pred Wants to (add-pred Publish what [c1])) 733697.0 1)
john wants to publish what c1 at 733697.0
----------running---------------------
john publish what c1 at 733697.0
c1 has what public at from 733697.0 till -1.0
basic_perm isneeded for_action view what c1 at from 733697.0 till -1.0
pete can what view what c1 at from 733697.0 till -1.0
member can what view what c1 at from 733697.0 till -1.0
----------run---------------------
(find-all-instances ((?prop Proposition) (?Y42 Has) (?Y43 Duration)) (and (eq ?prop:subject [c1]) (eq ?Y42:what [private]) (eq ?prop:predicate ?Y42) (= ?Y43:start 733697.0) (= ?Y43:end -1.0) (eq ?prop:truth 1)))
47


c1 has what private at from 733697.0 till -1.0
c1 has what private at from 733697.0 till -1.0
c1 has what private at from 733697.0 till -1.0
c1 has what private at from 733697.0 till -1.0
c1 has what private at from 733697.0 till -1.0
c1 has what private at from 733697.0 till -1.0
c1 has what private at from 733697.0 till -1.0
c1 has what private at from 733697.0 till -1.0
c1 has what private at from 733697.0 till -1.0
c1 has what private at from 733697.0 till -1.0
c1 has what private at from 733697.0 till -1.0
c1 has what private at from 733697.0 till -1.0
c1 has what private at from 733697.0 till -1.0
c1 has what private at from 733697.0 till -1.0
c1 has what private at from 733697.0 till -1.0
c1 has what private at from 733697.0 till -1.0
c1 has what private at from 733697.0 till -1.0
c1 has what private at from 733697.0 till -1.0
c1 has what private at from 733697.0 till -1.0
c1 has what private at from 733697.0 till -1.0
c1 has what private at from 733697.0 till -1.0
c1 has what private at from 733697.0 till -1.0
c1 has what private at from 733697.0 till -1.0
c1 has what private at from 733697.0 till -1.0
c1 has what private at from 733697.0 till -1.0
c1 has what private at from 733697.0 till -1.0
c1 has what private at from 733697.0 till -1.0
c1 has what private at from 733697.0 till -1.0
c1 has what private at from 733697.0 till -1.0
c1 has what private at from 733697.0 till -1.0
c1 has what private at from 733697.0 till -1.0
c1 has what private at from 733697.0 till -1.0
c1 has what private at from 733697.0 till -1.0
c1 has what private at from 733697.0 till -1.0
c1 has what private at from 733697.0 till -1.0
c1 has what private at from 733697.0 till -1.0
c1 has what private at from 733697.0 till -1.0
c1 has what private at from 733697.0 till -1.0
c1 has what private at from 733697.0 till -1.0
c1 has what private at from 733697.0 till -1.0
c1 has what private at from 733697.0 till -1.0
c1 has what private at from 733697.0 till -1.0
c1 has what private at from 733697.0 till -1.0
c1 has what private at from 733697.0 till -1.0
c1 has what private at from 733697.0 till -1.0
c1 has what private at from 733697.0 till -1.0
c1 has what private at from 733697.0 till -1.0
(defclass Name (is-a USER))
(deffunction reduce-class (?instance ?class) (if (eq (length$ (find-instance ((?a ?class)) (eq (instance-name ?a) ?instance))) 0) then (make-instance ?instance of ?class) (python-call tonl ?class ?instance)))
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


(deffunction mincomstart (?dur1 ?dur2)
    (return (max (send ?dur1 get-start) (send ?dur2 get-start)))
)



(deffunction maxcomend (?dur1 ?dur2)
    (bind ?e1 (send ?dur1 get-end))
    (bind ?e2 (send ?dur2 get-end))
    (if (= ?e1 ?e2) then (return ?e1))
    (if (= ?e2 -1) then
        (return ?e1)
    )
    (if (= ?e1 -1) then
        (return ?e2)
    )
    (return (min ?e1 ?e2))
)

(defclass Proposition (is-a Name) (slot truth (type INTEGER) (default 1) (pattern-match reactive)) (slot subject (type INSTANCE) (pattern-match reactive)) (slot predicate (type INSTANCE) (pattern-match reactive)) (slot time (type ?VARIABLE) (pattern-match reactive)))

(deffunction add-prop (?s ?p ?t ?r)
       (if (and (python-call ptonl ?s ?p ?t ?r)
                (= (+ (length$ (find-instance ((?prop Proposition) (?dur Duration))
                          (and (eq ?prop:subject ?s)
                               (eq ?prop:predicate ?p)
                               (eq ?prop:time ?dur)
                               (= ?dur:start (send ?t get-start))
                               (= ?dur:end (send ?t get-end))
                               (eq ?prop:truth ?r))))
                      (length$ (find-instance ((?prop Proposition))
                          (and (eq ?prop:subject ?s)
                               (eq ?prop:predicate ?p)
                               (= ?prop:time ?t)
                               (eq ?prop:truth ?r)))))
                 0))
        then (make-instance of Proposition (subject ?s)
                                           (predicate ?p)
                                           (time ?t)
                                           (truth ?r))
        else (return TRUE)))
------------OPEN---------------------
----------F-OPEN---------------------
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
admin
(reduce-class [member] Role)
member
(reduce-class [manager] Role)
manager
(reduce-class [basic_perm] Permission)
basic_perm
(reduce-class [manage_perm] Permission)
manage_perm
(reduce-class [create_perm] Permission)
create_perm
(reduce-class [public] Status)
public
(reduce-class [private] Status)
private
(add-prop [admin] (add-pred Has what [manager]) (make-instance of Duration (start 733697.0) (end -1.0)) 1)
admin has what manager at from 733697.0 till -1.0
(add-prop [member] (add-pred Has what [basic_perm]) (make-instance of Duration (start 733697.0) (end -1.0)) 1)
member has what basic_perm at from 733697.0 till -1.0
(defrule c74b1e7e306f4ddf9bb23f112c8a607a (logical (object (is-a Proposition) (subject ?X1&:(or (eq (class ?X1) Person) (subclassp (class ?X1) Person))) (predicate ?Y1&:(or (eq (class ?Y1) Wants) (subclassp (class ?Y1) Wants))&:(or (eq (class (send ?Y1 get-to)) Create) (subclassp (class (send ?Y1 get-to)) Create))&:(or (eq (class (send (send ?Y1 get-to) get-what)) Thing) (subclassp (class (send (send ?Y1 get-to) get-what)) Thing))) (time ?X2) (truth 1))) (logical (object (is-a Proposition) (subject ?X1) (predicate ?Y2&:(or (eq (class ?Y2) Has) (subclassp (class ?Y2) Has))&:(eq (send ?Y2 get-what) [create_perm])) (time ?X3&:(or (eq (class ?X3) Duration) (subclassp (class ?X3) Duration))) (truth 1))) (test (and (<= (send ?X3 get-start) ?X2) (or (= (send ?X3 get-end) -1) (>= (send ?X3 get-end) ?X2)))) => (add-prop ?X1 (add-pred Create what (send (send ?Y1 get-to) get-what)) ?X2 1))
(defrule 8f9a21314291460586d86b522de3e5ab (logical (object (is-a Proposition) (subject ?X1&:(or (eq (class ?X1) Person) (subclassp (class ?X1) Person))) (predicate ?Y4&:(or (eq (class ?Y4) Wants) (subclassp (class ?Y4) Wants))) (time ?X2) (truth 1))) (logical (object (is-a Proposition) (subject ?X1) (predicate ?Y5&:(or (eq (class ?Y5) Can) (subclassp (class ?Y5) Can))&:(eq (send ?Y4 get-to) (send ?Y5 get-what))) (time ?X3&:(or (eq (class ?X3) Duration) (subclassp (class ?X3) Duration))) (truth 1))) (test (and (<= (send ?X3 get-start) ?X2) (or (= (send ?X3 get-end) -1) (>= (send ?X3 get-end) ?X2)))) => (add-prop ?X1 (send ?Y4 get-to) ?X2 1))
(defrule f2ccb804232d43ca8e3c0c1bce63d285 (logical (object (is-a Proposition) (subject ?X2&:(or (eq (class ?X2) Thing) (subclassp (class ?X2) Thing))) (predicate ?Y7&:(or (eq (class ?Y7) IsNeeded) (subclassp (class ?Y7) IsNeeded))) (time ?X3&:(or (eq (class ?X3) Duration) (subclassp (class ?X3) Duration))) (truth 1))) (logical (object (is-a Proposition) (subject ?X1&:(or (eq (class ?X1) Thing) (subclassp (class ?X1) Thing))) (predicate ?Y9&:(or (eq (class ?Y9) Has) (subclassp (class ?Y9) Has))&:(eq (send ?Y9 get-what) ?X2)) (time ?X5&:(or (eq (class ?X5) Duration) (subclassp (class ?X5) Duration))) (truth 1))) (test (or (and (>= (send ?X3 get-start) (send ?X3 get-start)) (or (<= (send ?X3 get-start) (send ?X3 get-end)) (= (send ?X3 get-end) -1))) (and (>= (send ?X3 get-start) (send ?X3 get-start)) (or (<= (send ?X3 get-start) (send ?X3 get-end)) (= (send ?X3 get-end) -1))))) => (add-prop ?X1 (add-pred Can what (send ?Y7 get-for_action)) (make-instance of Duration (start (mincomstart ?X3 ?X5)) (end (maxcomend ?X3 ?X5))) 1))
(defrule 967457cddb594a2c852ab2638e375fae (logical (object (is-a Proposition) (subject ?X1&:(or (eq (class ?X1) Thing) (subclassp (class ?X1) Thing))) (predicate ?Y11&:(or (eq (class ?Y11) IsIn) (subclassp (class ?Y11) IsIn))&:(or (eq (class (send ?Y11 get-what)) Thing) (subclassp (class (send ?Y11 get-what)) Thing))) (time ?X4&:(or (eq (class ?X4) Duration) (subclassp (class ?X4) Duration))) (truth 1))) (logical (object (is-a Proposition) (subject ?X2&:(eq ?X2 (send ?Y11 get-what))) (predicate ?Y13&:(or (eq (class ?Y13) IsIn) (subclassp (class ?Y13) IsIn))&:(or (eq (class (send ?Y13 get-what)) Thing) (subclassp (class (send ?Y13 get-what)) Thing))) (time ?X5&:(or (eq (class ?X5) Duration) (subclassp (class ?X5) Duration))) (truth 1))) (test (or (and (>= (send ?X4 get-start) (send ?X4 get-start)) (or (<= (send ?X4 get-start) (send ?X4 get-end)) (= (send ?X4 get-end) -1))) (and (>= (send ?X4 get-start) (send ?X4 get-start)) (or (<= (send ?X4 get-start) (send ?X4 get-end)) (= (send ?X4 get-end) -1))))) => (add-prop ?X1 (add-pred IsIn what (send ?Y13 get-what)) (make-instance of Duration (start (mincomstart ?X4 ?X5)) (end (maxcomend ?X4 ?X5))) 1))
(defrule 4f4e7439e8f247d59b1801d40671eda6 (logical (object (is-a Proposition) (subject ?X1&:(or (eq (class ?X1) Person) (subclassp (class ?X1) Person))) (predicate ?Y15&:(or (eq (class ?Y15) IsIn) (subclassp (class ?Y15) IsIn))) (time ?X3&:(or (eq (class ?X3) Duration) (subclassp (class ?X3) Duration))) (truth 1))) (logical (object (is-a Proposition) (subject ?X2&:(or (eq (class ?X2) Group) (subclassp (class ?X2) Group))) (predicate ?Y17&:(or (eq (class ?Y17) Has) (subclassp (class ?Y17) Has))&:(or (eq (class (send ?Y17 get-what)) Permission) (subclassp (class (send ?Y17 get-what)) Permission))) (time ?X5&:(or (eq (class ?X5) Duration) (subclassp (class ?X5) Duration))) (truth 1))) (test (or (and (>= (send ?X3 get-start) (send ?X3 get-start)) (or (<= (send ?X3 get-start) (send ?X3 get-end)) (= (send ?X3 get-end) -1))) (and (>= (send ?X3 get-start) (send ?X3 get-start)) (or (<= (send ?X3 get-start) (send ?X3 get-end)) (= (send ?X3 get-end) -1))))) => (add-prop ?X1 (add-pred Has what (send ?Y17 get-what)) (make-instance of Duration (start (mincomstart ?X3 ?X5)) (end (maxcomend ?X3 ?X5))) 1))
(defrule befe767823b04b8c8b359bad8bb8297d (logical (object (is-a Proposition) (subject ?X1&:(or (eq (class ?X1) Person) (subclassp (class ?X1) Person))) (predicate ?Y19&:(or (eq (class ?Y19) Has) (subclassp (class ?Y19) Has))&:(or (eq (class (send ?Y19 get-what)) Role) (subclassp (class (send ?Y19 get-what)) Role))) (time ?X3&:(or (eq (class ?X3) Duration) (subclassp (class ?X3) Duration))) (truth 1))) (logical (object (is-a Proposition) (subject ?X2&:(eq ?X2 (send ?Y19 get-what))) (predicate ?Y21&:(or (eq (class ?Y21) Has) (subclassp (class ?Y21) Has))&:(or (eq (class (send ?Y21 get-what)) Permission) (subclassp (class (send ?Y21 get-what)) Permission))) (time ?X5&:(or (eq (class ?X5) Duration) (subclassp (class ?X5) Duration))) (truth 1))) (test (or (and (>= (send ?X3 get-start) (send ?X3 get-start)) (or (<= (send ?X3 get-start) (send ?X3 get-end)) (= (send ?X3 get-end) -1))) (and (>= (send ?X3 get-start) (send ?X3 get-start)) (or (<= (send ?X3 get-start) (send ?X3 get-end)) (= (send ?X3 get-end) -1))))) => (add-prop ?X1 (add-pred Has what (send ?Y21 get-what)) (make-instance of Duration (start (mincomstart ?X3 ?X5)) (end (maxcomend ?X3 ?X5))) 1))
(defrule 4f6e5b65120b4a27bdb6a1064e3fa19e (logical (object (is-a Person) (name ?X1))) => (add-prop ?X1 (add-pred Has what [member]) (make-instance of Duration (start 733697.0) (end -1.0)) 1))
(defrule d01a1ec2425344b89e5e1febbf5be882 (logical (object (is-a Proposition) (subject ?X1&:(or (eq (class ?X1) Person) (subclassp (class ?X1) Person))) (predicate ?Y23&:(or (eq (class ?Y23) Create) (subclassp (class ?Y23) Create))&:(or (eq (class (send ?Y23 get-what)) Content) (subclassp (class (send ?Y23 get-what)) Content))) (time ?X3) (truth 1))) => (reduce-class (send ?Y23 get-what) Content) (add-prop ?X1 (add-pred IsOwner of (send ?Y23 get-what)) (make-instance of Duration (start ?X3) (end -1.0)) 1) (add-prop (send ?Y23 get-what) (add-pred Has what [private]) (make-instance of Duration (start ?X3) (end -1.0)) 1))
(defrule 959fd9cdbddc42bda936e90923fac907 (logical (object (is-a Permission) (name ?X2))) => (add-prop [manager] (add-pred Has what ?X2) (make-instance of Duration (start 733697.0) (end -1.0)) 1))
(defrule b1a10c092b6e4ebcacba103031688f4f (logical (object (is-a Proposition) (subject ?X1&:(or (eq (class ?X1) Content) (subclassp (class ?X1) Content))) (predicate ?Y24&:(or (eq (class ?Y24) Has) (subclassp (class ?Y24) Has))&:(eq (send ?Y24 get-what) [public])) (time ?X2&:(or (eq (class ?X2) Duration) (subclassp (class ?X2) Duration))) (truth 1))) => (add-prop [basic_perm] (add-pred IsNeeded for_action (add-pred View what ?X1)) ?X2 1))
(defrule 10184c6ab6754dadaf7baa136f647bae (logical (object (is-a Proposition) (subject ?X1&:(or (eq (class ?X1) Content) (subclassp (class ?X1) Content))) (predicate ?Y26&:(or (eq (class ?Y26) Has) (subclassp (class ?Y26) Has))&:(eq (send ?Y26 get-what) [private])) (time ?X2&:(or (eq (class ?X2) Duration) (subclassp (class ?X2) Duration))) (truth 1))) => (add-prop [manage_perm] (add-pred IsNeeded for_action (add-pred View what ?X1)) ?X2 1))
(defrule 8032f5f274644a93b88b65837ab02018 (logical (object (is-a Proposition) (subject ?X1&:(or (eq (class ?X1) Content) (subclassp (class ?X1) Content))) (predicate ?Y28&:(or (eq (class ?Y28) Has) (subclassp (class ?Y28) Has))&:(eq (send ?Y28 get-what) [private])) (time ?X3&:(or (eq (class ?X3) Duration) (subclassp (class ?X3) Duration))) (truth 1))) (logical (object (is-a Proposition) (subject ?X2&:(or (eq (class ?X2) Person) (subclassp (class ?X2) Person))) (predicate ?Y30&:(or (eq (class ?Y30) IsOwner) (subclassp (class ?Y30) IsOwner))&:(eq (send ?Y30 get-of) ?X1)) (time ?X4&:(or (eq (class ?X4) Duration) (subclassp (class ?X4) Duration))) (truth 1))) (test (or (and (>= (send ?X3 get-start) (send ?X3 get-start)) (or (<= (send ?X3 get-start) (send ?X3 get-end)) (= (send ?X3 get-end) -1))) (and (>= (send ?X3 get-start) (send ?X3 get-start)) (or (<= (send ?X3 get-start) (send ?X3 get-end)) (= (send ?X3 get-end) -1))))) => (add-prop ?X2 (add-pred Can what (add-pred View what ?X1)) (make-instance of Duration (start (mincomstart ?X3 ?X4)) (end (maxcomend ?X3 ?X4))) 1))
(defrule b6e2c12d83ff4afe9ee0d770967a8b2a (logical (object (is-a Proposition) (subject ?X1&:(or (eq (class ?X1) Person) (subclassp (class ?X1) Person))) (predicate ?Y32&:(or (eq (class ?Y32) Publish) (subclassp (class ?Y32) Publish))&:(or (eq (class (send ?Y32 get-what)) Content) (subclassp (class (send ?Y32 get-what)) Content))) (time ?X3) (truth 1))) (logical (object (is-a Proposition) (subject ?X2&:(eq ?X2 (send ?Y32 get-what))) (predicate ?Y33&:(or (eq (class ?Y33) Has) (subclassp (class ?Y33) Has))&:(or (eq (class (send ?Y33 get-what)) Status) (subclassp (class (send ?Y33 get-what)) Status))) (time ?X5&:(or (eq (class ?X5) Duration) (subclassp (class ?X5) Duration))) (truth 1))) => (send ?X5 put-end 733697) (add-prop (send ?Y32 get-what) (add-pred Has what [public]) (make-instance of Duration (start ?X3) (end -1.0)) 1))
(defrule d12ef29228ac45798ddcd4d0b3a3c5be (logical (object (is-a Content) (name ?X1))) => (add-prop [manage_perm] (add-pred IsNeeded for_action (add-pred Publish what ?X1)) (make-instance of Duration (start 733697.0) (end -1.0)) 1))
(defrule b7431b4eb98941ad8f2f87a58f2993c9 (logical (object (is-a Proposition) (subject ?X1&:(or (eq (class ?X1) Person) (subclassp (class ?X1) Person))) (predicate ?Y35&:(or (eq (class ?Y35) Hide) (subclassp (class ?Y35) Hide))&:(or (eq (class (send ?Y35 get-what)) Content) (subclassp (class (send ?Y35 get-what)) Content))) (time ?X3) (truth 1))) (logical (object (is-a Proposition) (subject ?X2&:(eq ?X2 (send ?Y35 get-what))) (predicate ?Y36&:(or (eq (class ?Y36) Has) (subclassp (class ?Y36) Has))&:(or (eq (class (send ?Y36 get-what)) Status) (subclassp (class (send ?Y36 get-what)) Status))) (time ?X5&:(or (eq (class ?X5) Duration) (subclassp (class ?X5) Duration))) (truth 1))) => (send ?X5 put-end 733697) (add-prop (send ?Y35 get-what) (add-pred Has what [private]) (make-instance of Duration (start ?X3) (end -1.0)) 1))
(defrule 62737c8ccdbe4340b03089d3e4d27ab6 (logical (object (is-a Proposition) (subject ?X1&:(or (eq (class ?X1) Person) (subclassp (class ?X1) Person))) (predicate ?Y38&:(or (eq (class ?Y38) IsOwner) (subclassp (class ?Y38) IsOwner))&:(or (eq (class (send ?Y38 get-of)) Content) (subclassp (class (send ?Y38 get-of)) Content))) (time ?X3&:(or (eq (class ?X3) Duration) (subclassp (class ?X3) Duration))) (truth 1))) => (add-prop ?X1 (add-pred Can what (add-pred Hide what (send ?Y38 get-of))) ?X3 1))
(reduce-class [john] Person)
john
(reduce-class [pete] Person)
pete
(reduce-class [jane] Person)
jane
(reduce-class [c1] Content)
c1
(reduce-class [c2] Content)
c2
(add-prop [john] (add-pred Has what [manager]) (make-instance of Duration (start 733697.0) (end -1.0)) 1)
john has what manager at from 733697.0 till -1.0
(add-prop [jane] (add-pred Has what [create_perm]) (make-instance of Duration (start 733697.0) (end -1.0)) 1)
jane has what create_perm at from 733697.0 till -1.0
(add-prop [jane] (add-pred Wants to (add-pred Create what [c1])) 733697.0 1)
jane wants to create what c1 at 733697.0
(add-prop [pete] (add-pred Wants to (add-pred Create what [c2])) 733697.0 1)
pete wants to create what c2 at 733697.0
----------running---------------------
jane create what c1 at 733697.0
jane isowner of c1 at from 733697.0 till -1.0
c1 has what private at from 733697.0 till -1.0
jane can what view what c1 at from 733697.0 till -1.0
manage_perm isneeded for_action view what c1 at from 733697.0 till -1.0
jane can what hide what c1 at from 733697.0 till -1.0
manage_perm isneeded for_action publish what c2 at from 733697.0 till -1.0
manage_perm isneeded for_action publish what c1 at from 733697.0 till -1.0
jane has what member at from 733697.0 till -1.0
jane has what basic_perm at from 733697.0 till -1.0
pete has what member at from 733697.0 till -1.0
pete has what basic_perm at from 733697.0 till -1.0
john has what member at from 733697.0 till -1.0
john has what basic_perm at from 733697.0 till -1.0
manager has what create_perm at from 733697.0 till -1.0
admin has what create_perm at from 733697.0 till -1.0
john has what create_perm at from 733697.0 till -1.0
manager has what manage_perm at from 733697.0 till -1.0
manager can what view what c1 at from 733697.0 till -1.0
manager can what publish what c2 at from 733697.0 till -1.0
manager can what publish what c1 at from 733697.0 till -1.0
admin has what manage_perm at from 733697.0 till -1.0
admin can what view what c1 at from 733697.0 till -1.0
admin can what publish what c2 at from 733697.0 till -1.0
admin can what publish what c1 at from 733697.0 till -1.0
john has what manage_perm at from 733697.0 till -1.0
john can what view what c1 at from 733697.0 till -1.0
john can what publish what c2 at from 733697.0 till -1.0
john can what publish what c1 at from 733697.0 till -1.0
manager has what basic_perm at from 733697.0 till -1.0
admin has what basic_perm at from 733697.0 till -1.0
admin has what member at from 733697.0 till -1.0
----------runned---------------------
(find-all-instances ((?prop Proposition) (?Y40 IsOwner) (?Y41 Duration)) (and (eq ?prop:subject [jane]) (eq ?Y40:of [c1]) (eq ?prop:predicate ?Y40) (= ?Y41:start 733697.0) (= ?Y41:end -1.0) (eq ?prop:truth 1)))
35


jane isowner of c1 at from 733697.0 till -1.0
jane isowner of c1 at from 733697.0 till -1.0
jane isowner of c1 at from 733697.0 till -1.0
jane isowner of c1 at from 733697.0 till -1.0
jane isowner of c1 at from 733697.0 till -1.0
jane isowner of c1 at from 733697.0 till -1.0
jane isowner of c1 at from 733697.0 till -1.0
jane isowner of c1 at from 733697.0 till -1.0
jane isowner of c1 at from 733697.0 till -1.0
jane isowner of c1 at from 733697.0 till -1.0
jane isowner of c1 at from 733697.0 till -1.0
jane isowner of c1 at from 733697.0 till -1.0
jane isowner of c1 at from 733697.0 till -1.0
jane isowner of c1 at from 733697.0 till -1.0
jane isowner of c1 at from 733697.0 till -1.0
jane isowner of c1 at from 733697.0 till -1.0
jane isowner of c1 at from 733697.0 till -1.0
jane isowner of c1 at from 733697.0 till -1.0
jane isowner of c1 at from 733697.0 till -1.0
jane isowner of c1 at from 733697.0 till -1.0
jane isowner of c1 at from 733697.0 till -1.0
jane isowner of c1 at from 733697.0 till -1.0
jane isowner of c1 at from 733697.0 till -1.0
jane isowner of c1 at from 733697.0 till -1.0
jane isowner of c1 at from 733697.0 till -1.0
jane isowner of c1 at from 733697.0 till -1.0
jane isowner of c1 at from 733697.0 till -1.0
jane isowner of c1 at from 733697.0 till -1.0
jane isowner of c1 at from 733697.0 till -1.0
jane isowner of c1 at from 733697.0 till -1.0
jane isowner of c1 at from 733697.0 till -1.0
jane isowner of c1 at from 733697.0 till -1.0
jane isowner of c1 at from 733697.0 till -1.0
jane isowner of c1 at from 733697.0 till -1.0
jane isowner of c1 at from 733697.0 till -1.0
(add-prop [jane] (add-pred Wants to (add-pred Publish what [c1])) 733697.0 1)
jane wants to publish what c1 at 733697.0
(add-prop [pete] (add-pred Wants to (add-pred Publish what [c2])) 733697.0 1)
pete wants to publish what c2 at 733697.0
----------running---------------------
----------runned---------------------
(add-prop [john] (add-pred Wants to (add-pred Publish what [c1])) 733697.0 1)
john wants to publish what c1 at 733697.0
----------running---------------------
john publish what c1 at 733697.0
c1 has what public at from 733697.0 till -1.0
basic_perm isneeded for_action view what c1 at from 733697.0 till -1.0
pete can what view what c1 at from 733697.0 till -1.0
member can what view what c1 at from 733697.0 till -1.0
----------runned---------------------
(find-all-instances ((?prop Proposition) (?Y42 Has) (?Y43 Duration)) (and (eq ?prop:subject [c1]) (eq ?Y42:what [private]) (eq ?prop:predicate ?Y42) (= ?Y43:start 733697.0) (= ?Y43:end -1.0) (eq ?prop:truth 1)))
41


c1 has what private at from 733697.0 till 733697.0
c1 has what private at from 733697.0 till 733697.0
c1 has what private at from 733697.0 till 733697.0
c1 has what private at from 733697.0 till 733697.0
c1 has what private at from 733697.0 till 733697.0
c1 has what private at from 733697.0 till 733697.0
c1 has what private at from 733697.0 till 733697.0
c1 has what private at from 733697.0 till 733697.0
c1 has what private at from 733697.0 till 733697.0
c1 has what private at from 733697.0 till 733697.0
c1 has what private at from 733697.0 till 733697.0
c1 has what private at from 733697.0 till 733697.0
c1 has what private at from 733697.0 till 733697.0
c1 has what private at from 733697.0 till 733697.0
c1 has what private at from 733697.0 till 733697.0
c1 has what private at from 733697.0 till 733697.0
c1 has what private at from 733697.0 till 733697.0
c1 has what private at from 733697.0 till 733697.0
c1 has what private at from 733697.0 till 733697.0
c1 has what private at from 733697.0 till 733697.0
c1 has what private at from 733697.0 till 733697.0
c1 has what private at from 733697.0 till 733697.0
c1 has what private at from 733697.0 till 733697.0
c1 has what private at from 733697.0 till 733697.0
c1 has what private at from 733697.0 till 733697.0
c1 has what private at from 733697.0 till 733697.0
c1 has what private at from 733697.0 till 733697.0
c1 has what private at from 733697.0 till 733697.0
c1 has what private at from 733697.0 till 733697.0
c1 has what private at from 733697.0 till 733697.0
c1 has what private at from 733697.0 till 733697.0
c1 has what private at from 733697.0 till 733697.0
c1 has what private at from 733697.0 till 733697.0
c1 has what private at from 733697.0 till 733697.0
c1 has what private at from 733697.0 till 733697.0
c1 has what private at from 733697.0 till 733697.0
c1 has what private at from 733697.0 till 733697.0
c1 has what private at from 733697.0 till 733697.0
c1 has what private at from 733697.0 till 733697.0
c1 has what private at from 733697.0 till 733697.0
c1 has what private at from 733697.0 till 733697.0
(defclass Name (is-a USER))
(deffunction reduce-class (?instance ?class) (if (eq (length$ (find-instance ((?a ?class)) (eq (instance-name ?a) ?instance))) 0) then (make-instance ?instance of ?class) (python-call tonl ?class ?instance)))
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


(deffunction mincomstart (?dur1 ?dur2)
    (return (max (send ?dur1 get-start) (send ?dur2 get-start)))
)



(deffunction maxcomend (?dur1 ?dur2)
    (bind ?e1 (send ?dur1 get-end))
    (bind ?e2 (send ?dur2 get-end))
    (if (= ?e1 ?e2) then (return ?e1))
    (if (= ?e2 -1) then
        (return ?e1)
    )
    (if (= ?e1 -1) then
        (return ?e2)
    )
    (return (min ?e1 ?e2))
)

(defclass Proposition (is-a Name) (slot truth (type INTEGER) (default 1) (pattern-match reactive)) (slot subject (type INSTANCE) (pattern-match reactive)) (slot predicate (type INSTANCE) (pattern-match reactive)) (slot time (type ?VARIABLE) (pattern-match reactive)))

(deffunction add-prop (?s ?p ?t ?r)
       (if (and (python-call ptonl ?s ?p ?t ?r)
                (= (+ (length$ (find-instance ((?prop Proposition) (?dur Duration))
                          (and (eq ?prop:subject ?s)
                               (eq ?prop:predicate ?p)
                               (eq ?prop:time ?dur)
                               (= ?dur:start (send ?t get-start))
                               (= ?dur:end (send ?t get-end))
                               (eq ?prop:truth ?r))))
                      (length$ (find-instance ((?prop Proposition))
                          (and (eq ?prop:subject ?s)
                               (eq ?prop:predicate ?p)
                               (= ?prop:time ?t)
                               (eq ?prop:truth ?r)))))
                 0))
        then (make-instance of Proposition (subject ?s)
                                           (predicate ?p)
                                           (time ?t)
                                           (truth ?r))
        else (return TRUE)))
------------OPEN---------------------
----------F-OPEN---------------------
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
admin
(reduce-class [member] Role)
member
(reduce-class [manager] Role)
manager
(reduce-class [basic_perm] Permission)
basic_perm
(reduce-class [manage_perm] Permission)
manage_perm
(reduce-class [create_perm] Permission)
create_perm
(reduce-class [public] Status)
public
(reduce-class [private] Status)
private
(add-prop [admin] (add-pred Has what [manager]) (make-instance of Duration (start 733697.0) (end -1.0)) 1)
admin has what manager at from 733697.0 till -1.0
(add-prop [member] (add-pred Has what [basic_perm]) (make-instance of Duration (start 733697.0) (end -1.0)) 1)
member has what basic_perm at from 733697.0 till -1.0
(defrule aeb61c464c8b43529850efbdd2f38c74 (logical (object (is-a Proposition) (subject ?X1&:(or (eq (class ?X1) Person) (subclassp (class ?X1) Person))) (predicate ?Y1&:(or (eq (class ?Y1) Wants) (subclassp (class ?Y1) Wants))&:(or (eq (class (send ?Y1 get-to)) Create) (subclassp (class (send ?Y1 get-to)) Create))&:(or (eq (class (send (send ?Y1 get-to) get-what)) Thing) (subclassp (class (send (send ?Y1 get-to) get-what)) Thing))) (time ?X2) (truth 1))) (logical (object (is-a Proposition) (subject ?X1) (predicate ?Y2&:(or (eq (class ?Y2) Has) (subclassp (class ?Y2) Has))&:(eq (send ?Y2 get-what) [create_perm])) (time ?X3&:(or (eq (class ?X3) Duration) (subclassp (class ?X3) Duration))) (truth 1))) (test (and (<= (send ?X3 get-start) ?X2) (or (= (send ?X3 get-end) -1) (>= (send ?X3 get-end) ?X2)))) => (add-prop ?X1 (add-pred Create what (send (send ?Y1 get-to) get-what)) ?X2 1))
(defrule 26fcc024ec6944188c37c3e63aad3423 (logical (object (is-a Proposition) (subject ?X1&:(or (eq (class ?X1) Person) (subclassp (class ?X1) Person))) (predicate ?Y4&:(or (eq (class ?Y4) Wants) (subclassp (class ?Y4) Wants))) (time ?X2) (truth 1))) (logical (object (is-a Proposition) (subject ?X1) (predicate ?Y5&:(or (eq (class ?Y5) Can) (subclassp (class ?Y5) Can))&:(eq (send ?Y4 get-to) (send ?Y5 get-what))) (time ?X3&:(or (eq (class ?X3) Duration) (subclassp (class ?X3) Duration))) (truth 1))) (test (and (<= (send ?X3 get-start) ?X2) (or (= (send ?X3 get-end) -1) (>= (send ?X3 get-end) ?X2)))) => (add-prop ?X1 (send ?Y4 get-to) ?X2 1))
(defrule 96104e1684b7462d87b638fa5f16b247 (logical (object (is-a Proposition) (subject ?X2&:(or (eq (class ?X2) Thing) (subclassp (class ?X2) Thing))) (predicate ?Y7&:(or (eq (class ?Y7) IsNeeded) (subclassp (class ?Y7) IsNeeded))) (time ?X3&:(or (eq (class ?X3) Duration) (subclassp (class ?X3) Duration))) (truth 1))) (logical (object (is-a Proposition) (subject ?X1&:(or (eq (class ?X1) Thing) (subclassp (class ?X1) Thing))) (predicate ?Y9&:(or (eq (class ?Y9) Has) (subclassp (class ?Y9) Has))&:(eq (send ?Y9 get-what) ?X2)) (time ?X5&:(or (eq (class ?X5) Duration) (subclassp (class ?X5) Duration))) (truth 1))) (test (or (and (>= (send ?X3 get-start) (send ?X3 get-start)) (or (<= (send ?X3 get-start) (send ?X3 get-end)) (= (send ?X3 get-end) -1))) (and (>= (send ?X3 get-start) (send ?X3 get-start)) (or (<= (send ?X3 get-start) (send ?X3 get-end)) (= (send ?X3 get-end) -1))))) => (add-prop ?X1 (add-pred Can what (send ?Y7 get-for_action)) (make-instance of Duration (start (mincomstart ?X3 ?X5)) (end (maxcomend ?X3 ?X5))) 1))
(defrule 20f9c8d8ed7947c796395980b2671cbf (logical (object (is-a Proposition) (subject ?X1&:(or (eq (class ?X1) Thing) (subclassp (class ?X1) Thing))) (predicate ?Y11&:(or (eq (class ?Y11) IsIn) (subclassp (class ?Y11) IsIn))&:(or (eq (class (send ?Y11 get-what)) Thing) (subclassp (class (send ?Y11 get-what)) Thing))) (time ?X4&:(or (eq (class ?X4) Duration) (subclassp (class ?X4) Duration))) (truth 1))) (logical (object (is-a Proposition) (subject ?X2&:(eq ?X2 (send ?Y11 get-what))) (predicate ?Y13&:(or (eq (class ?Y13) IsIn) (subclassp (class ?Y13) IsIn))&:(or (eq (class (send ?Y13 get-what)) Thing) (subclassp (class (send ?Y13 get-what)) Thing))) (time ?X5&:(or (eq (class ?X5) Duration) (subclassp (class ?X5) Duration))) (truth 1))) (test (or (and (>= (send ?X4 get-start) (send ?X4 get-start)) (or (<= (send ?X4 get-start) (send ?X4 get-end)) (= (send ?X4 get-end) -1))) (and (>= (send ?X4 get-start) (send ?X4 get-start)) (or (<= (send ?X4 get-start) (send ?X4 get-end)) (= (send ?X4 get-end) -1))))) => (add-prop ?X1 (add-pred IsIn what (send ?Y13 get-what)) (make-instance of Duration (start (mincomstart ?X4 ?X5)) (end (maxcomend ?X4 ?X5))) 1))
(defrule 2b31385d903f45448c1943d44e75792e (logical (object (is-a Proposition) (subject ?X1&:(or (eq (class ?X1) Person) (subclassp (class ?X1) Person))) (predicate ?Y15&:(or (eq (class ?Y15) IsIn) (subclassp (class ?Y15) IsIn))) (time ?X3&:(or (eq (class ?X3) Duration) (subclassp (class ?X3) Duration))) (truth 1))) (logical (object (is-a Proposition) (subject ?X2&:(or (eq (class ?X2) Group) (subclassp (class ?X2) Group))) (predicate ?Y17&:(or (eq (class ?Y17) Has) (subclassp (class ?Y17) Has))&:(or (eq (class (send ?Y17 get-what)) Permission) (subclassp (class (send ?Y17 get-what)) Permission))) (time ?X5&:(or (eq (class ?X5) Duration) (subclassp (class ?X5) Duration))) (truth 1))) (test (or (and (>= (send ?X3 get-start) (send ?X3 get-start)) (or (<= (send ?X3 get-start) (send ?X3 get-end)) (= (send ?X3 get-end) -1))) (and (>= (send ?X3 get-start) (send ?X3 get-start)) (or (<= (send ?X3 get-start) (send ?X3 get-end)) (= (send ?X3 get-end) -1))))) => (add-prop ?X1 (add-pred Has what (send ?Y17 get-what)) (make-instance of Duration (start (mincomstart ?X3 ?X5)) (end (maxcomend ?X3 ?X5))) 1))
(defrule 6c6197a2db7b425d88ac9f2bfe3411bc (logical (object (is-a Proposition) (subject ?X1&:(or (eq (class ?X1) Person) (subclassp (class ?X1) Person))) (predicate ?Y19&:(or (eq (class ?Y19) Has) (subclassp (class ?Y19) Has))&:(or (eq (class (send ?Y19 get-what)) Role) (subclassp (class (send ?Y19 get-what)) Role))) (time ?X3&:(or (eq (class ?X3) Duration) (subclassp (class ?X3) Duration))) (truth 1))) (logical (object (is-a Proposition) (subject ?X2&:(eq ?X2 (send ?Y19 get-what))) (predicate ?Y21&:(or (eq (class ?Y21) Has) (subclassp (class ?Y21) Has))&:(or (eq (class (send ?Y21 get-what)) Permission) (subclassp (class (send ?Y21 get-what)) Permission))) (time ?X5&:(or (eq (class ?X5) Duration) (subclassp (class ?X5) Duration))) (truth 1))) (test (or (and (>= (send ?X3 get-start) (send ?X3 get-start)) (or (<= (send ?X3 get-start) (send ?X3 get-end)) (= (send ?X3 get-end) -1))) (and (>= (send ?X3 get-start) (send ?X3 get-start)) (or (<= (send ?X3 get-start) (send ?X3 get-end)) (= (send ?X3 get-end) -1))))) => (add-prop ?X1 (add-pred Has what (send ?Y21 get-what)) (make-instance of Duration (start (mincomstart ?X3 ?X5)) (end (maxcomend ?X3 ?X5))) 1))
(defrule 322afc5f0fc544db87b2c5385550e86d (logical (object (is-a Person) (name ?X1))) => (add-prop ?X1 (add-pred Has what [member]) (make-instance of Duration (start 733697.0) (end -1.0)) 1))
(defrule 6ae1a334d8974674b55f0cd96399dbf0 (logical (object (is-a Proposition) (subject ?X1&:(or (eq (class ?X1) Person) (subclassp (class ?X1) Person))) (predicate ?Y23&:(or (eq (class ?Y23) Create) (subclassp (class ?Y23) Create))&:(or (eq (class (send ?Y23 get-what)) Content) (subclassp (class (send ?Y23 get-what)) Content))) (time ?X3) (truth 1))) => (reduce-class (send ?Y23 get-what) Content) (add-prop ?X1 (add-pred IsOwner of (send ?Y23 get-what)) (make-instance of Duration (start ?X3) (end -1.0)) 1) (add-prop (send ?Y23 get-what) (add-pred Has what [private]) (make-instance of Duration (start ?X3) (end -1.0)) 1))
(defrule 9e2a1e8c5e7a458bab61e1770f458d50 (logical (object (is-a Permission) (name ?X2))) => (add-prop [manager] (add-pred Has what ?X2) (make-instance of Duration (start 733697.0) (end -1.0)) 1))
(defrule ff8e615086a549d6930fc37629624ae5 (logical (object (is-a Proposition) (subject ?X1&:(or (eq (class ?X1) Content) (subclassp (class ?X1) Content))) (predicate ?Y24&:(or (eq (class ?Y24) Has) (subclassp (class ?Y24) Has))&:(eq (send ?Y24 get-what) [public])) (time ?X2&:(or (eq (class ?X2) Duration) (subclassp (class ?X2) Duration))) (truth 1))) => (add-prop [basic_perm] (add-pred IsNeeded for_action (add-pred View what ?X1)) ?X2 1))
(defrule e37eae65c00f45d8a3c8655b86f57f6f (logical (object (is-a Proposition) (subject ?X1&:(or (eq (class ?X1) Content) (subclassp (class ?X1) Content))) (predicate ?Y26&:(or (eq (class ?Y26) Has) (subclassp (class ?Y26) Has))&:(eq (send ?Y26 get-what) [private])) (time ?X2&:(or (eq (class ?X2) Duration) (subclassp (class ?X2) Duration))) (truth 1))) => (add-prop [manage_perm] (add-pred IsNeeded for_action (add-pred View what ?X1)) ?X2 1))
(defrule 05736911053a412896ce798f9a60d7d6 (logical (object (is-a Proposition) (subject ?X1&:(or (eq (class ?X1) Content) (subclassp (class ?X1) Content))) (predicate ?Y28&:(or (eq (class ?Y28) Has) (subclassp (class ?Y28) Has))&:(eq (send ?Y28 get-what) [private])) (time ?X3&:(or (eq (class ?X3) Duration) (subclassp (class ?X3) Duration))) (truth 1))) (logical (object (is-a Proposition) (subject ?X2&:(or (eq (class ?X2) Person) (subclassp (class ?X2) Person))) (predicate ?Y30&:(or (eq (class ?Y30) IsOwner) (subclassp (class ?Y30) IsOwner))&:(eq (send ?Y30 get-of) ?X1)) (time ?X4&:(or (eq (class ?X4) Duration) (subclassp (class ?X4) Duration))) (truth 1))) (test (or (and (>= (send ?X3 get-start) (send ?X3 get-start)) (or (<= (send ?X3 get-start) (send ?X3 get-end)) (= (send ?X3 get-end) -1))) (and (>= (send ?X3 get-start) (send ?X3 get-start)) (or (<= (send ?X3 get-start) (send ?X3 get-end)) (= (send ?X3 get-end) -1))))) => (add-prop ?X2 (add-pred Can what (add-pred View what ?X1)) (make-instance of Duration (start (mincomstart ?X3 ?X4)) (end (maxcomend ?X3 ?X4))) 1))
(defrule 37870ea2f510427ead0aa5061341fa97 (logical (object (is-a Proposition) (subject ?X1&:(or (eq (class ?X1) Person) (subclassp (class ?X1) Person))) (predicate ?Y32&:(or (eq (class ?Y32) Publish) (subclassp (class ?Y32) Publish))&:(or (eq (class (send ?Y32 get-what)) Content) (subclassp (class (send ?Y32 get-what)) Content))) (time ?X3) (truth 1))) (logical (object (is-a Proposition) (subject ?X2&:(eq ?X2 (send ?Y32 get-what))) (predicate ?Y33&:(or (eq (class ?Y33) Has) (subclassp (class ?Y33) Has))&:(or (eq (class (send ?Y33 get-what)) Status) (subclassp (class (send ?Y33 get-what)) Status))) (time ?X5&:(or (eq (class ?X5) Duration) (subclassp (class ?X5) Duration))) (truth 1))) => (add-prop (send ?Y32 get-what) (add-pred Has what [public]) (make-instance of Duration (start ?X3) (end -1.0)) 1))
(defrule 51a43a901c1a46e7afe6f1a7ddd4956c (logical (object (is-a Content) (name ?X1))) => (add-prop [manage_perm] (add-pred IsNeeded for_action (add-pred Publish what ?X1)) (make-instance of Duration (start 733697.0) (end -1.0)) 1))
(defrule 4db0f0380f28456796130c45c47251d9 (logical (object (is-a Proposition) (subject ?X1&:(or (eq (class ?X1) Person) (subclassp (class ?X1) Person))) (predicate ?Y35&:(or (eq (class ?Y35) Hide) (subclassp (class ?Y35) Hide))&:(or (eq (class (send ?Y35 get-what)) Content) (subclassp (class (send ?Y35 get-what)) Content))) (time ?X3) (truth 1))) (logical (object (is-a Proposition) (subject ?X2&:(eq ?X2 (send ?Y35 get-what))) (predicate ?Y36&:(or (eq (class ?Y36) Has) (subclassp (class ?Y36) Has))&:(or (eq (class (send ?Y36 get-what)) Status) (subclassp (class (send ?Y36 get-what)) Status))) (time ?X5&:(or (eq (class ?X5) Duration) (subclassp (class ?X5) Duration))) (truth 1))) => (send ?X5 put-end 733697) (add-prop (send ?Y35 get-what) (add-pred Has what [private]) (make-instance of Duration (start ?X3) (end -1.0)) 1))
(defrule ed333999ef7e4c88b6e745fc14138de0 (logical (object (is-a Proposition) (subject ?X1&:(or (eq (class ?X1) Person) (subclassp (class ?X1) Person))) (predicate ?Y38&:(or (eq (class ?Y38) IsOwner) (subclassp (class ?Y38) IsOwner))&:(or (eq (class (send ?Y38 get-of)) Content) (subclassp (class (send ?Y38 get-of)) Content))) (time ?X3&:(or (eq (class ?X3) Duration) (subclassp (class ?X3) Duration))) (truth 1))) => (add-prop ?X1 (add-pred Can what (add-pred Hide what (send ?Y38 get-of))) ?X3 1))
(reduce-class [john] Person)
john
(reduce-class [pete] Person)
pete
(reduce-class [jane] Person)
jane
(reduce-class [c1] Content)
c1
(reduce-class [c2] Content)
c2
(add-prop [john] (add-pred Has what [manager]) (make-instance of Duration (start 733697.0) (end -1.0)) 1)
john has what manager at from 733697.0 till -1.0
(add-prop [jane] (add-pred Has what [create_perm]) (make-instance of Duration (start 733697.0) (end -1.0)) 1)
jane has what create_perm at from 733697.0 till -1.0
(add-prop [jane] (add-pred Wants to (add-pred Create what [c1])) 733697.0 1)
jane wants to create what c1 at 733697.0
(add-prop [pete] (add-pred Wants to (add-pred Create what [c2])) 733697.0 1)
pete wants to create what c2 at 733697.0
----------running---------------------
jane create what c1 at 733697.0
jane isowner of c1 at from 733697.0 till -1.0
c1 has what private at from 733697.0 till -1.0
jane can what view what c1 at from 733697.0 till -1.0
manage_perm isneeded for_action view what c1 at from 733697.0 till -1.0
jane can what hide what c1 at from 733697.0 till -1.0
manage_perm isneeded for_action publish what c2 at from 733697.0 till -1.0
manage_perm isneeded for_action publish what c1 at from 733697.0 till -1.0
jane has what member at from 733697.0 till -1.0
jane has what basic_perm at from 733697.0 till -1.0
pete has what member at from 733697.0 till -1.0
pete has what basic_perm at from 733697.0 till -1.0
john has what member at from 733697.0 till -1.0
john has what basic_perm at from 733697.0 till -1.0
manager has what create_perm at from 733697.0 till -1.0
admin has what create_perm at from 733697.0 till -1.0
john has what create_perm at from 733697.0 till -1.0
manager has what manage_perm at from 733697.0 till -1.0
manager can what view what c1 at from 733697.0 till -1.0
manager can what publish what c2 at from 733697.0 till -1.0
manager can what publish what c1 at from 733697.0 till -1.0
admin has what manage_perm at from 733697.0 till -1.0
admin can what view what c1 at from 733697.0 till -1.0
admin can what publish what c2 at from 733697.0 till -1.0
admin can what publish what c1 at from 733697.0 till -1.0
john has what manage_perm at from 733697.0 till -1.0
john can what view what c1 at from 733697.0 till -1.0
john can what publish what c2 at from 733697.0 till -1.0
john can what publish what c1 at from 733697.0 till -1.0
manager has what basic_perm at from 733697.0 till -1.0
admin has what basic_perm at from 733697.0 till -1.0
admin has what member at from 733697.0 till -1.0
----------runned---------------------
(find-all-instances ((?prop Proposition) (?Y40 IsOwner) (?Y41 Duration)) (and (eq ?prop:subject [jane]) (eq ?Y40:of [c1]) (eq ?prop:predicate ?Y40) (= ?Y41:start 733697.0) (= ?Y41:end -1.0) (eq ?prop:truth 1)))
35


jane isowner of c1 at from 733697.0 till -1.0
jane isowner of c1 at from 733697.0 till -1.0
jane isowner of c1 at from 733697.0 till -1.0
jane isowner of c1 at from 733697.0 till -1.0
jane isowner of c1 at from 733697.0 till -1.0
jane isowner of c1 at from 733697.0 till -1.0
jane isowner of c1 at from 733697.0 till -1.0
jane isowner of c1 at from 733697.0 till -1.0
jane isowner of c1 at from 733697.0 till -1.0
jane isowner of c1 at from 733697.0 till -1.0
jane isowner of c1 at from 733697.0 till -1.0
jane isowner of c1 at from 733697.0 till -1.0
jane isowner of c1 at from 733697.0 till -1.0
jane isowner of c1 at from 733697.0 till -1.0
jane isowner of c1 at from 733697.0 till -1.0
jane isowner of c1 at from 733697.0 till -1.0
jane isowner of c1 at from 733697.0 till -1.0
jane isowner of c1 at from 733697.0 till -1.0
jane isowner of c1 at from 733697.0 till -1.0
jane isowner of c1 at from 733697.0 till -1.0
jane isowner of c1 at from 733697.0 till -1.0
jane isowner of c1 at from 733697.0 till -1.0
jane isowner of c1 at from 733697.0 till -1.0
jane isowner of c1 at from 733697.0 till -1.0
jane isowner of c1 at from 733697.0 till -1.0
jane isowner of c1 at from 733697.0 till -1.0
jane isowner of c1 at from 733697.0 till -1.0
jane isowner of c1 at from 733697.0 till -1.0
jane isowner of c1 at from 733697.0 till -1.0
jane isowner of c1 at from 733697.0 till -1.0
jane isowner of c1 at from 733697.0 till -1.0
jane isowner of c1 at from 733697.0 till -1.0
jane isowner of c1 at from 733697.0 till -1.0
jane isowner of c1 at from 733697.0 till -1.0
jane isowner of c1 at from 733697.0 till -1.0
(add-prop [jane] (add-pred Wants to (add-pred Publish what [c1])) 733697.0 1)
jane wants to publish what c1 at 733697.0
(add-prop [pete] (add-pred Wants to (add-pred Publish what [c2])) 733697.0 1)
pete wants to publish what c2 at 733697.0
----------running---------------------
----------runned---------------------
(add-prop [john] (add-pred Wants to (add-pred Publish what [c1])) 733697.0 1)
john wants to publish what c1 at 733697.0
----------running---------------------
john publish what c1 at 733697.0
c1 has what public at from 733697.0 till -1.0
basic_perm isneeded for_action view what c1 at from 733697.0 till -1.0
pete can what view what c1 at from 733697.0 till -1.0
member can what view what c1 at from 733697.0 till -1.0
----------runned---------------------
(find-all-instances ((?prop Proposition) (?Y42 Has) (?Y43 Duration)) (and (eq ?prop:subject [c1]) (eq ?Y42:what [private]) (eq ?prop:predicate ?Y42) (= ?Y43:start 733697.0) (= ?Y43:end -1.0) (eq ?prop:truth 1)))
43


c1 has what private at from 733697.0 till -1.0
c1 has what private at from 733697.0 till -1.0
c1 has what private at from 733697.0 till -1.0
c1 has what private at from 733697.0 till -1.0
c1 has what private at from 733697.0 till -1.0
c1 has what private at from 733697.0 till -1.0
c1 has what private at from 733697.0 till -1.0
c1 has what private at from 733697.0 till -1.0
c1 has what private at from 733697.0 till -1.0
c1 has what private at from 733697.0 till -1.0
c1 has what private at from 733697.0 till -1.0
c1 has what private at from 733697.0 till -1.0
c1 has what private at from 733697.0 till -1.0
c1 has what private at from 733697.0 till -1.0
c1 has what private at from 733697.0 till -1.0
c1 has what private at from 733697.0 till -1.0
c1 has what private at from 733697.0 till -1.0
c1 has what private at from 733697.0 till -1.0
c1 has what private at from 733697.0 till -1.0
c1 has what private at from 733697.0 till -1.0
c1 has what private at from 733697.0 till -1.0
c1 has what private at from 733697.0 till -1.0
c1 has what private at from 733697.0 till -1.0
c1 has what private at from 733697.0 till -1.0
c1 has what private at from 733697.0 till -1.0
c1 has what private at from 733697.0 till -1.0
c1 has what private at from 733697.0 till -1.0
c1 has what private at from 733697.0 till -1.0
c1 has what private at from 733697.0 till -1.0
c1 has what private at from 733697.0 till -1.0
c1 has what private at from 733697.0 till -1.0
c1 has what private at from 733697.0 till -1.0
c1 has what private at from 733697.0 till -1.0
c1 has what private at from 733697.0 till -1.0
c1 has what private at from 733697.0 till -1.0
c1 has what private at from 733697.0 till -1.0
c1 has what private at from 733697.0 till -1.0
c1 has what private at from 733697.0 till -1.0
c1 has what private at from 733697.0 till -1.0
c1 has what private at from 733697.0 till -1.0
c1 has what private at from 733697.0 till -1.0
c1 has what private at from 733697.0 till -1.0
c1 has what private at from 733697.0 till -1.0
(defclass Name (is-a USER))
(deffunction reduce-class (?instance ?class) (if (eq (length$ (find-instance ((?a ?class)) (eq (instance-name ?a) ?instance))) 0) then (make-instance ?instance of ?class) (python-call tonl ?class ?instance)))
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


(deffunction mincomstart (?dur1 ?dur2)
    (return (max (send ?dur1 get-start) (send ?dur2 get-start)))
)



(deffunction maxcomend (?dur1 ?dur2)
    (bind ?e1 (send ?dur1 get-end))
    (bind ?e2 (send ?dur2 get-end))
    (if (= ?e1 ?e2) then (return ?e1))
    (if (= ?e2 -1) then
        (return ?e1)
    )
    (if (= ?e1 -1) then
        (return ?e2)
    )
    (return (min ?e1 ?e2))
)

(defclass Proposition (is-a Name) (slot truth (type INTEGER) (default 1) (pattern-match reactive)) (slot subject (type INSTANCE) (pattern-match reactive)) (slot predicate (type INSTANCE) (pattern-match reactive)) (slot time (type ?VARIABLE) (pattern-match reactive)))

(deffunction add-prop (?s ?p ?t ?r)
       (if (and (python-call ptonl ?s ?p ?t ?r)
                (= (+ (length$ (find-instance ((?prop Proposition) (?dur Duration))
                          (and (eq ?prop:subject ?s)
                               (eq ?prop:predicate ?p)
                               (eq ?prop:time ?dur)
                               (= ?dur:start (send ?t get-start))
                               (= ?dur:end (send ?t get-end))
                               (eq ?prop:truth ?r))))
                      (length$ (find-instance ((?prop Proposition))
                          (and (eq ?prop:subject ?s)
                               (eq ?prop:predicate ?p)
                               (= ?prop:time ?t)
                               (eq ?prop:truth ?r)))))
                 0))
        then (make-instance of Proposition (subject ?s)
                                           (predicate ?p)
                                           (time ?t)
                                           (truth ?r))
        else (return TRUE)))
------------OPEN---------------------
----------F-OPEN---------------------
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
admin
(reduce-class [member] Role)
member
(reduce-class [manager] Role)
manager
(reduce-class [basic_perm] Permission)
basic_perm
(reduce-class [manage_perm] Permission)
manage_perm
(reduce-class [create_perm] Permission)
create_perm
(reduce-class [public] Status)
public
(reduce-class [private] Status)
private
(add-prop [admin] (add-pred Has what [manager]) (make-instance of Duration (start 733697.0) (end -1.0)) 1)
admin has what manager at from 733697.0 till -1.0
(add-prop [member] (add-pred Has what [basic_perm]) (make-instance of Duration (start 733697.0) (end -1.0)) 1)
member has what basic_perm at from 733697.0 till -1.0
(defrule 0002ecf1266d42db87819edd04c05467 (logical (object (is-a Proposition) (subject ?X1&:(or (eq (class ?X1) Person) (subclassp (class ?X1) Person))) (predicate ?Y1&:(or (eq (class ?Y1) Wants) (subclassp (class ?Y1) Wants))&:(or (eq (class (send ?Y1 get-to)) Create) (subclassp (class (send ?Y1 get-to)) Create))&:(or (eq (class (send (send ?Y1 get-to) get-what)) Thing) (subclassp (class (send (send ?Y1 get-to) get-what)) Thing))) (time ?X2) (truth 1))) (logical (object (is-a Proposition) (subject ?X1) (predicate ?Y2&:(or (eq (class ?Y2) Has) (subclassp (class ?Y2) Has))&:(eq (send ?Y2 get-what) [create_perm])) (time ?X3&:(or (eq (class ?X3) Duration) (subclassp (class ?X3) Duration))) (truth 1))) (test (and (<= (send ?X3 get-start) ?X2) (or (= (send ?X3 get-end) -1) (>= (send ?X3 get-end) ?X2)))) => (add-prop ?X1 (add-pred Create what (send (send ?Y1 get-to) get-what)) ?X2 1))
(defrule aedc3075892e4e25838b2a3fe15826b7 (logical (object (is-a Proposition) (subject ?X1&:(or (eq (class ?X1) Person) (subclassp (class ?X1) Person))) (predicate ?Y4&:(or (eq (class ?Y4) Wants) (subclassp (class ?Y4) Wants))) (time ?X2) (truth 1))) (logical (object (is-a Proposition) (subject ?X1) (predicate ?Y5&:(or (eq (class ?Y5) Can) (subclassp (class ?Y5) Can))&:(eq (send ?Y4 get-to) (send ?Y5 get-what))) (time ?X3&:(or (eq (class ?X3) Duration) (subclassp (class ?X3) Duration))) (truth 1))) (test (and (<= (send ?X3 get-start) ?X2) (or (= (send ?X3 get-end) -1) (>= (send ?X3 get-end) ?X2)))) => (add-prop ?X1 (send ?Y4 get-to) ?X2 1))
(defrule 7cb61a607d024169ad914f1f83c88ef4 (logical (object (is-a Proposition) (subject ?X2&:(or (eq (class ?X2) Thing) (subclassp (class ?X2) Thing))) (predicate ?Y7&:(or (eq (class ?Y7) IsNeeded) (subclassp (class ?Y7) IsNeeded))) (time ?X3&:(or (eq (class ?X3) Duration) (subclassp (class ?X3) Duration))) (truth 1))) (logical (object (is-a Proposition) (subject ?X1&:(or (eq (class ?X1) Thing) (subclassp (class ?X1) Thing))) (predicate ?Y9&:(or (eq (class ?Y9) Has) (subclassp (class ?Y9) Has))&:(eq (send ?Y9 get-what) ?X2)) (time ?X5&:(or (eq (class ?X5) Duration) (subclassp (class ?X5) Duration))) (truth 1))) (test (or (and (>= (send ?X3 get-start) (send ?X3 get-start)) (or (<= (send ?X3 get-start) (send ?X3 get-end)) (= (send ?X3 get-end) -1))) (and (>= (send ?X3 get-start) (send ?X3 get-start)) (or (<= (send ?X3 get-start) (send ?X3 get-end)) (= (send ?X3 get-end) -1))))) => (add-prop ?X1 (add-pred Can what (send ?Y7 get-for_action)) (make-instance of Duration (start (mincomstart ?X3 ?X5)) (end (maxcomend ?X3 ?X5))) 1))
(defrule 1652a66a12e643eaaf030c71b1af24bb (logical (object (is-a Proposition) (subject ?X1&:(or (eq (class ?X1) Thing) (subclassp (class ?X1) Thing))) (predicate ?Y11&:(or (eq (class ?Y11) IsIn) (subclassp (class ?Y11) IsIn))&:(or (eq (class (send ?Y11 get-what)) Thing) (subclassp (class (send ?Y11 get-what)) Thing))) (time ?X4&:(or (eq (class ?X4) Duration) (subclassp (class ?X4) Duration))) (truth 1))) (logical (object (is-a Proposition) (subject ?X2&:(eq ?X2 (send ?Y11 get-what))) (predicate ?Y13&:(or (eq (class ?Y13) IsIn) (subclassp (class ?Y13) IsIn))&:(or (eq (class (send ?Y13 get-what)) Thing) (subclassp (class (send ?Y13 get-what)) Thing))) (time ?X5&:(or (eq (class ?X5) Duration) (subclassp (class ?X5) Duration))) (truth 1))) (test (or (and (>= (send ?X4 get-start) (send ?X4 get-start)) (or (<= (send ?X4 get-start) (send ?X4 get-end)) (= (send ?X4 get-end) -1))) (and (>= (send ?X4 get-start) (send ?X4 get-start)) (or (<= (send ?X4 get-start) (send ?X4 get-end)) (= (send ?X4 get-end) -1))))) => (add-prop ?X1 (add-pred IsIn what (send ?Y13 get-what)) (make-instance of Duration (start (mincomstart ?X4 ?X5)) (end (maxcomend ?X4 ?X5))) 1))
(defrule 92c73e75f29b4a54a08218bb89692a2b (logical (object (is-a Proposition) (subject ?X1&:(or (eq (class ?X1) Person) (subclassp (class ?X1) Person))) (predicate ?Y15&:(or (eq (class ?Y15) IsIn) (subclassp (class ?Y15) IsIn))) (time ?X3&:(or (eq (class ?X3) Duration) (subclassp (class ?X3) Duration))) (truth 1))) (logical (object (is-a Proposition) (subject ?X2&:(or (eq (class ?X2) Group) (subclassp (class ?X2) Group))) (predicate ?Y17&:(or (eq (class ?Y17) Has) (subclassp (class ?Y17) Has))&:(or (eq (class (send ?Y17 get-what)) Permission) (subclassp (class (send ?Y17 get-what)) Permission))) (time ?X5&:(or (eq (class ?X5) Duration) (subclassp (class ?X5) Duration))) (truth 1))) (test (or (and (>= (send ?X3 get-start) (send ?X3 get-start)) (or (<= (send ?X3 get-start) (send ?X3 get-end)) (= (send ?X3 get-end) -1))) (and (>= (send ?X3 get-start) (send ?X3 get-start)) (or (<= (send ?X3 get-start) (send ?X3 get-end)) (= (send ?X3 get-end) -1))))) => (add-prop ?X1 (add-pred Has what (send ?Y17 get-what)) (make-instance of Duration (start (mincomstart ?X3 ?X5)) (end (maxcomend ?X3 ?X5))) 1))
(defrule 3d2b9b38be624b018e29a09ab909a6a6 (logical (object (is-a Proposition) (subject ?X1&:(or (eq (class ?X1) Person) (subclassp (class ?X1) Person))) (predicate ?Y19&:(or (eq (class ?Y19) Has) (subclassp (class ?Y19) Has))&:(or (eq (class (send ?Y19 get-what)) Role) (subclassp (class (send ?Y19 get-what)) Role))) (time ?X3&:(or (eq (class ?X3) Duration) (subclassp (class ?X3) Duration))) (truth 1))) (logical (object (is-a Proposition) (subject ?X2&:(eq ?X2 (send ?Y19 get-what))) (predicate ?Y21&:(or (eq (class ?Y21) Has) (subclassp (class ?Y21) Has))&:(or (eq (class (send ?Y21 get-what)) Permission) (subclassp (class (send ?Y21 get-what)) Permission))) (time ?X5&:(or (eq (class ?X5) Duration) (subclassp (class ?X5) Duration))) (truth 1))) (test (or (and (>= (send ?X3 get-start) (send ?X3 get-start)) (or (<= (send ?X3 get-start) (send ?X3 get-end)) (= (send ?X3 get-end) -1))) (and (>= (send ?X3 get-start) (send ?X3 get-start)) (or (<= (send ?X3 get-start) (send ?X3 get-end)) (= (send ?X3 get-end) -1))))) => (add-prop ?X1 (add-pred Has what (send ?Y21 get-what)) (make-instance of Duration (start (mincomstart ?X3 ?X5)) (end (maxcomend ?X3 ?X5))) 1))
(defrule d93847ceb84a4ea3a2b6d1a079fddd5a (logical (object (is-a Person) (name ?X1))) => (add-prop ?X1 (add-pred Has what [member]) (make-instance of Duration (start 733697.0) (end -1.0)) 1))
(defrule dbe90c1746d94cd898d5e14a91444bc6 (logical (object (is-a Proposition) (subject ?X1&:(or (eq (class ?X1) Person) (subclassp (class ?X1) Person))) (predicate ?Y23&:(or (eq (class ?Y23) Create) (subclassp (class ?Y23) Create))&:(or (eq (class (send ?Y23 get-what)) Content) (subclassp (class (send ?Y23 get-what)) Content))) (time ?X3) (truth 1))) => (reduce-class (send ?Y23 get-what) Content) (add-prop ?X1 (add-pred IsOwner of (send ?Y23 get-what)) (make-instance of Duration (start ?X3) (end -1.0)) 1) (add-prop (send ?Y23 get-what) (add-pred Has what [private]) (make-instance of Duration (start ?X3) (end -1.0)) 1))
(defrule 678faac4f9d245c6b6990590a6a35beb (logical (object (is-a Permission) (name ?X2))) => (add-prop [manager] (add-pred Has what ?X2) (make-instance of Duration (start 733697.0) (end -1.0)) 1))
(defrule 2531ba25ac28436ab1aa90796d797002 (logical (object (is-a Proposition) (subject ?X1&:(or (eq (class ?X1) Content) (subclassp (class ?X1) Content))) (predicate ?Y24&:(or (eq (class ?Y24) Has) (subclassp (class ?Y24) Has))&:(eq (send ?Y24 get-what) [public])) (time ?X2&:(or (eq (class ?X2) Duration) (subclassp (class ?X2) Duration))) (truth 1))) => (add-prop [basic_perm] (add-pred IsNeeded for_action (add-pred View what ?X1)) ?X2 1))
(defrule 160754208636417680de656225e7454b (logical (object (is-a Proposition) (subject ?X1&:(or (eq (class ?X1) Content) (subclassp (class ?X1) Content))) (predicate ?Y26&:(or (eq (class ?Y26) Has) (subclassp (class ?Y26) Has))&:(eq (send ?Y26 get-what) [private])) (time ?X2&:(or (eq (class ?X2) Duration) (subclassp (class ?X2) Duration))) (truth 1))) => (add-prop [manage_perm] (add-pred IsNeeded for_action (add-pred View what ?X1)) ?X2 1))
(defrule 28233222677c4d16a3f579b05ae59d9a (logical (object (is-a Proposition) (subject ?X1&:(or (eq (class ?X1) Content) (subclassp (class ?X1) Content))) (predicate ?Y28&:(or (eq (class ?Y28) Has) (subclassp (class ?Y28) Has))&:(eq (send ?Y28 get-what) [private])) (time ?X3&:(or (eq (class ?X3) Duration) (subclassp (class ?X3) Duration))) (truth 1))) (logical (object (is-a Proposition) (subject ?X2&:(or (eq (class ?X2) Person) (subclassp (class ?X2) Person))) (predicate ?Y30&:(or (eq (class ?Y30) IsOwner) (subclassp (class ?Y30) IsOwner))&:(eq (send ?Y30 get-of) ?X1)) (time ?X4&:(or (eq (class ?X4) Duration) (subclassp (class ?X4) Duration))) (truth 1))) (test (or (and (>= (send ?X3 get-start) (send ?X3 get-start)) (or (<= (send ?X3 get-start) (send ?X3 get-end)) (= (send ?X3 get-end) -1))) (and (>= (send ?X3 get-start) (send ?X3 get-start)) (or (<= (send ?X3 get-start) (send ?X3 get-end)) (= (send ?X3 get-end) -1))))) => (add-prop ?X2 (add-pred Can what (add-pred View what ?X1)) (make-instance of Duration (start (mincomstart ?X3 ?X4)) (end (maxcomend ?X3 ?X4))) 1))
(defrule 30bf1401a2d54b4cb72529974fff4163 (logical (object (is-a Proposition) (subject ?X1&:(or (eq (class ?X1) Person) (subclassp (class ?X1) Person))) (predicate ?Y32&:(or (eq (class ?Y32) Publish) (subclassp (class ?Y32) Publish))&:(or (eq (class (send ?Y32 get-what)) Content) (subclassp (class (send ?Y32 get-what)) Content))) (time ?X3) (truth 1))) (logical (object (is-a Proposition) (subject ?X2&:(eq ?X2 (send ?Y32 get-what))) (predicate ?Y33&:(or (eq (class ?Y33) Has) (subclassp (class ?Y33) Has))&:(or (eq (class (send ?Y33 get-what)) Status) (subclassp (class (send ?Y33 get-what)) Status))) (time ?X5&:(or (eq (class ?X5) Duration) (subclassp (class ?X5) Duration))) (truth 1))) => (send ?X5 put-end 733697) (add-prop (send ?Y32 get-what) (add-pred Has what [public]) (make-instance of Duration (start ?X3) (end -1.0)) 1))
(defrule 4f1b1fa59a0a4e989eebfacbf02173fe (logical (object (is-a Content) (name ?X1))) => (add-prop [manage_perm] (add-pred IsNeeded for_action (add-pred Publish what ?X1)) (make-instance of Duration (start 733697.0) (end -1.0)) 1))
(defrule 78de321dbc47405b99cdb591c3960916 (logical (object (is-a Proposition) (subject ?X1&:(or (eq (class ?X1) Person) (subclassp (class ?X1) Person))) (predicate ?Y35&:(or (eq (class ?Y35) Hide) (subclassp (class ?Y35) Hide))&:(or (eq (class (send ?Y35 get-what)) Content) (subclassp (class (send ?Y35 get-what)) Content))) (time ?X3) (truth 1))) (logical (object (is-a Proposition) (subject ?X2&:(eq ?X2 (send ?Y35 get-what))) (predicate ?Y36&:(or (eq (class ?Y36) Has) (subclassp (class ?Y36) Has))&:(or (eq (class (send ?Y36 get-what)) Status) (subclassp (class (send ?Y36 get-what)) Status))) (time ?X5&:(or (eq (class ?X5) Duration) (subclassp (class ?X5) Duration))) (truth 1))) => (send ?X5 put-end 733697) (add-prop (send ?Y35 get-what) (add-pred Has what [private]) (make-instance of Duration (start ?X3) (end -1.0)) 1))
(defrule aa8a668a48c64c0d9a34a9c2a11e0fd7 (logical (object (is-a Proposition) (subject ?X1&:(or (eq (class ?X1) Person) (subclassp (class ?X1) Person))) (predicate ?Y38&:(or (eq (class ?Y38) IsOwner) (subclassp (class ?Y38) IsOwner))&:(or (eq (class (send ?Y38 get-of)) Content) (subclassp (class (send ?Y38 get-of)) Content))) (time ?X3&:(or (eq (class ?X3) Duration) (subclassp (class ?X3) Duration))) (truth 1))) => (add-prop ?X1 (add-pred Can what (add-pred Hide what (send ?Y38 get-of))) ?X3 1))
(reduce-class [john] Person)
john
(reduce-class [pete] Person)
pete
(reduce-class [jane] Person)
jane
(reduce-class [c1] Content)
c1
(reduce-class [c2] Content)
c2
(add-prop [john] (add-pred Has what [manager]) (make-instance of Duration (start 733697.0) (end -1.0)) 1)
john has what manager at from 733697.0 till -1.0
(add-prop [jane] (add-pred Has what [create_perm]) (make-instance of Duration (start 733697.0) (end -1.0)) 1)
jane has what create_perm at from 733697.0 till -1.0
(add-prop [jane] (add-pred Wants to (add-pred Create what [c1])) 733697.0 1)
jane wants to create what c1 at 733697.0
(add-prop [pete] (add-pred Wants to (add-pred Create what [c2])) 733697.0 1)
pete wants to create what c2 at 733697.0
(add-prop [jane] (add-pred Wants to (add-pred Publish what [c1])) 733697.0 1)
jane wants to publish what c1 at 733697.0
(add-prop [pete] (add-pred Wants to (add-pred Publish what [c2])) 733697.0 1)
pete wants to publish what c2 at 733697.0
(add-prop [john] (add-pred Wants to (add-pred Publish what [c1])) 733697.0 1)
john wants to publish what c1 at 733697.0
----------running---------------------
jane create what c1 at 733697.0
jane isowner of c1 at from 733697.0 till -1.0
c1 has what private at from 733697.0 till -1.0
jane can what view what c1 at from 733697.0 till -1.0
manage_perm isneeded for_action view what c1 at from 733697.0 till -1.0
jane can what hide what c1 at from 733697.0 till -1.0
manage_perm isneeded for_action publish what c2 at from 733697.0 till -1.0
manage_perm isneeded for_action publish what c1 at from 733697.0 till -1.0
jane has what member at from 733697.0 till -1.0
jane has what basic_perm at from 733697.0 till -1.0
pete has what member at from 733697.0 till -1.0
pete has what basic_perm at from 733697.0 till -1.0
john has what member at from 733697.0 till -1.0
john has what basic_perm at from 733697.0 till -1.0
manager has what create_perm at from 733697.0 till -1.0
admin has what create_perm at from 733697.0 till -1.0
john has what create_perm at from 733697.0 till -1.0
manager has what manage_perm at from 733697.0 till -1.0
manager can what view what c1 at from 733697.0 till -1.0
manager can what publish what c2 at from 733697.0 till -1.0
manager can what publish what c1 at from 733697.0 till -1.0
admin has what manage_perm at from 733697.0 till -1.0
admin can what view what c1 at from 733697.0 till -1.0
admin can what publish what c2 at from 733697.0 till -1.0
admin can what publish what c1 at from 733697.0 till -1.0
john has what manage_perm at from 733697.0 till -1.0
john can what view what c1 at from 733697.0 till -1.0
john can what publish what c2 at from 733697.0 till -1.0
john can what publish what c1 at from 733697.0 till -1.0
john publish what c1 at 733697.0
c1 has what public at from 733697.0 till -1.0
basic_perm isneeded for_action view what c1 at from 733697.0 till -1.0
pete can what view what c1 at from 733697.0 till -1.0
member can what view what c1 at from 733697.0 till -1.0
manager has what basic_perm at from 733697.0 till -1.0
manager can what view what c1 at from 733697.0 till 733697.0
----------runned---------------------
(find-all-instances ((?prop Proposition) (?Y40 Has) (?Y41 Duration)) (and (eq ?prop:subject [c1]) (eq ?Y40:what [private]) (eq ?prop:predicate ?Y40) (= ?Y41:start 733697.0) (= ?Y41:end -1.0) (eq ?prop:truth 1)))
0


no
(defclass Name (is-a USER))
(deffunction reduce-class (?instance ?class) (if (eq (length$ (find-instance ((?a ?class)) (eq (instance-name ?a) ?instance))) 0) then (make-instance ?instance of ?class) (python-call tonl ?class ?instance)))
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


(deffunction mincomstart (?dur1 ?dur2)
    (return (max (send ?dur1 get-start) (send ?dur2 get-start)))
)



(deffunction maxcomend (?dur1 ?dur2)
    (bind ?e1 (send ?dur1 get-end))
    (bind ?e2 (send ?dur2 get-end))
    (if (= ?e1 ?e2) then (return ?e1))
    (if (= ?e2 -1) then
        (return ?e1)
    )
    (if (= ?e1 -1) then
        (return ?e2)
    )
    (return (min ?e1 ?e2))
)

(defclass Proposition (is-a Name) (slot truth (type INTEGER) (default 1) (pattern-match reactive)) (slot subject (type INSTANCE) (pattern-match reactive)) (slot predicate (type INSTANCE) (pattern-match reactive)) (slot time (type ?VARIABLE) (pattern-match reactive)))

(deffunction add-prop (?s ?p ?t ?r)
       (if (and (python-call ptonl ?s ?p ?t ?r)
                (= (+ (length$ (find-instance ((?prop Proposition) (?dur Duration))
                          (and (eq ?prop:subject ?s)
                               (eq ?prop:predicate ?p)
                               (eq ?prop:time ?dur)
                               (= ?dur:start (send ?t get-start))
                               (= ?dur:end (send ?t get-end))
                               (eq ?prop:truth ?r))))
                      (length$ (find-instance ((?prop Proposition))
                          (and (eq ?prop:subject ?s)
                               (eq ?prop:predicate ?p)
                               (= ?prop:time ?t)
                               (eq ?prop:truth ?r)))))
                 0))
        then (make-instance of Proposition (subject ?s)
                                           (predicate ?p)
                                           (time ?t)
                                           (truth ?r))
        else (return TRUE)))
------------OPEN---------------------
----------F-OPEN---------------------
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
admin
(reduce-class [member] Role)
member
(reduce-class [manager] Role)
manager
(reduce-class [basic_perm] Permission)
basic_perm
(reduce-class [manage_perm] Permission)
manage_perm
(reduce-class [create_perm] Permission)
create_perm
(reduce-class [public] Status)
public
(reduce-class [private] Status)
private
(add-prop [admin] (add-pred Has what [manager]) (make-instance of Duration (start 733697.0) (end -1.0)) 1)
admin has what manager at from 733697.0 till -1.0
(add-prop [member] (add-pred Has what [basic_perm]) (make-instance of Duration (start 733697.0) (end -1.0)) 1)
member has what basic_perm at from 733697.0 till -1.0
(defrule e89615055236494ca98d2a145f967a02 (logical (object (is-a Proposition) (subject ?X1&:(or (eq (class ?X1) Person) (subclassp (class ?X1) Person))) (predicate ?Y1&:(or (eq (class ?Y1) Wants) (subclassp (class ?Y1) Wants))&:(or (eq (class (send ?Y1 get-to)) Create) (subclassp (class (send ?Y1 get-to)) Create))&:(or (eq (class (send (send ?Y1 get-to) get-what)) Thing) (subclassp (class (send (send ?Y1 get-to) get-what)) Thing))) (time ?X2) (truth 1))) (logical (object (is-a Proposition) (subject ?X1) (predicate ?Y2&:(or (eq (class ?Y2) Has) (subclassp (class ?Y2) Has))&:(eq (send ?Y2 get-what) [create_perm])) (time ?X3&:(or (eq (class ?X3) Duration) (subclassp (class ?X3) Duration))) (truth 1))) (test (and (<= (send ?X3 get-start) ?X2) (or (= (send ?X3 get-end) -1) (>= (send ?X3 get-end) ?X2)))) => (add-prop ?X1 (add-pred Create what (send (send ?Y1 get-to) get-what)) ?X2 1))
(defrule 4e1e55372eec4886addee9a47a3ae116 (logical (object (is-a Proposition) (subject ?X1&:(or (eq (class ?X1) Person) (subclassp (class ?X1) Person))) (predicate ?Y4&:(or (eq (class ?Y4) Wants) (subclassp (class ?Y4) Wants))) (time ?X2) (truth 1))) (logical (object (is-a Proposition) (subject ?X1) (predicate ?Y5&:(or (eq (class ?Y5) Can) (subclassp (class ?Y5) Can))&:(eq (send ?Y4 get-to) (send ?Y5 get-what))) (time ?X3&:(or (eq (class ?X3) Duration) (subclassp (class ?X3) Duration))) (truth 1))) (test (and (<= (send ?X3 get-start) ?X2) (or (= (send ?X3 get-end) -1) (>= (send ?X3 get-end) ?X2)))) => (add-prop ?X1 (send ?Y4 get-to) ?X2 1))
(defrule 4b2483d87a884c239927c744a1e20c44 (logical (object (is-a Proposition) (subject ?X2&:(or (eq (class ?X2) Thing) (subclassp (class ?X2) Thing))) (predicate ?Y7&:(or (eq (class ?Y7) IsNeeded) (subclassp (class ?Y7) IsNeeded))) (time ?X3&:(or (eq (class ?X3) Duration) (subclassp (class ?X3) Duration))) (truth 1))) (logical (object (is-a Proposition) (subject ?X1&:(or (eq (class ?X1) Thing) (subclassp (class ?X1) Thing))) (predicate ?Y9&:(or (eq (class ?Y9) Has) (subclassp (class ?Y9) Has))&:(eq (send ?Y9 get-what) ?X2)) (time ?X5&:(or (eq (class ?X5) Duration) (subclassp (class ?X5) Duration))) (truth 1))) (test (or (and (>= (send ?X3 get-start) (send ?X3 get-start)) (or (<= (send ?X3 get-start) (send ?X3 get-end)) (= (send ?X3 get-end) -1))) (and (>= (send ?X3 get-start) (send ?X3 get-start)) (or (<= (send ?X3 get-start) (send ?X3 get-end)) (= (send ?X3 get-end) -1))))) => (add-prop ?X1 (add-pred Can what (send ?Y7 get-for_action)) (make-instance of Duration (start (mincomstart ?X3 ?X5)) (end (maxcomend ?X3 ?X5))) 1))
(defrule 9c75a581977749b7bb854ecd6ebc9cb3 (logical (object (is-a Proposition) (subject ?X1&:(or (eq (class ?X1) Thing) (subclassp (class ?X1) Thing))) (predicate ?Y11&:(or (eq (class ?Y11) IsIn) (subclassp (class ?Y11) IsIn))&:(or (eq (class (send ?Y11 get-what)) Thing) (subclassp (class (send ?Y11 get-what)) Thing))) (time ?X4&:(or (eq (class ?X4) Duration) (subclassp (class ?X4) Duration))) (truth 1))) (logical (object (is-a Proposition) (subject ?X2&:(eq ?X2 (send ?Y11 get-what))) (predicate ?Y13&:(or (eq (class ?Y13) IsIn) (subclassp (class ?Y13) IsIn))&:(or (eq (class (send ?Y13 get-what)) Thing) (subclassp (class (send ?Y13 get-what)) Thing))) (time ?X5&:(or (eq (class ?X5) Duration) (subclassp (class ?X5) Duration))) (truth 1))) (test (or (and (>= (send ?X4 get-start) (send ?X4 get-start)) (or (<= (send ?X4 get-start) (send ?X4 get-end)) (= (send ?X4 get-end) -1))) (and (>= (send ?X4 get-start) (send ?X4 get-start)) (or (<= (send ?X4 get-start) (send ?X4 get-end)) (= (send ?X4 get-end) -1))))) => (add-prop ?X1 (add-pred IsIn what (send ?Y13 get-what)) (make-instance of Duration (start (mincomstart ?X4 ?X5)) (end (maxcomend ?X4 ?X5))) 1))
(defrule 2072af564d9146618f01b611e0193ed4 (logical (object (is-a Proposition) (subject ?X1&:(or (eq (class ?X1) Person) (subclassp (class ?X1) Person))) (predicate ?Y15&:(or (eq (class ?Y15) IsIn) (subclassp (class ?Y15) IsIn))) (time ?X3&:(or (eq (class ?X3) Duration) (subclassp (class ?X3) Duration))) (truth 1))) (logical (object (is-a Proposition) (subject ?X2&:(or (eq (class ?X2) Group) (subclassp (class ?X2) Group))) (predicate ?Y17&:(or (eq (class ?Y17) Has) (subclassp (class ?Y17) Has))&:(or (eq (class (send ?Y17 get-what)) Permission) (subclassp (class (send ?Y17 get-what)) Permission))) (time ?X5&:(or (eq (class ?X5) Duration) (subclassp (class ?X5) Duration))) (truth 1))) (test (or (and (>= (send ?X3 get-start) (send ?X3 get-start)) (or (<= (send ?X3 get-start) (send ?X3 get-end)) (= (send ?X3 get-end) -1))) (and (>= (send ?X3 get-start) (send ?X3 get-start)) (or (<= (send ?X3 get-start) (send ?X3 get-end)) (= (send ?X3 get-end) -1))))) => (add-prop ?X1 (add-pred Has what (send ?Y17 get-what)) (make-instance of Duration (start (mincomstart ?X3 ?X5)) (end (maxcomend ?X3 ?X5))) 1))
(defrule 59c4ff2e23e24e5a861d77801fded4e4 (logical (object (is-a Proposition) (subject ?X1&:(or (eq (class ?X1) Person) (subclassp (class ?X1) Person))) (predicate ?Y19&:(or (eq (class ?Y19) Has) (subclassp (class ?Y19) Has))&:(or (eq (class (send ?Y19 get-what)) Role) (subclassp (class (send ?Y19 get-what)) Role))) (time ?X3&:(or (eq (class ?X3) Duration) (subclassp (class ?X3) Duration))) (truth 1))) (logical (object (is-a Proposition) (subject ?X2&:(eq ?X2 (send ?Y19 get-what))) (predicate ?Y21&:(or (eq (class ?Y21) Has) (subclassp (class ?Y21) Has))&:(or (eq (class (send ?Y21 get-what)) Permission) (subclassp (class (send ?Y21 get-what)) Permission))) (time ?X5&:(or (eq (class ?X5) Duration) (subclassp (class ?X5) Duration))) (truth 1))) (test (or (and (>= (send ?X3 get-start) (send ?X3 get-start)) (or (<= (send ?X3 get-start) (send ?X3 get-end)) (= (send ?X3 get-end) -1))) (and (>= (send ?X3 get-start) (send ?X3 get-start)) (or (<= (send ?X3 get-start) (send ?X3 get-end)) (= (send ?X3 get-end) -1))))) => (add-prop ?X1 (add-pred Has what (send ?Y21 get-what)) (make-instance of Duration (start (mincomstart ?X3 ?X5)) (end (maxcomend ?X3 ?X5))) 1))
(defrule f3be9a912e1744d5b4a79f208b1c9d7d (logical (object (is-a Person) (name ?X1))) => (add-prop ?X1 (add-pred Has what [member]) (make-instance of Duration (start 733697.0) (end -1.0)) 1))
(defrule 1619861135604123965243dfc8c5b232 (logical (object (is-a Proposition) (subject ?X1&:(or (eq (class ?X1) Person) (subclassp (class ?X1) Person))) (predicate ?Y23&:(or (eq (class ?Y23) Create) (subclassp (class ?Y23) Create))&:(or (eq (class (send ?Y23 get-what)) Content) (subclassp (class (send ?Y23 get-what)) Content))) (time ?X3) (truth 1))) => (reduce-class (send ?Y23 get-what) Content) (add-prop ?X1 (add-pred IsOwner of (send ?Y23 get-what)) (make-instance of Duration (start ?X3) (end -1.0)) 1) (add-prop (send ?Y23 get-what) (add-pred Has what [private]) (make-instance of Duration (start ?X3) (end -1.0)) 1))
(defrule d81ae0a6b46146fb851fd38630716fca (logical (object (is-a Permission) (name ?X2))) => (add-prop [manager] (add-pred Has what ?X2) (make-instance of Duration (start 733697.0) (end -1.0)) 1))
(defrule dcdff370b6674d9bb571aeab1b700331 (logical (object (is-a Proposition) (subject ?X1&:(or (eq (class ?X1) Content) (subclassp (class ?X1) Content))) (predicate ?Y24&:(or (eq (class ?Y24) Has) (subclassp (class ?Y24) Has))&:(eq (send ?Y24 get-what) [public])) (time ?X2&:(or (eq (class ?X2) Duration) (subclassp (class ?X2) Duration))) (truth 1))) => (add-prop [basic_perm] (add-pred IsNeeded for_action (add-pred View what ?X1)) ?X2 1))
(defrule 78bd208902b84ccfbfed9178168a475d (logical (object (is-a Proposition) (subject ?X1&:(or (eq (class ?X1) Content) (subclassp (class ?X1) Content))) (predicate ?Y26&:(or (eq (class ?Y26) Has) (subclassp (class ?Y26) Has))&:(eq (send ?Y26 get-what) [private])) (time ?X2&:(or (eq (class ?X2) Duration) (subclassp (class ?X2) Duration))) (truth 1))) => (add-prop [manage_perm] (add-pred IsNeeded for_action (add-pred View what ?X1)) ?X2 1))
(defrule 8725629caaf64597a112035166a87fc3 (logical (object (is-a Proposition) (subject ?X1&:(or (eq (class ?X1) Content) (subclassp (class ?X1) Content))) (predicate ?Y28&:(or (eq (class ?Y28) Has) (subclassp (class ?Y28) Has))&:(eq (send ?Y28 get-what) [private])) (time ?X3&:(or (eq (class ?X3) Duration) (subclassp (class ?X3) Duration))) (truth 1))) (logical (object (is-a Proposition) (subject ?X2&:(or (eq (class ?X2) Person) (subclassp (class ?X2) Person))) (predicate ?Y30&:(or (eq (class ?Y30) IsOwner) (subclassp (class ?Y30) IsOwner))&:(eq (send ?Y30 get-of) ?X1)) (time ?X4&:(or (eq (class ?X4) Duration) (subclassp (class ?X4) Duration))) (truth 1))) (test (or (and (>= (send ?X3 get-start) (send ?X3 get-start)) (or (<= (send ?X3 get-start) (send ?X3 get-end)) (= (send ?X3 get-end) -1))) (and (>= (send ?X3 get-start) (send ?X3 get-start)) (or (<= (send ?X3 get-start) (send ?X3 get-end)) (= (send ?X3 get-end) -1))))) => (add-prop ?X2 (add-pred Can what (add-pred View what ?X1)) (make-instance of Duration (start (mincomstart ?X3 ?X4)) (end (maxcomend ?X3 ?X4))) 1))
(defrule ce3fcdb04f4f41ac8cf3833706fccd17 (logical (object (is-a Proposition) (subject ?X1&:(or (eq (class ?X1) Person) (subclassp (class ?X1) Person))) (predicate ?Y32&:(or (eq (class ?Y32) Publish) (subclassp (class ?Y32) Publish))&:(or (eq (class (send ?Y32 get-what)) Content) (subclassp (class (send ?Y32 get-what)) Content))) (time ?X3) (truth 1))) (logical (object (is-a Proposition) (subject ?X2&:(eq ?X2 (send ?Y32 get-what))) (predicate ?Y33&:(or (eq (class ?Y33) Has) (subclassp (class ?Y33) Has))&:(or (eq (class (send ?Y33 get-what)) Status) (subclassp (class (send ?Y33 get-what)) Status))) (time ?X5&:(or (eq (class ?X5) Duration) (subclassp (class ?X5) Duration))) (truth 1))) => (send ?X5 put-end 733697) (add-prop (send ?Y32 get-what) (add-pred Has what [public]) (make-instance of Duration (start ?X3) (end -1.0)) 1))
(defrule 0170f8d6cd834f4280a807115e0442b2 (logical (object (is-a Content) (name ?X1))) => (add-prop [manage_perm] (add-pred IsNeeded for_action (add-pred Publish what ?X1)) (make-instance of Duration (start 733697.0) (end -1.0)) 1))
(defrule 6703649688aa497c96cea6df07646027 (logical (object (is-a Proposition) (subject ?X1&:(or (eq (class ?X1) Person) (subclassp (class ?X1) Person))) (predicate ?Y35&:(or (eq (class ?Y35) Hide) (subclassp (class ?Y35) Hide))&:(or (eq (class (send ?Y35 get-what)) Content) (subclassp (class (send ?Y35 get-what)) Content))) (time ?X3) (truth 1))) (logical (object (is-a Proposition) (subject ?X2&:(eq ?X2 (send ?Y35 get-what))) (predicate ?Y36&:(or (eq (class ?Y36) Has) (subclassp (class ?Y36) Has))&:(or (eq (class (send ?Y36 get-what)) Status) (subclassp (class (send ?Y36 get-what)) Status))) (time ?X5&:(or (eq (class ?X5) Duration) (subclassp (class ?X5) Duration))) (truth 1))) => (send ?X5 put-end 733697) (add-prop (send ?Y35 get-what) (add-pred Has what [private]) (make-instance of Duration (start ?X3) (end -1.0)) 1))
(defrule fd44fc6169c7449ca27cffe2c5596549 (logical (object (is-a Proposition) (subject ?X1&:(or (eq (class ?X1) Person) (subclassp (class ?X1) Person))) (predicate ?Y38&:(or (eq (class ?Y38) IsOwner) (subclassp (class ?Y38) IsOwner))&:(or (eq (class (send ?Y38 get-of)) Content) (subclassp (class (send ?Y38 get-of)) Content))) (time ?X3&:(or (eq (class ?X3) Duration) (subclassp (class ?X3) Duration))) (truth 1))) => (add-prop ?X1 (add-pred Can what (add-pred Hide what (send ?Y38 get-of))) ?X3 1))
(reduce-class [john] Person)
john
(reduce-class [pete] Person)
pete
(reduce-class [jane] Person)
jane
(reduce-class [c1] Content)
c1
(reduce-class [c2] Content)
c2
(add-prop [john] (add-pred Has what [manager]) (make-instance of Duration (start 733697.0) (end -1.0)) 1)
john has what manager at from 733697.0 till -1.0
(add-prop [jane] (add-pred Has what [create_perm]) (make-instance of Duration (start 733697.0) (end -1.0)) 1)
jane has what create_perm at from 733697.0 till -1.0
(add-prop [jane] (add-pred Wants to (add-pred Create what [c1])) 733697.0 1)
jane wants to create what c1 at 733697.0
(add-prop [pete] (add-pred Wants to (add-pred Create what [c2])) 733697.0 1)
pete wants to create what c2 at 733697.0
(add-prop [jane] (add-pred Wants to (add-pred Publish what [c1])) 733697.0 1)
jane wants to publish what c1 at 733697.0
(add-prop [pete] (add-pred Wants to (add-pred Publish what [c2])) 733697.0 1)
pete wants to publish what c2 at 733697.0
(add-prop [john] (add-pred Wants to (add-pred Publish what [c1])) 733697.0 1)
john wants to publish what c1 at 733697.0
----------running---------------------
----------runned---------------------
(defclass Name (is-a USER))
(deffunction reduce-class (?instance ?class) (if (eq (length$ (find-instance ((?a ?class)) (eq (instance-name ?a) ?instance))) 0) then (make-instance ?instance of ?class) (python-call tonl ?class ?instance)))
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


(deffunction mincomstart (?dur1 ?dur2)
    (return (max (send ?dur1 get-start) (send ?dur2 get-start)))
)



(deffunction maxcomend (?dur1 ?dur2)
    (bind ?e1 (send ?dur1 get-end))
    (bind ?e2 (send ?dur2 get-end))
    (if (= ?e1 ?e2) then (return ?e1))
    (if (= ?e2 -1) then
        (return ?e1)
    )
    (if (= ?e1 -1) then
        (return ?e2)
    )
    (return (min ?e1 ?e2))
)

(defclass Proposition (is-a Name) (slot truth (type INTEGER) (default 1) (pattern-match reactive)) (slot subject (type INSTANCE) (pattern-match reactive)) (slot predicate (type INSTANCE) (pattern-match reactive)) (slot time (type ?VARIABLE) (pattern-match reactive)))

(deffunction add-prop (?s ?p ?t ?r)
       (if (and (python-call ptonl ?s ?p ?t ?r)
                (= (+ (length$ (find-instance ((?prop Proposition) (?dur Duration))
                          (and (eq ?prop:subject ?s)
                               (eq ?prop:predicate ?p)
                               (eq ?prop:time ?dur)
                               (= ?dur:start (send ?t get-start))
                               (= ?dur:end (send ?t get-end))
                               (eq ?prop:truth ?r))))
                      (length$ (find-instance ((?prop Proposition))
                          (and (eq ?prop:subject ?s)
                               (eq ?prop:predicate ?p)
                               (= ?prop:time ?t)
                               (eq ?prop:truth ?r)))))
                 0))
        then (make-instance of Proposition (subject ?s)
                                           (predicate ?p)
                                           (time ?t)
                                           (truth ?r))
        else (return TRUE)))
------------OPEN---------------------
----------F-OPEN---------------------
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
admin
(reduce-class [member] Role)
member
(reduce-class [manager] Role)
manager
(reduce-class [basic_perm] Permission)
basic_perm
(reduce-class [manage_perm] Permission)
manage_perm
(reduce-class [create_perm] Permission)
create_perm
(reduce-class [public] Status)
public
(reduce-class [private] Status)
private
(add-prop [admin] (add-pred Has what [manager]) (make-instance of Duration (start 733697.0) (end -1.0)) 1)
(defclass Name (is-a USER))
(deffunction reduce-class (?instance ?class) (if (eq (length$ (find-instance ((?a ?class)) (eq (instance-name ?a) ?instance))) 0) then (make-instance ?instance of ?class) (python-call tonl ?class ?instance)))
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


(deffunction mincomstart (?dur1 ?dur2)
    (return (max (send ?dur1 get-start) (send ?dur2 get-start)))
)



(deffunction maxcomend (?dur1 ?dur2)
    (bind ?e1 (send ?dur1 get-end))
    (bind ?e2 (send ?dur2 get-end))
    (if (= ?e1 ?e2) then (return ?e1))
    (if (= ?e2 -1) then
        (return ?e1)
    )
    (if (= ?e1 -1) then
        (return ?e2)
    )
    (return (min ?e1 ?e2))
)

(defclass Proposition (is-a Name) (slot truth (type INTEGER) (default 1) (pattern-match reactive)) (slot subject (type INSTANCE) (pattern-match reactive)) (slot predicate (type INSTANCE) (pattern-match reactive)) (slot time (type ?VARIABLE) (pattern-match reactive)))

(deffunction add-prop (?s ?p ?t ?r)
       (if (and (python-call ptonl ?s ?p ?t ?r)
                (= (+ (length$ (find-instance ((?prop Proposition) (?dur Duration))
                          (and (eq ?prop:subject ?s)
                               (eq ?prop:predicate ?p)
                               (eq ?prop:time ?dur)
                               (= ?dur:start (send ?t get-start))
                               (= ?dur:end (send ?t get-end))
                               (eq ?prop:truth ?r))))
                      (length$ (find-instance ((?prop Proposition))
                          (and (eq ?prop:subject ?s)
                               (eq ?prop:predicate ?p)
                               (= ?prop:time ?t)
                               (eq ?prop:truth ?r)))))
                 0))
        then (make-instance of Proposition (subject ?s)
                                           (predicate ?p)
                                           (time ?t)
                                           (truth ?r))
        else (return TRUE)))
------------OPEN---------------------
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
admin
(reduce-class [member] Role)
member
(reduce-class [manager] Role)
manager
(reduce-class [basic_perm] Permission)
basic_perm
(reduce-class [manage_perm] Permission)
manage_perm
(reduce-class [create_perm] Permission)
create_perm
(reduce-class [public] Status)
public
(reduce-class [private] Status)
private
(add-prop [admin] (add-pred Has what [manager]) (make-instance of Duration (start 733697.0) (end -1.0)) 1)
admin has what manager at from 733697.0 till -1.0
(add-prop [member] (add-pred Has what [basic_perm]) (make-instance of Duration (start 733697.0) (end -1.0)) 1)
member has what basic_perm at from 733697.0 till -1.0
(defrule 189c8bde18f94f7b9ed08fb21f785ff3 (logical (object (is-a Proposition) (subject ?X1&:(or (eq (class ?X1) Person) (subclassp (class ?X1) Person))) (predicate ?Y1&:(or (eq (class ?Y1) Wants) (subclassp (class ?Y1) Wants))&:(or (eq (class (send ?Y1 get-to)) Create) (subclassp (class (send ?Y1 get-to)) Create))&:(or (eq (class (send (send ?Y1 get-to) get-what)) Thing) (subclassp (class (send (send ?Y1 get-to) get-what)) Thing))) (time ?X2) (truth 1))) (logical (object (is-a Proposition) (subject ?X1) (predicate ?Y2&:(or (eq (class ?Y2) Has) (subclassp (class ?Y2) Has))&:(eq (send ?Y2 get-what) [create_perm])) (time ?X3&:(or (eq (class ?X3) Duration) (subclassp (class ?X3) Duration))) (truth 1))) (test (and (<= (send ?X3 get-start) ?X2) (or (= (send ?X3 get-end) -1) (>= (send ?X3 get-end) ?X2)))) => (add-prop ?X1 (add-pred Create what (send (send ?Y1 get-to) get-what)) ?X2 1))
(defrule 42bf643ccbbc4775a7b3228c5f024e1d (logical (object (is-a Proposition) (subject ?X1&:(or (eq (class ?X1) Person) (subclassp (class ?X1) Person))) (predicate ?Y4&:(or (eq (class ?Y4) Wants) (subclassp (class ?Y4) Wants))) (time ?X2) (truth 1))) (logical (object (is-a Proposition) (subject ?X1) (predicate ?Y5&:(or (eq (class ?Y5) Can) (subclassp (class ?Y5) Can))&:(eq (send ?Y4 get-to) (send ?Y5 get-what))) (time ?X3&:(or (eq (class ?X3) Duration) (subclassp (class ?X3) Duration))) (truth 1))) (test (and (<= (send ?X3 get-start) ?X2) (or (= (send ?X3 get-end) -1) (>= (send ?X3 get-end) ?X2)))) => (add-prop ?X1 (send ?Y4 get-to) ?X2 1))
(defrule 05c6ced2e45c434b8d7ab3c5a903f0be (logical (object (is-a Proposition) (subject ?X2&:(or (eq (class ?X2) Thing) (subclassp (class ?X2) Thing))) (predicate ?Y7&:(or (eq (class ?Y7) IsNeeded) (subclassp (class ?Y7) IsNeeded))) (time ?X3&:(or (eq (class ?X3) Duration) (subclassp (class ?X3) Duration))) (truth 1))) (logical (object (is-a Proposition) (subject ?X1&:(or (eq (class ?X1) Thing) (subclassp (class ?X1) Thing))) (predicate ?Y9&:(or (eq (class ?Y9) Has) (subclassp (class ?Y9) Has))&:(eq (send ?Y9 get-what) ?X2)) (time ?X5&:(or (eq (class ?X5) Duration) (subclassp (class ?X5) Duration))) (truth 1))) (test (or (and (>= (send ?X3 get-start) (send ?X3 get-start)) (or (<= (send ?X3 get-start) (send ?X3 get-end)) (= (send ?X3 get-end) -1))) (and (>= (send ?X3 get-start) (send ?X3 get-start)) (or (<= (send ?X3 get-start) (send ?X3 get-end)) (= (send ?X3 get-end) -1))))) => (add-prop ?X1 (add-pred Can what (send ?Y7 get-for_action)) (make-instance of Duration (start (mincomstart ?X3 ?X5)) (end (maxcomend ?X3 ?X5))) 1))
(defrule f30a0513b1e7497d9b1959b9dafbb785 (logical (object (is-a Proposition) (subject ?X1&:(or (eq (class ?X1) Thing) (subclassp (class ?X1) Thing))) (predicate ?Y11&:(or (eq (class ?Y11) IsIn) (subclassp (class ?Y11) IsIn))&:(or (eq (class (send ?Y11 get-what)) Thing) (subclassp (class (send ?Y11 get-what)) Thing))) (time ?X4&:(or (eq (class ?X4) Duration) (subclassp (class ?X4) Duration))) (truth 1))) (logical (object (is-a Proposition) (subject ?X2&:(eq ?X2 (send ?Y11 get-what))) (predicate ?Y13&:(or (eq (class ?Y13) IsIn) (subclassp (class ?Y13) IsIn))&:(or (eq (class (send ?Y13 get-what)) Thing) (subclassp (class (send ?Y13 get-what)) Thing))) (time ?X5&:(or (eq (class ?X5) Duration) (subclassp (class ?X5) Duration))) (truth 1))) (test (or (and (>= (send ?X4 get-start) (send ?X4 get-start)) (or (<= (send ?X4 get-start) (send ?X4 get-end)) (= (send ?X4 get-end) -1))) (and (>= (send ?X4 get-start) (send ?X4 get-start)) (or (<= (send ?X4 get-start) (send ?X4 get-end)) (= (send ?X4 get-end) -1))))) => (add-prop ?X1 (add-pred IsIn what (send ?Y13 get-what)) (make-instance of Duration (start (mincomstart ?X4 ?X5)) (end (maxcomend ?X4 ?X5))) 1))
(defrule 92bd1f2cd14b4242a0531a65745fe2dd (logical (object (is-a Proposition) (subject ?X1&:(or (eq (class ?X1) Person) (subclassp (class ?X1) Person))) (predicate ?Y15&:(or (eq (class ?Y15) IsIn) (subclassp (class ?Y15) IsIn))) (time ?X3&:(or (eq (class ?X3) Duration) (subclassp (class ?X3) Duration))) (truth 1))) (logical (object (is-a Proposition) (subject ?X2&:(or (eq (class ?X2) Group) (subclassp (class ?X2) Group))) (predicate ?Y17&:(or (eq (class ?Y17) Has) (subclassp (class ?Y17) Has))&:(or (eq (class (send ?Y17 get-what)) Permission) (subclassp (class (send ?Y17 get-what)) Permission))) (time ?X5&:(or (eq (class ?X5) Duration) (subclassp (class ?X5) Duration))) (truth 1))) (test (or (and (>= (send ?X3 get-start) (send ?X3 get-start)) (or (<= (send ?X3 get-start) (send ?X3 get-end)) (= (send ?X3 get-end) -1))) (and (>= (send ?X3 get-start) (send ?X3 get-start)) (or (<= (send ?X3 get-start) (send ?X3 get-end)) (= (send ?X3 get-end) -1))))) => (add-prop ?X1 (add-pred Has what (send ?Y17 get-what)) (make-instance of Duration (start (mincomstart ?X3 ?X5)) (end (maxcomend ?X3 ?X5))) 1))
(defrule f1a8cfe912f044b386ba818b60208dc0 (logical (object (is-a Proposition) (subject ?X1&:(or (eq (class ?X1) Person) (subclassp (class ?X1) Person))) (predicate ?Y19&:(or (eq (class ?Y19) Has) (subclassp (class ?Y19) Has))&:(or (eq (class (send ?Y19 get-what)) Role) (subclassp (class (send ?Y19 get-what)) Role))) (time ?X3&:(or (eq (class ?X3) Duration) (subclassp (class ?X3) Duration))) (truth 1))) (logical (object (is-a Proposition) (subject ?X2&:(eq ?X2 (send ?Y19 get-what))) (predicate ?Y21&:(or (eq (class ?Y21) Has) (subclassp (class ?Y21) Has))&:(or (eq (class (send ?Y21 get-what)) Permission) (subclassp (class (send ?Y21 get-what)) Permission))) (time ?X5&:(or (eq (class ?X5) Duration) (subclassp (class ?X5) Duration))) (truth 1))) (test (or (and (>= (send ?X3 get-start) (send ?X3 get-start)) (or (<= (send ?X3 get-start) (send ?X3 get-end)) (= (send ?X3 get-end) -1))) (and (>= (send ?X3 get-start) (send ?X3 get-start)) (or (<= (send ?X3 get-start) (send ?X3 get-end)) (= (send ?X3 get-end) -1))))) => (add-prop ?X1 (add-pred Has what (send ?Y21 get-what)) (make-instance of Duration (start (mincomstart ?X3 ?X5)) (end (maxcomend ?X3 ?X5))) 1))
(defrule 5e390f7b6ab641a49ec105e68e9723b6 (logical (object (is-a Person) (name ?X1))) => (add-prop ?X1 (add-pred Has what [member]) (make-instance of Duration (start 733697.0) (end -1.0)) 1))
(defrule 59c093a433674c19bf42cb820569cdaf (logical (object (is-a Proposition) (subject ?X1&:(or (eq (class ?X1) Person) (subclassp (class ?X1) Person))) (predicate ?Y23&:(or (eq (class ?Y23) Create) (subclassp (class ?Y23) Create))&:(or (eq (class (send ?Y23 get-what)) Content) (subclassp (class (send ?Y23 get-what)) Content))) (time ?X3) (truth 1))) => (reduce-class (send ?Y23 get-what) Content) (add-prop ?X1 (add-pred IsOwner of (send ?Y23 get-what)) (make-instance of Duration (start ?X3) (end -1.0)) 1) (add-prop (send ?Y23 get-what) (add-pred Has what [private]) (make-instance of Duration (start ?X3) (end -1.0)) 1))
(defrule 82329082f7494ddb93e3c082117773e7 (logical (object (is-a Permission) (name ?X2))) => (add-prop [manager] (add-pred Has what ?X2) (make-instance of Duration (start 733697.0) (end -1.0)) 1))
(defrule 038df7889ec740ae9b5227517a092c3e (logical (object (is-a Proposition) (subject ?X1&:(or (eq (class ?X1) Content) (subclassp (class ?X1) Content))) (predicate ?Y24&:(or (eq (class ?Y24) Has) (subclassp (class ?Y24) Has))&:(eq (send ?Y24 get-what) [public])) (time ?X2&:(or (eq (class ?X2) Duration) (subclassp (class ?X2) Duration))) (truth 1))) => (add-prop [basic_perm] (add-pred IsNeeded for_action (add-pred View what ?X1)) ?X2 1))
(defrule 5aaf460948e941b0be74c9ec9beca255 (logical (object (is-a Proposition) (subject ?X1&:(or (eq (class ?X1) Content) (subclassp (class ?X1) Content))) (predicate ?Y26&:(or (eq (class ?Y26) Has) (subclassp (class ?Y26) Has))&:(eq (send ?Y26 get-what) [private])) (time ?X2&:(or (eq (class ?X2) Duration) (subclassp (class ?X2) Duration))) (truth 1))) => (add-prop [manage_perm] (add-pred IsNeeded for_action (add-pred View what ?X1)) ?X2 1))
(defrule 33512ab543c845298806a482225c3579 (logical (object (is-a Proposition) (subject ?X1&:(or (eq (class ?X1) Content) (subclassp (class ?X1) Content))) (predicate ?Y28&:(or (eq (class ?Y28) Has) (subclassp (class ?Y28) Has))&:(eq (send ?Y28 get-what) [private])) (time ?X3&:(or (eq (class ?X3) Duration) (subclassp (class ?X3) Duration))) (truth 1))) (logical (object (is-a Proposition) (subject ?X2&:(or (eq (class ?X2) Person) (subclassp (class ?X2) Person))) (predicate ?Y30&:(or (eq (class ?Y30) IsOwner) (subclassp (class ?Y30) IsOwner))&:(eq (send ?Y30 get-of) ?X1)) (time ?X4&:(or (eq (class ?X4) Duration) (subclassp (class ?X4) Duration))) (truth 1))) (test (or (and (>= (send ?X3 get-start) (send ?X3 get-start)) (or (<= (send ?X3 get-start) (send ?X3 get-end)) (= (send ?X3 get-end) -1))) (and (>= (send ?X3 get-start) (send ?X3 get-start)) (or (<= (send ?X3 get-start) (send ?X3 get-end)) (= (send ?X3 get-end) -1))))) => (add-prop ?X2 (add-pred Can what (add-pred View what ?X1)) (make-instance of Duration (start (mincomstart ?X3 ?X4)) (end (maxcomend ?X3 ?X4))) 1))
(defrule 5078fc97c0ab405ba92665304e167c33 (logical (object (is-a Proposition) (subject ?X1&:(or (eq (class ?X1) Person) (subclassp (class ?X1) Person))) (predicate ?Y32&:(or (eq (class ?Y32) Publish) (subclassp (class ?Y32) Publish))&:(or (eq (class (send ?Y32 get-what)) Content) (subclassp (class (send ?Y32 get-what)) Content))) (time ?X3) (truth 1))) (logical (object (is-a Proposition) (subject ?X2&:(eq ?X2 (send ?Y32 get-what))) (predicate ?Y33&:(or (eq (class ?Y33) Has) (subclassp (class ?Y33) Has))&:(or (eq (class (send ?Y33 get-what)) Status) (subclassp (class (send ?Y33 get-what)) Status))) (time ?X5&:(or (eq (class ?X5) Duration) (subclassp (class ?X5) Duration))) (truth 1))) => (send ?X5 put-end 733697) (add-prop (send ?Y32 get-what) (add-pred Has what [public]) (make-instance of Duration (start ?X3) (end -1.0)) 1))
(defrule d4d815ae33bd49b7acafbdf953294c28 (logical (object (is-a Content) (name ?X1))) => (add-prop [manage_perm] (add-pred IsNeeded for_action (add-pred Publish what ?X1)) (make-instance of Duration (start 733697.0) (end -1.0)) 1))
(defrule 5bd5bad3c0ea4560a7010a7737fe662f (logical (object (is-a Proposition) (subject ?X1&:(or (eq (class ?X1) Person) (subclassp (class ?X1) Person))) (predicate ?Y35&:(or (eq (class ?Y35) Hide) (subclassp (class ?Y35) Hide))&:(or (eq (class (send ?Y35 get-what)) Content) (subclassp (class (send ?Y35 get-what)) Content))) (time ?X3) (truth 1))) (logical (object (is-a Proposition) (subject ?X2&:(eq ?X2 (send ?Y35 get-what))) (predicate ?Y36&:(or (eq (class ?Y36) Has) (subclassp (class ?Y36) Has))&:(or (eq (class (send ?Y36 get-what)) Status) (subclassp (class (send ?Y36 get-what)) Status))) (time ?X5&:(or (eq (class ?X5) Duration) (subclassp (class ?X5) Duration))) (truth 1))) => (send ?X5 put-end 733697) (add-prop (send ?Y35 get-what) (add-pred Has what [private]) (make-instance of Duration (start ?X3) (end -1.0)) 1))
(defrule f0771a52098f4dc6b4ac2dab262c9877 (logical (object (is-a Proposition) (subject ?X1&:(or (eq (class ?X1) Person) (subclassp (class ?X1) Person))) (predicate ?Y38&:(or (eq (class ?Y38) IsOwner) (subclassp (class ?Y38) IsOwner))&:(or (eq (class (send ?Y38 get-of)) Content) (subclassp (class (send ?Y38 get-of)) Content))) (time ?X3&:(or (eq (class ?X3) Duration) (subclassp (class ?X3) Duration))) (truth 1))) => (add-prop ?X1 (add-pred Can what (add-pred Hide what (send ?Y38 get-of))) ?X3 1))
(reduce-class [admin] Person)
(reduce-class [basic_perm] Permission)
(reduce-class [create_perm] Permission)
(reduce-class [manage_perm] Permission)
(reduce-class [manager] Role)
(reduce-class [member] Role)
(reduce-class [private] Status)
(reduce-class [public] Status)
(defrule 038df7889ec740ae9b5227517a092c3e (logical (object (is-a Proposition) (subject ?X1&:(or (eq (class ?X1) Content) (subclassp (class ?X1) Content))) (predicate ?Y40&:(or (eq (class ?Y40) Has) (subclassp (class ?Y40) Has))&:(eq (send ?Y40 get-what) [public])) (time ?X2&:(or (eq (class ?X2) Duration) (subclassp (class ?X2) Duration))) (truth 1))) => (add-prop [basic_perm] (add-pred IsNeeded for_action (add-pred View what ?X1)) ?X2 1))
(defrule 05c6ced2e45c434b8d7ab3c5a903f0be (logical (object (is-a Proposition) (subject ?X2&:(or (eq (class ?X2) Thing) (subclassp (class ?X2) Thing))) (predicate ?Y42&:(or (eq (class ?Y42) IsNeeded) (subclassp (class ?Y42) IsNeeded))) (time ?X3&:(or (eq (class ?X3) Duration) (subclassp (class ?X3) Duration))) (truth 1))) (logical (object (is-a Proposition) (subject ?X1&:(or (eq (class ?X1) Thing) (subclassp (class ?X1) Thing))) (predicate ?Y44&:(or (eq (class ?Y44) Has) (subclassp (class ?Y44) Has))&:(eq (send ?Y44 get-what) ?X2)) (time ?X5&:(or (eq (class ?X5) Duration) (subclassp (class ?X5) Duration))) (truth 1))) (test (or (and (>= (send ?X3 get-start) (send ?X3 get-start)) (or (<= (send ?X3 get-start) (send ?X3 get-end)) (= (send ?X3 get-end) -1))) (and (>= (send ?X3 get-start) (send ?X3 get-start)) (or (<= (send ?X3 get-start) (send ?X3 get-end)) (= (send ?X3 get-end) -1))))) => (add-prop ?X1 (add-pred Can what (send ?Y42 get-for_action)) (make-instance of Duration (start (mincomstart ?X3 ?X5)) (end (maxcomend ?X3 ?X5))) 1))
(defrule 189c8bde18f94f7b9ed08fb21f785ff3 (logical (object (is-a Proposition) (subject ?X1&:(or (eq (class ?X1) Person) (subclassp (class ?X1) Person))) (predicate ?Y46&:(or (eq (class ?Y46) Wants) (subclassp (class ?Y46) Wants))&:(or (eq (class (send ?Y46 get-to)) Create) (subclassp (class (send ?Y46 get-to)) Create))&:(or (eq (class (send (send ?Y46 get-to) get-what)) Thing) (subclassp (class (send (send ?Y46 get-to) get-what)) Thing))) (time ?X2) (truth 1))) (logical (object (is-a Proposition) (subject ?X1) (predicate ?Y47&:(or (eq (class ?Y47) Has) (subclassp (class ?Y47) Has))&:(eq (send ?Y47 get-what) [create_perm])) (time ?X3&:(or (eq (class ?X3) Duration) (subclassp (class ?X3) Duration))) (truth 1))) (test (and (<= (send ?X3 get-start) ?X2) (or (= (send ?X3 get-end) -1) (>= (send ?X3 get-end) ?X2)))) => (add-prop ?X1 (add-pred Create what (send (send ?Y46 get-to) get-what)) ?X2 1))
(defrule 33512ab543c845298806a482225c3579 (logical (object (is-a Proposition) (subject ?X1&:(or (eq (class ?X1) Content) (subclassp (class ?X1) Content))) (predicate ?Y49&:(or (eq (class ?Y49) Has) (subclassp (class ?Y49) Has))&:(eq (send ?Y49 get-what) [private])) (time ?X3&:(or (eq (class ?X3) Duration) (subclassp (class ?X3) Duration))) (truth 1))) (logical (object (is-a Proposition) (subject ?X2&:(or (eq (class ?X2) Person) (subclassp (class ?X2) Person))) (predicate ?Y51&:(or (eq (class ?Y51) IsOwner) (subclassp (class ?Y51) IsOwner))&:(eq (send ?Y51 get-of) ?X1)) (time ?X4&:(or (eq (class ?X4) Duration) (subclassp (class ?X4) Duration))) (truth 1))) (test (or (and (>= (send ?X3 get-start) (send ?X3 get-start)) (or (<= (send ?X3 get-start) (send ?X3 get-end)) (= (send ?X3 get-end) -1))) (and (>= (send ?X3 get-start) (send ?X3 get-start)) (or (<= (send ?X3 get-start) (send ?X3 get-end)) (= (send ?X3 get-end) -1))))) => (add-prop ?X2 (add-pred Can what (add-pred View what ?X1)) (make-instance of Duration (start (mincomstart ?X3 ?X4)) (end (maxcomend ?X3 ?X4))) 1))
(defrule 42bf643ccbbc4775a7b3228c5f024e1d (logical (object (is-a Proposition) (subject ?X1&:(or (eq (class ?X1) Person) (subclassp (class ?X1) Person))) (predicate ?Y53&:(or (eq (class ?Y53) Wants) (subclassp (class ?Y53) Wants))) (time ?X2) (truth 1))) (logical (object (is-a Proposition) (subject ?X1) (predicate ?Y54&:(or (eq (class ?Y54) Can) (subclassp (class ?Y54) Can))&:(eq (send ?Y53 get-to) (send ?Y54 get-what))) (time ?X3&:(or (eq (class ?X3) Duration) (subclassp (class ?X3) Duration))) (truth 1))) (test (and (<= (send ?X3 get-start) ?X2) (or (= (send ?X3 get-end) -1) (>= (send ?X3 get-end) ?X2)))) => (add-prop ?X1 (send ?Y53 get-to) ?X2 1))
(defrule 5078fc97c0ab405ba92665304e167c33 (logical (object (is-a Proposition) (subject ?X1&:(or (eq (class ?X1) Person) (subclassp (class ?X1) Person))) (predicate ?Y56&:(or (eq (class ?Y56) Publish) (subclassp (class ?Y56) Publish))&:(or (eq (class (send ?Y56 get-what)) Content) (subclassp (class (send ?Y56 get-what)) Content))) (time ?X3) (truth 1))) (logical (object (is-a Proposition) (subject ?X2&:(eq ?X2 (send ?Y56 get-what))) (predicate ?Y57&:(or (eq (class ?Y57) Has) (subclassp (class ?Y57) Has))&:(or (eq (class (send ?Y57 get-what)) Status) (subclassp (class (send ?Y57 get-what)) Status))) (time ?X5&:(or (eq (class ?X5) Duration) (subclassp (class ?X5) Duration))) (truth 1))) => (send ?X5 put-end 733697) (add-prop (send ?Y56 get-what) (add-pred Has what [public]) (make-instance of Duration (start ?X3) (end -1.0)) 1))
(defrule 59c093a433674c19bf42cb820569cdaf (logical (object (is-a Proposition) (subject ?X1&:(or (eq (class ?X1) Person) (subclassp (class ?X1) Person))) (predicate ?Y59&:(or (eq (class ?Y59) Create) (subclassp (class ?Y59) Create))&:(or (eq (class (send ?Y59 get-what)) Content) (subclassp (class (send ?Y59 get-what)) Content))) (time ?X3) (truth 1))) => (reduce-class (send ?Y59 get-what) Content) (add-prop ?X1 (add-pred IsOwner of (send ?Y59 get-what)) (make-instance of Duration (start ?X3) (end -1.0)) 1) (add-prop (send ?Y59 get-what) (add-pred Has what [private]) (make-instance of Duration (start ?X3) (end -1.0)) 1))
(defrule 5aaf460948e941b0be74c9ec9beca255 (logical (object (is-a Proposition) (subject ?X1&:(or (eq (class ?X1) Content) (subclassp (class ?X1) Content))) (predicate ?Y60&:(or (eq (class ?Y60) Has) (subclassp (class ?Y60) Has))&:(eq (send ?Y60 get-what) [private])) (time ?X2&:(or (eq (class ?X2) Duration) (subclassp (class ?X2) Duration))) (truth 1))) => (add-prop [manage_perm] (add-pred IsNeeded for_action (add-pred View what ?X1)) ?X2 1))
(defrule 5bd5bad3c0ea4560a7010a7737fe662f (logical (object (is-a Proposition) (subject ?X1&:(or (eq (class ?X1) Person) (subclassp (class ?X1) Person))) (predicate ?Y62&:(or (eq (class ?Y62) Hide) (subclassp (class ?Y62) Hide))&:(or (eq (class (send ?Y62 get-what)) Content) (subclassp (class (send ?Y62 get-what)) Content))) (time ?X3) (truth 1))) (logical (object (is-a Proposition) (subject ?X2&:(eq ?X2 (send ?Y62 get-what))) (predicate ?Y63&:(or (eq (class ?Y63) Has) (subclassp (class ?Y63) Has))&:(or (eq (class (send ?Y63 get-what)) Status) (subclassp (class (send ?Y63 get-what)) Status))) (time ?X5&:(or (eq (class ?X5) Duration) (subclassp (class ?X5) Duration))) (truth 1))) => (send ?X5 put-end 733697) (add-prop (send ?Y62 get-what) (add-pred Has what [private]) (make-instance of Duration (start ?X3) (end -1.0)) 1))
(defrule 5e390f7b6ab641a49ec105e68e9723b6 (logical (object (is-a Person) (name ?X1))) => (add-prop ?X1 (add-pred Has what [member]) (make-instance of Duration (start 733697.0) (end -1.0)) 1))
(defrule 82329082f7494ddb93e3c082117773e7 (logical (object (is-a Permission) (name ?X2))) => (add-prop [manager] (add-pred Has what ?X2) (make-instance of Duration (start 733697.0) (end -1.0)) 1))
(defrule 92bd1f2cd14b4242a0531a65745fe2dd (logical (object (is-a Proposition) (subject ?X1&:(or (eq (class ?X1) Person) (subclassp (class ?X1) Person))) (predicate ?Y65&:(or (eq (class ?Y65) IsIn) (subclassp (class ?Y65) IsIn))) (time ?X3&:(or (eq (class ?X3) Duration) (subclassp (class ?X3) Duration))) (truth 1))) (logical (object (is-a Proposition) (subject ?X2&:(or (eq (class ?X2) Group) (subclassp (class ?X2) Group))) (predicate ?Y67&:(or (eq (class ?Y67) Has) (subclassp (class ?Y67) Has))&:(or (eq (class (send ?Y67 get-what)) Permission) (subclassp (class (send ?Y67 get-what)) Permission))) (time ?X5&:(or (eq (class ?X5) Duration) (subclassp (class ?X5) Duration))) (truth 1))) (test (or (and (>= (send ?X3 get-start) (send ?X3 get-start)) (or (<= (send ?X3 get-start) (send ?X3 get-end)) (= (send ?X3 get-end) -1))) (and (>= (send ?X3 get-start) (send ?X3 get-start)) (or (<= (send ?X3 get-start) (send ?X3 get-end)) (= (send ?X3 get-end) -1))))) => (add-prop ?X1 (add-pred Has what (send ?Y67 get-what)) (make-instance of Duration (start (mincomstart ?X3 ?X5)) (end (maxcomend ?X3 ?X5))) 1))
(defrule d4d815ae33bd49b7acafbdf953294c28 (logical (object (is-a Content) (name ?X1))) => (add-prop [manage_perm] (add-pred IsNeeded for_action (add-pred Publish what ?X1)) (make-instance of Duration (start 733697.0) (end -1.0)) 1))
(defrule f0771a52098f4dc6b4ac2dab262c9877 (logical (object (is-a Proposition) (subject ?X1&:(or (eq (class ?X1) Person) (subclassp (class ?X1) Person))) (predicate ?Y69&:(or (eq (class ?Y69) IsOwner) (subclassp (class ?Y69) IsOwner))&:(or (eq (class (send ?Y69 get-of)) Content) (subclassp (class (send ?Y69 get-of)) Content))) (time ?X3&:(or (eq (class ?X3) Duration) (subclassp (class ?X3) Duration))) (truth 1))) => (add-prop ?X1 (add-pred Can what (add-pred Hide what (send ?Y69 get-of))) ?X3 1))
(defrule f1a8cfe912f044b386ba818b60208dc0 (logical (object (is-a Proposition) (subject ?X1&:(or (eq (class ?X1) Person) (subclassp (class ?X1) Person))) (predicate ?Y71&:(or (eq (class ?Y71) Has) (subclassp (class ?Y71) Has))&:(or (eq (class (send ?Y71 get-what)) Role) (subclassp (class (send ?Y71 get-what)) Role))) (time ?X3&:(or (eq (class ?X3) Duration) (subclassp (class ?X3) Duration))) (truth 1))) (logical (object (is-a Proposition) (subject ?X2&:(eq ?X2 (send ?Y71 get-what))) (predicate ?Y73&:(or (eq (class ?Y73) Has) (subclassp (class ?Y73) Has))&:(or (eq (class (send ?Y73 get-what)) Permission) (subclassp (class (send ?Y73 get-what)) Permission))) (time ?X5&:(or (eq (class ?X5) Duration) (subclassp (class ?X5) Duration))) (truth 1))) (test (or (and (>= (send ?X3 get-start) (send ?X3 get-start)) (or (<= (send ?X3 get-start) (send ?X3 get-end)) (= (send ?X3 get-end) -1))) (and (>= (send ?X3 get-start) (send ?X3 get-start)) (or (<= (send ?X3 get-start) (send ?X3 get-end)) (= (send ?X3 get-end) -1))))) => (add-prop ?X1 (add-pred Has what (send ?Y73 get-what)) (make-instance of Duration (start (mincomstart ?X3 ?X5)) (end (maxcomend ?X3 ?X5))) 1))
(defrule f30a0513b1e7497d9b1959b9dafbb785 (logical (object (is-a Proposition) (subject ?X1&:(or (eq (class ?X1) Thing) (subclassp (class ?X1) Thing))) (predicate ?Y75&:(or (eq (class ?Y75) IsIn) (subclassp (class ?Y75) IsIn))&:(or (eq (class (send ?Y75 get-what)) Thing) (subclassp (class (send ?Y75 get-what)) Thing))) (time ?X4&:(or (eq (class ?X4) Duration) (subclassp (class ?X4) Duration))) (truth 1))) (logical (object (is-a Proposition) (subject ?X2&:(eq ?X2 (send ?Y75 get-what))) (predicate ?Y77&:(or (eq (class ?Y77) IsIn) (subclassp (class ?Y77) IsIn))&:(or (eq (class (send ?Y77 get-what)) Thing) (subclassp (class (send ?Y77 get-what)) Thing))) (time ?X5&:(or (eq (class ?X5) Duration) (subclassp (class ?X5) Duration))) (truth 1))) (test (or (and (>= (send ?X4 get-start) (send ?X4 get-start)) (or (<= (send ?X4 get-start) (send ?X4 get-end)) (= (send ?X4 get-end) -1))) (and (>= (send ?X4 get-start) (send ?X4 get-start)) (or (<= (send ?X4 get-start) (send ?X4 get-end)) (= (send ?X4 get-end) -1))))) => (add-prop ?X1 (add-pred IsIn what (send ?Y77 get-what)) (make-instance of Duration (start (mincomstart ?X4 ?X5)) (end (maxcomend ?X4 ?X5))) 1))
(add-prop [admin] (add-pred Has what [manager]) (make-instance of Duration (start 733697.0) (end -1.0)) 1)
admin has what manager at from 733697.0 till -1.0
(defclass Name (is-a USER))
(deffunction reduce-class (?instance ?class) (if (eq (length$ (find-instance ((?a ?class)) (eq (instance-name ?a) ?instance))) 0) then (make-instance ?instance of ?class) (python-call tonl ?class ?instance)))
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


(deffunction mincomstart (?dur1 ?dur2)
    (return (max (send ?dur1 get-start) (send ?dur2 get-start)))
)



(deffunction maxcomend (?dur1 ?dur2)
    (bind ?e1 (send ?dur1 get-end))
    (bind ?e2 (send ?dur2 get-end))
    (if (= ?e1 ?e2) then (return ?e1))
    (if (= ?e2 -1) then
        (return ?e1)
    )
    (if (= ?e1 -1) then
        (return ?e2)
    )
    (return (min ?e1 ?e2))
)

(defclass Proposition (is-a Name) (slot truth (type INTEGER) (default 1) (pattern-match reactive)) (slot subject (type INSTANCE) (pattern-match reactive)) (slot predicate (type INSTANCE) (pattern-match reactive)) (slot time (type ?VARIABLE) (pattern-match reactive)))

(deffunction add-prop (?s ?p ?t ?r)
       (if (and (python-call ptonl ?s ?p ?t ?r)
                (= (+ (length$ (find-instance ((?prop Proposition) (?dur Duration))
                          (and (eq ?prop:subject ?s)
                               (eq ?prop:predicate ?p)
                               (eq ?prop:time ?dur)
                               (= ?dur:start (send ?t get-start))
                               (= ?dur:end (send ?t get-end))
                               (eq ?prop:truth ?r))))
                      (length$ (find-instance ((?prop Proposition))
                          (and (eq ?prop:subject ?s)
                               (eq ?prop:predicate ?p)
                               (= ?prop:time ?t)
                               (eq ?prop:truth ?r)))))
                 0))
        then (make-instance of Proposition (subject ?s)
                                           (predicate ?p)
                                           (time ?t)
                                           (truth ?r))
        else (return TRUE)))
------------OPEN---------------------
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
admin
(reduce-class [member] Role)
member
(reduce-class [manager] Role)
manager
(reduce-class [basic_perm] Permission)
basic_perm
(reduce-class [manage_perm] Permission)
manage_perm
(reduce-class [create_perm] Permission)
create_perm
(reduce-class [public] Status)
public
(reduce-class [private] Status)
private
(add-prop [admin] (add-pred Has what [manager]) (make-instance of Duration (start 733697.0) (end -1.0)) 1)
admin has what manager at from 733697.0 till -1.0
(add-prop [member] (add-pred Has what [basic_perm]) (make-instance of Duration (start 733697.0) (end -1.0)) 1)
member has what basic_perm at from 733697.0 till -1.0
(defrule b741d07e127f4b71a2b12a6f797a0e1f (logical (object (is-a Proposition) (subject ?X1&:(or (eq (class ?X1) Person) (subclassp (class ?X1) Person))) (predicate ?Y1&:(or (eq (class ?Y1) Wants) (subclassp (class ?Y1) Wants))&:(or (eq (class (send ?Y1 get-to)) Create) (subclassp (class (send ?Y1 get-to)) Create))&:(or (eq (class (send (send ?Y1 get-to) get-what)) Thing) (subclassp (class (send (send ?Y1 get-to) get-what)) Thing))) (time ?X2) (truth 1))) (logical (object (is-a Proposition) (subject ?X1) (predicate ?Y2&:(or (eq (class ?Y2) Has) (subclassp (class ?Y2) Has))&:(eq (send ?Y2 get-what) [create_perm])) (time ?X3&:(or (eq (class ?X3) Duration) (subclassp (class ?X3) Duration))) (truth 1))) (test (and (<= (send ?X3 get-start) ?X2) (or (= (send ?X3 get-end) -1) (>= (send ?X3 get-end) ?X2)))) => (add-prop ?X1 (add-pred Create what (send (send ?Y1 get-to) get-what)) ?X2 1))
(defrule 357f882a2d92451b84dd4eacafb217bd (logical (object (is-a Proposition) (subject ?X1&:(or (eq (class ?X1) Person) (subclassp (class ?X1) Person))) (predicate ?Y4&:(or (eq (class ?Y4) Wants) (subclassp (class ?Y4) Wants))) (time ?X2) (truth 1))) (logical (object (is-a Proposition) (subject ?X1) (predicate ?Y5&:(or (eq (class ?Y5) Can) (subclassp (class ?Y5) Can))&:(eq (send ?Y4 get-to) (send ?Y5 get-what))) (time ?X3&:(or (eq (class ?X3) Duration) (subclassp (class ?X3) Duration))) (truth 1))) (test (and (<= (send ?X3 get-start) ?X2) (or (= (send ?X3 get-end) -1) (>= (send ?X3 get-end) ?X2)))) => (add-prop ?X1 (send ?Y4 get-to) ?X2 1))
(defrule 392fb732f46a4b2ba072f7bd1f67c777 (logical (object (is-a Proposition) (subject ?X2&:(or (eq (class ?X2) Thing) (subclassp (class ?X2) Thing))) (predicate ?Y7&:(or (eq (class ?Y7) IsNeeded) (subclassp (class ?Y7) IsNeeded))) (time ?X3&:(or (eq (class ?X3) Duration) (subclassp (class ?X3) Duration))) (truth 1))) (logical (object (is-a Proposition) (subject ?X1&:(or (eq (class ?X1) Thing) (subclassp (class ?X1) Thing))) (predicate ?Y9&:(or (eq (class ?Y9) Has) (subclassp (class ?Y9) Has))&:(eq (send ?Y9 get-what) ?X2)) (time ?X5&:(or (eq (class ?X5) Duration) (subclassp (class ?X5) Duration))) (truth 1))) (test (or (and (>= (send ?X3 get-start) (send ?X3 get-start)) (or (<= (send ?X3 get-start) (send ?X3 get-end)) (= (send ?X3 get-end) -1))) (and (>= (send ?X3 get-start) (send ?X3 get-start)) (or (<= (send ?X3 get-start) (send ?X3 get-end)) (= (send ?X3 get-end) -1))))) => (add-prop ?X1 (add-pred Can what (send ?Y7 get-for_action)) (make-instance of Duration (start (mincomstart ?X3 ?X5)) (end (maxcomend ?X3 ?X5))) 1))
(defrule dfd7da19575a4b7fac832a4f2a21fcfd (logical (object (is-a Proposition) (subject ?X1&:(or (eq (class ?X1) Thing) (subclassp (class ?X1) Thing))) (predicate ?Y11&:(or (eq (class ?Y11) IsIn) (subclassp (class ?Y11) IsIn))&:(or (eq (class (send ?Y11 get-what)) Thing) (subclassp (class (send ?Y11 get-what)) Thing))) (time ?X4&:(or (eq (class ?X4) Duration) (subclassp (class ?X4) Duration))) (truth 1))) (logical (object (is-a Proposition) (subject ?X2&:(eq ?X2 (send ?Y11 get-what))) (predicate ?Y13&:(or (eq (class ?Y13) IsIn) (subclassp (class ?Y13) IsIn))&:(or (eq (class (send ?Y13 get-what)) Thing) (subclassp (class (send ?Y13 get-what)) Thing))) (time ?X5&:(or (eq (class ?X5) Duration) (subclassp (class ?X5) Duration))) (truth 1))) (test (or (and (>= (send ?X4 get-start) (send ?X4 get-start)) (or (<= (send ?X4 get-start) (send ?X4 get-end)) (= (send ?X4 get-end) -1))) (and (>= (send ?X4 get-start) (send ?X4 get-start)) (or (<= (send ?X4 get-start) (send ?X4 get-end)) (= (send ?X4 get-end) -1))))) => (add-prop ?X1 (add-pred IsIn what (send ?Y13 get-what)) (make-instance of Duration (start (mincomstart ?X4 ?X5)) (end (maxcomend ?X4 ?X5))) 1))
(defrule d2bb33e4ca2242c7a6ea4f723ad7018a (logical (object (is-a Proposition) (subject ?X1&:(or (eq (class ?X1) Person) (subclassp (class ?X1) Person))) (predicate ?Y15&:(or (eq (class ?Y15) IsIn) (subclassp (class ?Y15) IsIn))) (time ?X3&:(or (eq (class ?X3) Duration) (subclassp (class ?X3) Duration))) (truth 1))) (logical (object (is-a Proposition) (subject ?X2&:(or (eq (class ?X2) Group) (subclassp (class ?X2) Group))) (predicate ?Y17&:(or (eq (class ?Y17) Has) (subclassp (class ?Y17) Has))&:(or (eq (class (send ?Y17 get-what)) Permission) (subclassp (class (send ?Y17 get-what)) Permission))) (time ?X5&:(or (eq (class ?X5) Duration) (subclassp (class ?X5) Duration))) (truth 1))) (test (or (and (>= (send ?X3 get-start) (send ?X3 get-start)) (or (<= (send ?X3 get-start) (send ?X3 get-end)) (= (send ?X3 get-end) -1))) (and (>= (send ?X3 get-start) (send ?X3 get-start)) (or (<= (send ?X3 get-start) (send ?X3 get-end)) (= (send ?X3 get-end) -1))))) => (add-prop ?X1 (add-pred Has what (send ?Y17 get-what)) (make-instance of Duration (start (mincomstart ?X3 ?X5)) (end (maxcomend ?X3 ?X5))) 1))
(defrule 85a70d9d763b4b7895b30a644c3d9ad8 (logical (object (is-a Proposition) (subject ?X1&:(or (eq (class ?X1) Person) (subclassp (class ?X1) Person))) (predicate ?Y19&:(or (eq (class ?Y19) Has) (subclassp (class ?Y19) Has))&:(or (eq (class (send ?Y19 get-what)) Role) (subclassp (class (send ?Y19 get-what)) Role))) (time ?X3&:(or (eq (class ?X3) Duration) (subclassp (class ?X3) Duration))) (truth 1))) (logical (object (is-a Proposition) (subject ?X2&:(eq ?X2 (send ?Y19 get-what))) (predicate ?Y21&:(or (eq (class ?Y21) Has) (subclassp (class ?Y21) Has))&:(or (eq (class (send ?Y21 get-what)) Permission) (subclassp (class (send ?Y21 get-what)) Permission))) (time ?X5&:(or (eq (class ?X5) Duration) (subclassp (class ?X5) Duration))) (truth 1))) (test (or (and (>= (send ?X3 get-start) (send ?X3 get-start)) (or (<= (send ?X3 get-start) (send ?X3 get-end)) (= (send ?X3 get-end) -1))) (and (>= (send ?X3 get-start) (send ?X3 get-start)) (or (<= (send ?X3 get-start) (send ?X3 get-end)) (= (send ?X3 get-end) -1))))) => (add-prop ?X1 (add-pred Has what (send ?Y21 get-what)) (make-instance of Duration (start (mincomstart ?X3 ?X5)) (end (maxcomend ?X3 ?X5))) 1))
(defrule 43ff755350cb4719b016bf8cd1027eb9 (logical (object (is-a Person) (name ?X1))) => (add-prop ?X1 (add-pred Has what [member]) (make-instance of Duration (start 733697.0) (end -1.0)) 1))
(defrule eacd2d4096d64677b09493e41eab4eea (logical (object (is-a Proposition) (subject ?X1&:(or (eq (class ?X1) Person) (subclassp (class ?X1) Person))) (predicate ?Y23&:(or (eq (class ?Y23) Create) (subclassp (class ?Y23) Create))&:(or (eq (class (send ?Y23 get-what)) Content) (subclassp (class (send ?Y23 get-what)) Content))) (time ?X3) (truth 1))) => (reduce-class (send ?Y23 get-what) Content) (add-prop ?X1 (add-pred IsOwner of (send ?Y23 get-what)) (make-instance of Duration (start ?X3) (end -1.0)) 1) (add-prop (send ?Y23 get-what) (add-pred Has what [private]) (make-instance of Duration (start ?X3) (end -1.0)) 1))
(defrule deef1978c7de43e69d49eb1c3ac7d885 (logical (object (is-a Permission) (name ?X2))) => (add-prop [manager] (add-pred Has what ?X2) (make-instance of Duration (start 733697.0) (end -1.0)) 1))
(defrule ddfdcfe7c2a646db850a95067d2f575b (logical (object (is-a Proposition) (subject ?X1&:(or (eq (class ?X1) Content) (subclassp (class ?X1) Content))) (predicate ?Y24&:(or (eq (class ?Y24) Has) (subclassp (class ?Y24) Has))&:(eq (send ?Y24 get-what) [public])) (time ?X2&:(or (eq (class ?X2) Duration) (subclassp (class ?X2) Duration))) (truth 1))) => (add-prop [basic_perm] (add-pred IsNeeded for_action (add-pred View what ?X1)) ?X2 1))
(defrule c2659c60f4bd4dafa172b38d0f1b4f54 (logical (object (is-a Proposition) (subject ?X1&:(or (eq (class ?X1) Content) (subclassp (class ?X1) Content))) (predicate ?Y26&:(or (eq (class ?Y26) Has) (subclassp (class ?Y26) Has))&:(eq (send ?Y26 get-what) [private])) (time ?X2&:(or (eq (class ?X2) Duration) (subclassp (class ?X2) Duration))) (truth 1))) => (add-prop [manage_perm] (add-pred IsNeeded for_action (add-pred View what ?X1)) ?X2 1))
(defrule 2902b90d8b104fc49b307236e9226027 (logical (object (is-a Proposition) (subject ?X1&:(or (eq (class ?X1) Content) (subclassp (class ?X1) Content))) (predicate ?Y28&:(or (eq (class ?Y28) Has) (subclassp (class ?Y28) Has))&:(eq (send ?Y28 get-what) [private])) (time ?X3&:(or (eq (class ?X3) Duration) (subclassp (class ?X3) Duration))) (truth 1))) (logical (object (is-a Proposition) (subject ?X2&:(or (eq (class ?X2) Person) (subclassp (class ?X2) Person))) (predicate ?Y30&:(or (eq (class ?Y30) IsOwner) (subclassp (class ?Y30) IsOwner))&:(eq (send ?Y30 get-of) ?X1)) (time ?X4&:(or (eq (class ?X4) Duration) (subclassp (class ?X4) Duration))) (truth 1))) (test (or (and (>= (send ?X3 get-start) (send ?X3 get-start)) (or (<= (send ?X3 get-start) (send ?X3 get-end)) (= (send ?X3 get-end) -1))) (and (>= (send ?X3 get-start) (send ?X3 get-start)) (or (<= (send ?X3 get-start) (send ?X3 get-end)) (= (send ?X3 get-end) -1))))) => (add-prop ?X2 (add-pred Can what (add-pred View what ?X1)) (make-instance of Duration (start (mincomstart ?X3 ?X4)) (end (maxcomend ?X3 ?X4))) 1))
(defrule ca77bccaa164400c9529106e736f3327 (logical (object (is-a Proposition) (subject ?X1&:(or (eq (class ?X1) Person) (subclassp (class ?X1) Person))) (predicate ?Y32&:(or (eq (class ?Y32) Publish) (subclassp (class ?Y32) Publish))&:(or (eq (class (send ?Y32 get-what)) Content) (subclassp (class (send ?Y32 get-what)) Content))) (time ?X3) (truth 1))) (logical (object (is-a Proposition) (subject ?X2&:(eq ?X2 (send ?Y32 get-what))) (predicate ?Y33&:(or (eq (class ?Y33) Has) (subclassp (class ?Y33) Has))&:(or (eq (class (send ?Y33 get-what)) Status) (subclassp (class (send ?Y33 get-what)) Status))) (time ?X5&:(or (eq (class ?X5) Duration) (subclassp (class ?X5) Duration))) (truth 1))) => (send ?X5 put-end 733697) (add-prop (send ?Y32 get-what) (add-pred Has what [public]) (make-instance of Duration (start ?X3) (end -1.0)) 1))
(defrule db686b03af6c4e0a80c866925be65a5a (logical (object (is-a Content) (name ?X1))) => (add-prop [manage_perm] (add-pred IsNeeded for_action (add-pred Publish what ?X1)) (make-instance of Duration (start 733697.0) (end -1.0)) 1))
(defrule 5254f446d0214474a9e4cafcac1417c8 (logical (object (is-a Proposition) (subject ?X1&:(or (eq (class ?X1) Person) (subclassp (class ?X1) Person))) (predicate ?Y35&:(or (eq (class ?Y35) Hide) (subclassp (class ?Y35) Hide))&:(or (eq (class (send ?Y35 get-what)) Content) (subclassp (class (send ?Y35 get-what)) Content))) (time ?X3) (truth 1))) (logical (object (is-a Proposition) (subject ?X2&:(eq ?X2 (send ?Y35 get-what))) (predicate ?Y36&:(or (eq (class ?Y36) Has) (subclassp (class ?Y36) Has))&:(or (eq (class (send ?Y36 get-what)) Status) (subclassp (class (send ?Y36 get-what)) Status))) (time ?X5&:(or (eq (class ?X5) Duration) (subclassp (class ?X5) Duration))) (truth 1))) => (send ?X5 put-end 733697) (add-prop (send ?Y35 get-what) (add-pred Has what [private]) (make-instance of Duration (start ?X3) (end -1.0)) 1))
(defrule 224dbfb20d1146dbbf7f9bd8ddfaa5a0 (logical (object (is-a Proposition) (subject ?X1&:(or (eq (class ?X1) Person) (subclassp (class ?X1) Person))) (predicate ?Y38&:(or (eq (class ?Y38) IsOwner) (subclassp (class ?Y38) IsOwner))&:(or (eq (class (send ?Y38 get-of)) Content) (subclassp (class (send ?Y38 get-of)) Content))) (time ?X3&:(or (eq (class ?X3) Duration) (subclassp (class ?X3) Duration))) (truth 1))) => (add-prop ?X1 (add-pred Can what (add-pred Hide what (send ?Y38 get-of))) ?X3 1))
(reduce-class [admin] Person)
(reduce-class [basic_perm] Permission)
(reduce-class [create_perm] Permission)
(reduce-class [manage_perm] Permission)
(reduce-class [manager] Role)
(reduce-class [member] Role)
(reduce-class [private] Status)
(reduce-class [public] Status)
(defrule 038df7889ec740ae9b5227517a092c3e (logical (object (is-a Proposition) (subject ?X1&:(or (eq (class ?X1) Content) (subclassp (class ?X1) Content))) (predicate ?Y40&:(or (eq (class ?Y40) Has) (subclassp (class ?Y40) Has))&:(eq (send ?Y40 get-what) [public])) (time ?X2&:(or (eq (class ?X2) Duration) (subclassp (class ?X2) Duration))) (truth 1))) => (add-prop [basic_perm] (add-pred IsNeeded for_action (add-pred View what ?X1)) ?X2 1))
(defrule 05c6ced2e45c434b8d7ab3c5a903f0be (logical (object (is-a Proposition) (subject ?X2&:(or (eq (class ?X2) Thing) (subclassp (class ?X2) Thing))) (predicate ?Y42&:(or (eq (class ?Y42) IsNeeded) (subclassp (class ?Y42) IsNeeded))) (time ?X3&:(or (eq (class ?X3) Duration) (subclassp (class ?X3) Duration))) (truth 1))) (logical (object (is-a Proposition) (subject ?X1&:(or (eq (class ?X1) Thing) (subclassp (class ?X1) Thing))) (predicate ?Y44&:(or (eq (class ?Y44) Has) (subclassp (class ?Y44) Has))&:(eq (send ?Y44 get-what) ?X2)) (time ?X5&:(or (eq (class ?X5) Duration) (subclassp (class ?X5) Duration))) (truth 1))) (test (or (and (>= (send ?X3 get-start) (send ?X3 get-start)) (or (<= (send ?X3 get-start) (send ?X3 get-end)) (= (send ?X3 get-end) -1))) (and (>= (send ?X3 get-start) (send ?X3 get-start)) (or (<= (send ?X3 get-start) (send ?X3 get-end)) (= (send ?X3 get-end) -1))))) => (add-prop ?X1 (add-pred Can what (send ?Y42 get-for_action)) (make-instance of Duration (start (mincomstart ?X3 ?X5)) (end (maxcomend ?X3 ?X5))) 1))
(defrule 189c8bde18f94f7b9ed08fb21f785ff3 (logical (object (is-a Proposition) (subject ?X1&:(or (eq (class ?X1) Person) (subclassp (class ?X1) Person))) (predicate ?Y46&:(or (eq (class ?Y46) Wants) (subclassp (class ?Y46) Wants))&:(or (eq (class (send ?Y46 get-to)) Create) (subclassp (class (send ?Y46 get-to)) Create))&:(or (eq (class (send (send ?Y46 get-to) get-what)) Thing) (subclassp (class (send (send ?Y46 get-to) get-what)) Thing))) (time ?X2) (truth 1))) (logical (object (is-a Proposition) (subject ?X1) (predicate ?Y47&:(or (eq (class ?Y47) Has) (subclassp (class ?Y47) Has))&:(eq (send ?Y47 get-what) [create_perm])) (time ?X3&:(or (eq (class ?X3) Duration) (subclassp (class ?X3) Duration))) (truth 1))) (test (and (<= (send ?X3 get-start) ?X2) (or (= (send ?X3 get-end) -1) (>= (send ?X3 get-end) ?X2)))) => (add-prop ?X1 (add-pred Create what (send (send ?Y46 get-to) get-what)) ?X2 1))
(defrule 224dbfb20d1146dbbf7f9bd8ddfaa5a0 (logical (object (is-a Proposition) (subject ?X1&:(or (eq (class ?X1) Person) (subclassp (class ?X1) Person))) (predicate ?Y49&:(or (eq (class ?Y49) IsOwner) (subclassp (class ?Y49) IsOwner))&:(or (eq (class (send ?Y49 get-of)) Content) (subclassp (class (send ?Y49 get-of)) Content))) (time ?X3&:(or (eq (class ?X3) Duration) (subclassp (class ?X3) Duration))) (truth 1))) => (add-prop ?X1 (add-pred Can what (add-pred Hide what (send ?Y49 get-of))) ?X3 1))
(defrule 2902b90d8b104fc49b307236e9226027 (logical (object (is-a Proposition) (subject ?X1&:(or (eq (class ?X1) Content) (subclassp (class ?X1) Content))) (predicate ?Y51&:(or (eq (class ?Y51) Has) (subclassp (class ?Y51) Has))&:(eq (send ?Y51 get-what) [private])) (time ?X3&:(or (eq (class ?X3) Duration) (subclassp (class ?X3) Duration))) (truth 1))) (logical (object (is-a Proposition) (subject ?X2&:(or (eq (class ?X2) Person) (subclassp (class ?X2) Person))) (predicate ?Y53&:(or (eq (class ?Y53) IsOwner) (subclassp (class ?Y53) IsOwner))&:(eq (send ?Y53 get-of) ?X1)) (time ?X4&:(or (eq (class ?X4) Duration) (subclassp (class ?X4) Duration))) (truth 1))) (test (or (and (>= (send ?X3 get-start) (send ?X3 get-start)) (or (<= (send ?X3 get-start) (send ?X3 get-end)) (= (send ?X3 get-end) -1))) (and (>= (send ?X3 get-start) (send ?X3 get-start)) (or (<= (send ?X3 get-start) (send ?X3 get-end)) (= (send ?X3 get-end) -1))))) => (add-prop ?X2 (add-pred Can what (add-pred View what ?X1)) (make-instance of Duration (start (mincomstart ?X3 ?X4)) (end (maxcomend ?X3 ?X4))) 1))
(defrule 33512ab543c845298806a482225c3579 (logical (object (is-a Proposition) (subject ?X1&:(or (eq (class ?X1) Content) (subclassp (class ?X1) Content))) (predicate ?Y55&:(or (eq (class ?Y55) Has) (subclassp (class ?Y55) Has))&:(eq (send ?Y55 get-what) [private])) (time ?X3&:(or (eq (class ?X3) Duration) (subclassp (class ?X3) Duration))) (truth 1))) (logical (object (is-a Proposition) (subject ?X2&:(or (eq (class ?X2) Person) (subclassp (class ?X2) Person))) (predicate ?Y57&:(or (eq (class ?Y57) IsOwner) (subclassp (class ?Y57) IsOwner))&:(eq (send ?Y57 get-of) ?X1)) (time ?X4&:(or (eq (class ?X4) Duration) (subclassp (class ?X4) Duration))) (truth 1))) (test (or (and (>= (send ?X3 get-start) (send ?X3 get-start)) (or (<= (send ?X3 get-start) (send ?X3 get-end)) (= (send ?X3 get-end) -1))) (and (>= (send ?X3 get-start) (send ?X3 get-start)) (or (<= (send ?X3 get-start) (send ?X3 get-end)) (= (send ?X3 get-end) -1))))) => (add-prop ?X2 (add-pred Can what (add-pred View what ?X1)) (make-instance of Duration (start (mincomstart ?X3 ?X4)) (end (maxcomend ?X3 ?X4))) 1))
(defrule 357f882a2d92451b84dd4eacafb217bd (logical (object (is-a Proposition) (subject ?X1&:(or (eq (class ?X1) Person) (subclassp (class ?X1) Person))) (predicate ?Y59&:(or (eq (class ?Y59) Wants) (subclassp (class ?Y59) Wants))) (time ?X2) (truth 1))) (logical (object (is-a Proposition) (subject ?X1) (predicate ?Y60&:(or (eq (class ?Y60) Can) (subclassp (class ?Y60) Can))&:(eq (send ?Y59 get-to) (send ?Y60 get-what))) (time ?X3&:(or (eq (class ?X3) Duration) (subclassp (class ?X3) Duration))) (truth 1))) (test (and (<= (send ?X3 get-start) ?X2) (or (= (send ?X3 get-end) -1) (>= (send ?X3 get-end) ?X2)))) => (add-prop ?X1 (send ?Y59 get-to) ?X2 1))
(defrule 392fb732f46a4b2ba072f7bd1f67c777 (logical (object (is-a Proposition) (subject ?X2&:(or (eq (class ?X2) Thing) (subclassp (class ?X2) Thing))) (predicate ?Y62&:(or (eq (class ?Y62) IsNeeded) (subclassp (class ?Y62) IsNeeded))) (time ?X3&:(or (eq (class ?X3) Duration) (subclassp (class ?X3) Duration))) (truth 1))) (logical (object (is-a Proposition) (subject ?X1&:(or (eq (class ?X1) Thing) (subclassp (class ?X1) Thing))) (predicate ?Y64&:(or (eq (class ?Y64) Has) (subclassp (class ?Y64) Has))&:(eq (send ?Y64 get-what) ?X2)) (time ?X5&:(or (eq (class ?X5) Duration) (subclassp (class ?X5) Duration))) (truth 1))) (test (or (and (>= (send ?X3 get-start) (send ?X3 get-start)) (or (<= (send ?X3 get-start) (send ?X3 get-end)) (= (send ?X3 get-end) -1))) (and (>= (send ?X3 get-start) (send ?X3 get-start)) (or (<= (send ?X3 get-start) (send ?X3 get-end)) (= (send ?X3 get-end) -1))))) => (add-prop ?X1 (add-pred Can what (send ?Y62 get-for_action)) (make-instance of Duration (start (mincomstart ?X3 ?X5)) (end (maxcomend ?X3 ?X5))) 1))
(defrule 42bf643ccbbc4775a7b3228c5f024e1d (logical (object (is-a Proposition) (subject ?X1&:(or (eq (class ?X1) Person) (subclassp (class ?X1) Person))) (predicate ?Y66&:(or (eq (class ?Y66) Wants) (subclassp (class ?Y66) Wants))) (time ?X2) (truth 1))) (logical (object (is-a Proposition) (subject ?X1) (predicate ?Y67&:(or (eq (class ?Y67) Can) (subclassp (class ?Y67) Can))&:(eq (send ?Y66 get-to) (send ?Y67 get-what))) (time ?X3&:(or (eq (class ?X3) Duration) (subclassp (class ?X3) Duration))) (truth 1))) (test (and (<= (send ?X3 get-start) ?X2) (or (= (send ?X3 get-end) -1) (>= (send ?X3 get-end) ?X2)))) => (add-prop ?X1 (send ?Y66 get-to) ?X2 1))
(defrule 43ff755350cb4719b016bf8cd1027eb9 (logical (object (is-a Person) (name ?X1))) => (add-prop ?X1 (add-pred Has what [member]) (make-instance of Duration (start 733697.0) (end -1.0)) 1))
(defrule 5078fc97c0ab405ba92665304e167c33 (logical (object (is-a Proposition) (subject ?X1&:(or (eq (class ?X1) Person) (subclassp (class ?X1) Person))) (predicate ?Y69&:(or (eq (class ?Y69) Publish) (subclassp (class ?Y69) Publish))&:(or (eq (class (send ?Y69 get-what)) Content) (subclassp (class (send ?Y69 get-what)) Content))) (time ?X3) (truth 1))) (logical (object (is-a Proposition) (subject ?X2&:(eq ?X2 (send ?Y69 get-what))) (predicate ?Y70&:(or (eq (class ?Y70) Has) (subclassp (class ?Y70) Has))&:(or (eq (class (send ?Y70 get-what)) Status) (subclassp (class (send ?Y70 get-what)) Status))) (time ?X5&:(or (eq (class ?X5) Duration) (subclassp (class ?X5) Duration))) (truth 1))) => (send ?X5 put-end 733697) (add-prop (send ?Y69 get-what) (add-pred Has what [public]) (make-instance of Duration (start ?X3) (end -1.0)) 1))
(defrule 5254f446d0214474a9e4cafcac1417c8 (logical (object (is-a Proposition) (subject ?X1&:(or (eq (class ?X1) Person) (subclassp (class ?X1) Person))) (predicate ?Y72&:(or (eq (class ?Y72) Hide) (subclassp (class ?Y72) Hide))&:(or (eq (class (send ?Y72 get-what)) Content) (subclassp (class (send ?Y72 get-what)) Content))) (time ?X3) (truth 1))) (logical (object (is-a Proposition) (subject ?X2&:(eq ?X2 (send ?Y72 get-what))) (predicate ?Y73&:(or (eq (class ?Y73) Has) (subclassp (class ?Y73) Has))&:(or (eq (class (send ?Y73 get-what)) Status) (subclassp (class (send ?Y73 get-what)) Status))) (time ?X5&:(or (eq (class ?X5) Duration) (subclassp (class ?X5) Duration))) (truth 1))) => (send ?X5 put-end 733697) (add-prop (send ?Y72 get-what) (add-pred Has what [private]) (make-instance of Duration (start ?X3) (end -1.0)) 1))
(defrule 59c093a433674c19bf42cb820569cdaf (logical (object (is-a Proposition) (subject ?X1&:(or (eq (class ?X1) Person) (subclassp (class ?X1) Person))) (predicate ?Y75&:(or (eq (class ?Y75) Create) (subclassp (class ?Y75) Create))&:(or (eq (class (send ?Y75 get-what)) Content) (subclassp (class (send ?Y75 get-what)) Content))) (time ?X3) (truth 1))) => (reduce-class (send ?Y75 get-what) Content) (add-prop ?X1 (add-pred IsOwner of (send ?Y75 get-what)) (make-instance of Duration (start ?X3) (end -1.0)) 1) (add-prop (send ?Y75 get-what) (add-pred Has what [private]) (make-instance of Duration (start ?X3) (end -1.0)) 1))
(defrule 5aaf460948e941b0be74c9ec9beca255 (logical (object (is-a Proposition) (subject ?X1&:(or (eq (class ?X1) Content) (subclassp (class ?X1) Content))) (predicate ?Y76&:(or (eq (class ?Y76) Has) (subclassp (class ?Y76) Has))&:(eq (send ?Y76 get-what) [private])) (time ?X2&:(or (eq (class ?X2) Duration) (subclassp (class ?X2) Duration))) (truth 1))) => (add-prop [manage_perm] (add-pred IsNeeded for_action (add-pred View what ?X1)) ?X2 1))
(defrule 5bd5bad3c0ea4560a7010a7737fe662f (logical (object (is-a Proposition) (subject ?X1&:(or (eq (class ?X1) Person) (subclassp (class ?X1) Person))) (predicate ?Y78&:(or (eq (class ?Y78) Hide) (subclassp (class ?Y78) Hide))&:(or (eq (class (send ?Y78 get-what)) Content) (subclassp (class (send ?Y78 get-what)) Content))) (time ?X3) (truth 1))) (logical (object (is-a Proposition) (subject ?X2&:(eq ?X2 (send ?Y78 get-what))) (predicate ?Y79&:(or (eq (class ?Y79) Has) (subclassp (class ?Y79) Has))&:(or (eq (class (send ?Y79 get-what)) Status) (subclassp (class (send ?Y79 get-what)) Status))) (time ?X5&:(or (eq (class ?X5) Duration) (subclassp (class ?X5) Duration))) (truth 1))) => (send ?X5 put-end 733697) (add-prop (send ?Y78 get-what) (add-pred Has what [private]) (make-instance of Duration (start ?X3) (end -1.0)) 1))
(defrule 5e390f7b6ab641a49ec105e68e9723b6 (logical (object (is-a Person) (name ?X1))) => (add-prop ?X1 (add-pred Has what [member]) (make-instance of Duration (start 733697.0) (end -1.0)) 1))
(defrule 82329082f7494ddb93e3c082117773e7 (logical (object (is-a Permission) (name ?X2))) => (add-prop [manager] (add-pred Has what ?X2) (make-instance of Duration (start 733697.0) (end -1.0)) 1))
(defrule 85a70d9d763b4b7895b30a644c3d9ad8 (logical (object (is-a Proposition) (subject ?X1&:(or (eq (class ?X1) Person) (subclassp (class ?X1) Person))) (predicate ?Y81&:(or (eq (class ?Y81) Has) (subclassp (class ?Y81) Has))&:(or (eq (class (send ?Y81 get-what)) Role) (subclassp (class (send ?Y81 get-what)) Role))) (time ?X3&:(or (eq (class ?X3) Duration) (subclassp (class ?X3) Duration))) (truth 1))) (logical (object (is-a Proposition) (subject ?X2&:(eq ?X2 (send ?Y81 get-what))) (predicate ?Y83&:(or (eq (class ?Y83) Has) (subclassp (class ?Y83) Has))&:(or (eq (class (send ?Y83 get-what)) Permission) (subclassp (class (send ?Y83 get-what)) Permission))) (time ?X5&:(or (eq (class ?X5) Duration) (subclassp (class ?X5) Duration))) (truth 1))) (test (or (and (>= (send ?X3 get-start) (send ?X3 get-start)) (or (<= (send ?X3 get-start) (send ?X3 get-end)) (= (send ?X3 get-end) -1))) (and (>= (send ?X3 get-start) (send ?X3 get-start)) (or (<= (send ?X3 get-start) (send ?X3 get-end)) (= (send ?X3 get-end) -1))))) => (add-prop ?X1 (add-pred Has what (send ?Y83 get-what)) (make-instance of Duration (start (mincomstart ?X3 ?X5)) (end (maxcomend ?X3 ?X5))) 1))
(defrule 92bd1f2cd14b4242a0531a65745fe2dd (logical (object (is-a Proposition) (subject ?X1&:(or (eq (class ?X1) Person) (subclassp (class ?X1) Person))) (predicate ?Y85&:(or (eq (class ?Y85) IsIn) (subclassp (class ?Y85) IsIn))) (time ?X3&:(or (eq (class ?X3) Duration) (subclassp (class ?X3) Duration))) (truth 1))) (logical (object (is-a Proposition) (subject ?X2&:(or (eq (class ?X2) Group) (subclassp (class ?X2) Group))) (predicate ?Y87&:(or (eq (class ?Y87) Has) (subclassp (class ?Y87) Has))&:(or (eq (class (send ?Y87 get-what)) Permission) (subclassp (class (send ?Y87 get-what)) Permission))) (time ?X5&:(or (eq (class ?X5) Duration) (subclassp (class ?X5) Duration))) (truth 1))) (test (or (and (>= (send ?X3 get-start) (send ?X3 get-start)) (or (<= (send ?X3 get-start) (send ?X3 get-end)) (= (send ?X3 get-end) -1))) (and (>= (send ?X3 get-start) (send ?X3 get-start)) (or (<= (send ?X3 get-start) (send ?X3 get-end)) (= (send ?X3 get-end) -1))))) => (add-prop ?X1 (add-pred Has what (send ?Y87 get-what)) (make-instance of Duration (start (mincomstart ?X3 ?X5)) (end (maxcomend ?X3 ?X5))) 1))
(defrule b741d07e127f4b71a2b12a6f797a0e1f (logical (object (is-a Proposition) (subject ?X1&:(or (eq (class ?X1) Person) (subclassp (class ?X1) Person))) (predicate ?Y89&:(or (eq (class ?Y89) Wants) (subclassp (class ?Y89) Wants))&:(or (eq (class (send ?Y89 get-to)) Create) (subclassp (class (send ?Y89 get-to)) Create))&:(or (eq (class (send (send ?Y89 get-to) get-what)) Thing) (subclassp (class (send (send ?Y89 get-to) get-what)) Thing))) (time ?X2) (truth 1))) (logical (object (is-a Proposition) (subject ?X1) (predicate ?Y90&:(or (eq (class ?Y90) Has) (subclassp (class ?Y90) Has))&:(eq (send ?Y90 get-what) [create_perm])) (time ?X3&:(or (eq (class ?X3) Duration) (subclassp (class ?X3) Duration))) (truth 1))) (test (and (<= (send ?X3 get-start) ?X2) (or (= (send ?X3 get-end) -1) (>= (send ?X3 get-end) ?X2)))) => (add-prop ?X1 (add-pred Create what (send (send ?Y89 get-to) get-what)) ?X2 1))
(defrule c2659c60f4bd4dafa172b38d0f1b4f54 (logical (object (is-a Proposition) (subject ?X1&:(or (eq (class ?X1) Content) (subclassp (class ?X1) Content))) (predicate ?Y92&:(or (eq (class ?Y92) Has) (subclassp (class ?Y92) Has))&:(eq (send ?Y92 get-what) [private])) (time ?X2&:(or (eq (class ?X2) Duration) (subclassp (class ?X2) Duration))) (truth 1))) => (add-prop [manage_perm] (add-pred IsNeeded for_action (add-pred View what ?X1)) ?X2 1))
(defrule ca77bccaa164400c9529106e736f3327 (logical (object (is-a Proposition) (subject ?X1&:(or (eq (class ?X1) Person) (subclassp (class ?X1) Person))) (predicate ?Y94&:(or (eq (class ?Y94) Publish) (subclassp (class ?Y94) Publish))&:(or (eq (class (send ?Y94 get-what)) Content) (subclassp (class (send ?Y94 get-what)) Content))) (time ?X3) (truth 1))) (logical (object (is-a Proposition) (subject ?X2&:(eq ?X2 (send ?Y94 get-what))) (predicate ?Y95&:(or (eq (class ?Y95) Has) (subclassp (class ?Y95) Has))&:(or (eq (class (send ?Y95 get-what)) Status) (subclassp (class (send ?Y95 get-what)) Status))) (time ?X5&:(or (eq (class ?X5) Duration) (subclassp (class ?X5) Duration))) (truth 1))) => (send ?X5 put-end 733697) (add-prop (send ?Y94 get-what) (add-pred Has what [public]) (make-instance of Duration (start ?X3) (end -1.0)) 1))
(defrule d2bb33e4ca2242c7a6ea4f723ad7018a (logical (object (is-a Proposition) (subject ?X1&:(or (eq (class ?X1) Person) (subclassp (class ?X1) Person))) (predicate ?Y97&:(or (eq (class ?Y97) IsIn) (subclassp (class ?Y97) IsIn))) (time ?X3&:(or (eq (class ?X3) Duration) (subclassp (class ?X3) Duration))) (truth 1))) (logical (object (is-a Proposition) (subject ?X2&:(or (eq (class ?X2) Group) (subclassp (class ?X2) Group))) (predicate ?Y99&:(or (eq (class ?Y99) Has) (subclassp (class ?Y99) Has))&:(or (eq (class (send ?Y99 get-what)) Permission) (subclassp (class (send ?Y99 get-what)) Permission))) (time ?X5&:(or (eq (class ?X5) Duration) (subclassp (class ?X5) Duration))) (truth 1))) (test (or (and (>= (send ?X3 get-start) (send ?X3 get-start)) (or (<= (send ?X3 get-start) (send ?X3 get-end)) (= (send ?X3 get-end) -1))) (and (>= (send ?X3 get-start) (send ?X3 get-start)) (or (<= (send ?X3 get-start) (send ?X3 get-end)) (= (send ?X3 get-end) -1))))) => (add-prop ?X1 (add-pred Has what (send ?Y99 get-what)) (make-instance of Duration (start (mincomstart ?X3 ?X5)) (end (maxcomend ?X3 ?X5))) 1))
(defrule d4d815ae33bd49b7acafbdf953294c28 (logical (object (is-a Content) (name ?X1))) => (add-prop [manage_perm] (add-pred IsNeeded for_action (add-pred Publish what ?X1)) (make-instance of Duration (start 733697.0) (end -1.0)) 1))
(defrule db686b03af6c4e0a80c866925be65a5a (logical (object (is-a Content) (name ?X1))) => (add-prop [manage_perm] (add-pred IsNeeded for_action (add-pred Publish what ?X1)) (make-instance of Duration (start 733697.0) (end -1.0)) 1))
(defrule ddfdcfe7c2a646db850a95067d2f575b (logical (object (is-a Proposition) (subject ?X1&:(or (eq (class ?X1) Content) (subclassp (class ?X1) Content))) (predicate ?Y101&:(or (eq (class ?Y101) Has) (subclassp (class ?Y101) Has))&:(eq (send ?Y101 get-what) [public])) (time ?X2&:(or (eq (class ?X2) Duration) (subclassp (class ?X2) Duration))) (truth 1))) => (add-prop [basic_perm] (add-pred IsNeeded for_action (add-pred View what ?X1)) ?X2 1))
(defrule deef1978c7de43e69d49eb1c3ac7d885 (logical (object (is-a Permission) (name ?X2))) => (add-prop [manager] (add-pred Has what ?X2) (make-instance of Duration (start 733697.0) (end -1.0)) 1))
(defrule dfd7da19575a4b7fac832a4f2a21fcfd (logical (object (is-a Proposition) (subject ?X1&:(or (eq (class ?X1) Thing) (subclassp (class ?X1) Thing))) (predicate ?Y103&:(or (eq (class ?Y103) IsIn) (subclassp (class ?Y103) IsIn))&:(or (eq (class (send ?Y103 get-what)) Thing) (subclassp (class (send ?Y103 get-what)) Thing))) (time ?X4&:(or (eq (class ?X4) Duration) (subclassp (class ?X4) Duration))) (truth 1))) (logical (object (is-a Proposition) (subject ?X2&:(eq ?X2 (send ?Y103 get-what))) (predicate ?Y105&:(or (eq (class ?Y105) IsIn) (subclassp (class ?Y105) IsIn))&:(or (eq (class (send ?Y105 get-what)) Thing) (subclassp (class (send ?Y105 get-what)) Thing))) (time ?X5&:(or (eq (class ?X5) Duration) (subclassp (class ?X5) Duration))) (truth 1))) (test (or (and (>= (send ?X4 get-start) (send ?X4 get-start)) (or (<= (send ?X4 get-start) (send ?X4 get-end)) (= (send ?X4 get-end) -1))) (and (>= (send ?X4 get-start) (send ?X4 get-start)) (or (<= (send ?X4 get-start) (send ?X4 get-end)) (= (send ?X4 get-end) -1))))) => (add-prop ?X1 (add-pred IsIn what (send ?Y105 get-what)) (make-instance of Duration (start (mincomstart ?X4 ?X5)) (end (maxcomend ?X4 ?X5))) 1))
(defrule eacd2d4096d64677b09493e41eab4eea (logical (object (is-a Proposition) (subject ?X1&:(or (eq (class ?X1) Person) (subclassp (class ?X1) Person))) (predicate ?Y107&:(or (eq (class ?Y107) Create) (subclassp (class ?Y107) Create))&:(or (eq (class (send ?Y107 get-what)) Content) (subclassp (class (send ?Y107 get-what)) Content))) (time ?X3) (truth 1))) => (reduce-class (send ?Y107 get-what) Content) (add-prop ?X1 (add-pred IsOwner of (send ?Y107 get-what)) (make-instance of Duration (start ?X3) (end -1.0)) 1) (add-prop (send ?Y107 get-what) (add-pred Has what [private]) (make-instance of Duration (start ?X3) (end -1.0)) 1))
(defrule f0771a52098f4dc6b4ac2dab262c9877 (logical (object (is-a Proposition) (subject ?X1&:(or (eq (class ?X1) Person) (subclassp (class ?X1) Person))) (predicate ?Y108&:(or (eq (class ?Y108) IsOwner) (subclassp (class ?Y108) IsOwner))&:(or (eq (class (send ?Y108 get-of)) Content) (subclassp (class (send ?Y108 get-of)) Content))) (time ?X3&:(or (eq (class ?X3) Duration) (subclassp (class ?X3) Duration))) (truth 1))) => (add-prop ?X1 (add-pred Can what (add-pred Hide what (send ?Y108 get-of))) ?X3 1))
(defrule f1a8cfe912f044b386ba818b60208dc0 (logical (object (is-a Proposition) (subject ?X1&:(or (eq (class ?X1) Person) (subclassp (class ?X1) Person))) (predicate ?Y110&:(or (eq (class ?Y110) Has) (subclassp (class ?Y110) Has))&:(or (eq (class (send ?Y110 get-what)) Role) (subclassp (class (send ?Y110 get-what)) Role))) (time ?X3&:(or (eq (class ?X3) Duration) (subclassp (class ?X3) Duration))) (truth 1))) (logical (object (is-a Proposition) (subject ?X2&:(eq ?X2 (send ?Y110 get-what))) (predicate ?Y112&:(or (eq (class ?Y112) Has) (subclassp (class ?Y112) Has))&:(or (eq (class (send ?Y112 get-what)) Permission) (subclassp (class (send ?Y112 get-what)) Permission))) (time ?X5&:(or (eq (class ?X5) Duration) (subclassp (class ?X5) Duration))) (truth 1))) (test (or (and (>= (send ?X3 get-start) (send ?X3 get-start)) (or (<= (send ?X3 get-start) (send ?X3 get-end)) (= (send ?X3 get-end) -1))) (and (>= (send ?X3 get-start) (send ?X3 get-start)) (or (<= (send ?X3 get-start) (send ?X3 get-end)) (= (send ?X3 get-end) -1))))) => (add-prop ?X1 (add-pred Has what (send ?Y112 get-what)) (make-instance of Duration (start (mincomstart ?X3 ?X5)) (end (maxcomend ?X3 ?X5))) 1))
(defrule f30a0513b1e7497d9b1959b9dafbb785 (logical (object (is-a Proposition) (subject ?X1&:(or (eq (class ?X1) Thing) (subclassp (class ?X1) Thing))) (predicate ?Y114&:(or (eq (class ?Y114) IsIn) (subclassp (class ?Y114) IsIn))&:(or (eq (class (send ?Y114 get-what)) Thing) (subclassp (class (send ?Y114 get-what)) Thing))) (time ?X4&:(or (eq (class ?X4) Duration) (subclassp (class ?X4) Duration))) (truth 1))) (logical (object (is-a Proposition) (subject ?X2&:(eq ?X2 (send ?Y114 get-what))) (predicate ?Y116&:(or (eq (class ?Y116) IsIn) (subclassp (class ?Y116) IsIn))&:(or (eq (class (send ?Y116 get-what)) Thing) (subclassp (class (send ?Y116 get-what)) Thing))) (time ?X5&:(or (eq (class ?X5) Duration) (subclassp (class ?X5) Duration))) (truth 1))) (test (or (and (>= (send ?X4 get-start) (send ?X4 get-start)) (or (<= (send ?X4 get-start) (send ?X4 get-end)) (= (send ?X4 get-end) -1))) (and (>= (send ?X4 get-start) (send ?X4 get-start)) (or (<= (send ?X4 get-start) (send ?X4 get-end)) (= (send ?X4 get-end) -1))))) => (add-prop ?X1 (add-pred IsIn what (send ?Y116 get-what)) (make-instance of Duration (start (mincomstart ?X4 ?X5)) (end (maxcomend ?X4 ?X5))) 1))
(add-prop [admin] (add-pred Has what [manager]) (make-instance of Duration (start 733697.0) (end -1.0)) 1)
admin has what manager at from 733697.0 till -1.0
(defclass Name (is-a USER))
(deffunction reduce-class (?instance ?class) (if (eq (length$ (find-instance ((?a ?class)) (eq (instance-name ?a) ?instance))) 0) then (make-instance ?instance of ?class) (python-call tonl ?class ?instance)))
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


(deffunction mincomstart (?dur1 ?dur2)
    (return (max (send ?dur1 get-start) (send ?dur2 get-start)))
)



(deffunction maxcomend (?dur1 ?dur2)
    (bind ?e1 (send ?dur1 get-end))
    (bind ?e2 (send ?dur2 get-end))
    (if (= ?e1 ?e2) then (return ?e1))
    (if (= ?e2 -1) then
        (return ?e1)
    )
    (if (= ?e1 -1) then
        (return ?e2)
    )
    (return (min ?e1 ?e2))
)

(defclass Proposition (is-a Name) (slot truth (type INTEGER) (default 1) (pattern-match reactive)) (slot subject (type INSTANCE) (pattern-match reactive)) (slot predicate (type INSTANCE) (pattern-match reactive)) (slot time (type ?VARIABLE) (pattern-match reactive)))

(deffunction add-prop (?s ?p ?t ?r)
       (if (and (python-call ptonl ?s ?p ?t ?r)
                (= (+ (length$ (find-instance ((?prop Proposition) (?dur Duration))
                          (and (eq ?prop:subject ?s)
                               (eq ?prop:predicate ?p)
                               (eq ?prop:time ?dur)
                               (= ?dur:start (send ?t get-start))
                               (= ?dur:end (send ?t get-end))
                               (eq ?prop:truth ?r))))
                      (length$ (find-instance ((?prop Proposition))
                          (and (eq ?prop:subject ?s)
                               (eq ?prop:predicate ?p)
                               (= ?prop:time ?t)
                               (eq ?prop:truth ?r)))))
                 0))
        then (make-instance of Proposition (subject ?s)
                                           (predicate ?p)
                                           (time ?t)
                                           (truth ?r))
        else (return TRUE)))
------------OPEN---------------------
----------F-OPEN---------------------
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
admin
(reduce-class [member] Role)
member
(reduce-class [manager] Role)
manager
(reduce-class [basic_perm] Permission)
basic_perm
(reduce-class [manage_perm] Permission)
manage_perm
(reduce-class [create_perm] Permission)
create_perm
(reduce-class [public] Status)
public
(reduce-class [private] Status)
private
(add-prop [admin] (add-pred Has what [manager]) (make-instance of Duration (start 733697.0) (end -1.0)) 1)
admin has what manager at from 733697.0 till -1.0
(add-prop [member] (add-pred Has what [basic_perm]) (make-instance of Duration (start 733697.0) (end -1.0)) 1)
member has what basic_perm at from 733697.0 till -1.0
(defrule 280dd7539df8411d804388ee02f5ace1 (logical (object (is-a Proposition) (subject ?X1&:(or (eq (class ?X1) Person) (subclassp (class ?X1) Person))) (predicate ?Y1&:(or (eq (class ?Y1) Wants) (subclassp (class ?Y1) Wants))&:(or (eq (class (send ?Y1 get-to)) Create) (subclassp (class (send ?Y1 get-to)) Create))&:(or (eq (class (send (send ?Y1 get-to) get-what)) Thing) (subclassp (class (send (send ?Y1 get-to) get-what)) Thing))) (time ?X2) (truth 1))) (logical (object (is-a Proposition) (subject ?X1) (predicate ?Y2&:(or (eq (class ?Y2) Has) (subclassp (class ?Y2) Has))&:(eq (send ?Y2 get-what) [create_perm])) (time ?X3&:(or (eq (class ?X3) Duration) (subclassp (class ?X3) Duration))) (truth 1))) (test (and (<= (send ?X3 get-start) ?X2) (or (= (send ?X3 get-end) -1) (>= (send ?X3 get-end) ?X2)))) => (add-prop ?X1 (add-pred Create what (send (send ?Y1 get-to) get-what)) ?X2 1))
(defrule 6c1095a9ce7a4a8dbb0f0848bca466f8 (logical (object (is-a Proposition) (subject ?X1&:(or (eq (class ?X1) Person) (subclassp (class ?X1) Person))) (predicate ?Y4&:(or (eq (class ?Y4) Wants) (subclassp (class ?Y4) Wants))) (time ?X2) (truth 1))) (logical (object (is-a Proposition) (subject ?X1) (predicate ?Y5&:(or (eq (class ?Y5) Can) (subclassp (class ?Y5) Can))&:(eq (send ?Y4 get-to) (send ?Y5 get-what))) (time ?X3&:(or (eq (class ?X3) Duration) (subclassp (class ?X3) Duration))) (truth 1))) (test (and (<= (send ?X3 get-start) ?X2) (or (= (send ?X3 get-end) -1) (>= (send ?X3 get-end) ?X2)))) => (add-prop ?X1 (send ?Y4 get-to) ?X2 1))
(defrule 25e9bf50916d411f989515f16e109c33 (logical (object (is-a Proposition) (subject ?X2&:(or (eq (class ?X2) Thing) (subclassp (class ?X2) Thing))) (predicate ?Y7&:(or (eq (class ?Y7) IsNeeded) (subclassp (class ?Y7) IsNeeded))) (time ?X3&:(or (eq (class ?X3) Duration) (subclassp (class ?X3) Duration))) (truth 1))) (logical (object (is-a Proposition) (subject ?X1&:(or (eq (class ?X1) Thing) (subclassp (class ?X1) Thing))) (predicate ?Y9&:(or (eq (class ?Y9) Has) (subclassp (class ?Y9) Has))&:(eq (send ?Y9 get-what) ?X2)) (time ?X5&:(or (eq (class ?X5) Duration) (subclassp (class ?X5) Duration))) (truth 1))) (test (or (and (>= (send ?X3 get-start) (send ?X3 get-start)) (or (<= (send ?X3 get-start) (send ?X3 get-end)) (= (send ?X3 get-end) -1))) (and (>= (send ?X3 get-start) (send ?X3 get-start)) (or (<= (send ?X3 get-start) (send ?X3 get-end)) (= (send ?X3 get-end) -1))))) => (add-prop ?X1 (add-pred Can what (send ?Y7 get-for_action)) (make-instance of Duration (start (mincomstart ?X3 ?X5)) (end (maxcomend ?X3 ?X5))) 1))
(defrule e54ebb487e3e465b975e67856725f2fa (logical (object (is-a Proposition) (subject ?X1&:(or (eq (class ?X1) Thing) (subclassp (class ?X1) Thing))) (predicate ?Y11&:(or (eq (class ?Y11) IsIn) (subclassp (class ?Y11) IsIn))&:(or (eq (class (send ?Y11 get-what)) Thing) (subclassp (class (send ?Y11 get-what)) Thing))) (time ?X4&:(or (eq (class ?X4) Duration) (subclassp (class ?X4) Duration))) (truth 1))) (logical (object (is-a Proposition) (subject ?X2&:(eq ?X2 (send ?Y11 get-what))) (predicate ?Y13&:(or (eq (class ?Y13) IsIn) (subclassp (class ?Y13) IsIn))&:(or (eq (class (send ?Y13 get-what)) Thing) (subclassp (class (send ?Y13 get-what)) Thing))) (time ?X5&:(or (eq (class ?X5) Duration) (subclassp (class ?X5) Duration))) (truth 1))) (test (or (and (>= (send ?X4 get-start) (send ?X4 get-start)) (or (<= (send ?X4 get-start) (send ?X4 get-end)) (= (send ?X4 get-end) -1))) (and (>= (send ?X4 get-start) (send ?X4 get-start)) (or (<= (send ?X4 get-start) (send ?X4 get-end)) (= (send ?X4 get-end) -1))))) => (add-prop ?X1 (add-pred IsIn what (send ?Y13 get-what)) (make-instance of Duration (start (mincomstart ?X4 ?X5)) (end (maxcomend ?X4 ?X5))) 1))
(defrule f6e3e5921a564e85a1e6500a316f0c54 (logical (object (is-a Proposition) (subject ?X1&:(or (eq (class ?X1) Person) (subclassp (class ?X1) Person))) (predicate ?Y15&:(or (eq (class ?Y15) IsIn) (subclassp (class ?Y15) IsIn))) (time ?X3&:(or (eq (class ?X3) Duration) (subclassp (class ?X3) Duration))) (truth 1))) (logical (object (is-a Proposition) (subject ?X2&:(or (eq (class ?X2) Group) (subclassp (class ?X2) Group))) (predicate ?Y17&:(or (eq (class ?Y17) Has) (subclassp (class ?Y17) Has))&:(or (eq (class (send ?Y17 get-what)) Permission) (subclassp (class (send ?Y17 get-what)) Permission))) (time ?X5&:(or (eq (class ?X5) Duration) (subclassp (class ?X5) Duration))) (truth 1))) (test (or (and (>= (send ?X3 get-start) (send ?X3 get-start)) (or (<= (send ?X3 get-start) (send ?X3 get-end)) (= (send ?X3 get-end) -1))) (and (>= (send ?X3 get-start) (send ?X3 get-start)) (or (<= (send ?X3 get-start) (send ?X3 get-end)) (= (send ?X3 get-end) -1))))) => (add-prop ?X1 (add-pred Has what (send ?Y17 get-what)) (make-instance of Duration (start (mincomstart ?X3 ?X5)) (end (maxcomend ?X3 ?X5))) 1))
(defrule 5de4f30bbe4141eebb226a47c6e9203e (logical (object (is-a Proposition) (subject ?X1&:(or (eq (class ?X1) Person) (subclassp (class ?X1) Person))) (predicate ?Y19&:(or (eq (class ?Y19) Has) (subclassp (class ?Y19) Has))&:(or (eq (class (send ?Y19 get-what)) Role) (subclassp (class (send ?Y19 get-what)) Role))) (time ?X3&:(or (eq (class ?X3) Duration) (subclassp (class ?X3) Duration))) (truth 1))) (logical (object (is-a Proposition) (subject ?X2&:(eq ?X2 (send ?Y19 get-what))) (predicate ?Y21&:(or (eq (class ?Y21) Has) (subclassp (class ?Y21) Has))&:(or (eq (class (send ?Y21 get-what)) Permission) (subclassp (class (send ?Y21 get-what)) Permission))) (time ?X5&:(or (eq (class ?X5) Duration) (subclassp (class ?X5) Duration))) (truth 1))) (test (or (and (>= (send ?X3 get-start) (send ?X3 get-start)) (or (<= (send ?X3 get-start) (send ?X3 get-end)) (= (send ?X3 get-end) -1))) (and (>= (send ?X3 get-start) (send ?X3 get-start)) (or (<= (send ?X3 get-start) (send ?X3 get-end)) (= (send ?X3 get-end) -1))))) => (add-prop ?X1 (add-pred Has what (send ?Y21 get-what)) (make-instance of Duration (start (mincomstart ?X3 ?X5)) (end (maxcomend ?X3 ?X5))) 1))
(defrule 68ff58cfe0894c6883eb66c615f9b6aa (logical (object (is-a Person) (name ?X1))) => (add-prop ?X1 (add-pred Has what [member]) (make-instance of Duration (start 733697.0) (end -1.0)) 1))
(defrule 50eb1f4b20f049838b855a9a987e562b (logical (object (is-a Proposition) (subject ?X1&:(or (eq (class ?X1) Person) (subclassp (class ?X1) Person))) (predicate ?Y23&:(or (eq (class ?Y23) Create) (subclassp (class ?Y23) Create))&:(or (eq (class (send ?Y23 get-what)) Content) (subclassp (class (send ?Y23 get-what)) Content))) (time ?X3) (truth 1))) => (reduce-class (send ?Y23 get-what) Content) (add-prop ?X1 (add-pred IsOwner of (send ?Y23 get-what)) (make-instance of Duration (start ?X3) (end -1.0)) 1) (add-prop (send ?Y23 get-what) (add-pred Has what [private]) (make-instance of Duration (start ?X3) (end -1.0)) 1))
(defrule c6b381581cc042dfb8940ebf6015641e (logical (object (is-a Permission) (name ?X2))) => (add-prop [manager] (add-pred Has what ?X2) (make-instance of Duration (start 733697.0) (end -1.0)) 1))
(defrule f440fc3d51ed49f2b514e7a76b18c3b0 (logical (object (is-a Proposition) (subject ?X1&:(or (eq (class ?X1) Content) (subclassp (class ?X1) Content))) (predicate ?Y24&:(or (eq (class ?Y24) Has) (subclassp (class ?Y24) Has))&:(eq (send ?Y24 get-what) [public])) (time ?X2&:(or (eq (class ?X2) Duration) (subclassp (class ?X2) Duration))) (truth 1))) => (add-prop [basic_perm] (add-pred IsNeeded for_action (add-pred View what ?X1)) ?X2 1))
(defrule e8de5d95daf64a28a7aa439d35c3af31 (logical (object (is-a Proposition) (subject ?X1&:(or (eq (class ?X1) Content) (subclassp (class ?X1) Content))) (predicate ?Y26&:(or (eq (class ?Y26) Has) (subclassp (class ?Y26) Has))&:(eq (send ?Y26 get-what) [private])) (time ?X2&:(or (eq (class ?X2) Duration) (subclassp (class ?X2) Duration))) (truth 1))) => (add-prop [manage_perm] (add-pred IsNeeded for_action (add-pred View what ?X1)) ?X2 1))
(defrule 61280bcf8dfc4c33bb950a62b1b42cc2 (logical (object (is-a Proposition) (subject ?X1&:(or (eq (class ?X1) Content) (subclassp (class ?X1) Content))) (predicate ?Y28&:(or (eq (class ?Y28) Has) (subclassp (class ?Y28) Has))&:(eq (send ?Y28 get-what) [private])) (time ?X3&:(or (eq (class ?X3) Duration) (subclassp (class ?X3) Duration))) (truth 1))) (logical (object (is-a Proposition) (subject ?X2&:(or (eq (class ?X2) Person) (subclassp (class ?X2) Person))) (predicate ?Y30&:(or (eq (class ?Y30) IsOwner) (subclassp (class ?Y30) IsOwner))&:(eq (send ?Y30 get-of) ?X1)) (time ?X4&:(or (eq (class ?X4) Duration) (subclassp (class ?X4) Duration))) (truth 1))) (test (or (and (>= (send ?X3 get-start) (send ?X3 get-start)) (or (<= (send ?X3 get-start) (send ?X3 get-end)) (= (send ?X3 get-end) -1))) (and (>= (send ?X3 get-start) (send ?X3 get-start)) (or (<= (send ?X3 get-start) (send ?X3 get-end)) (= (send ?X3 get-end) -1))))) => (add-prop ?X2 (add-pred Can what (add-pred View what ?X1)) (make-instance of Duration (start (mincomstart ?X3 ?X4)) (end (maxcomend ?X3 ?X4))) 1))
(defrule fa825abde56749dd92e84e7f0a446fbb (logical (object (is-a Proposition) (subject ?X1&:(or (eq (class ?X1) Person) (subclassp (class ?X1) Person))) (predicate ?Y32&:(or (eq (class ?Y32) Publish) (subclassp (class ?Y32) Publish))&:(or (eq (class (send ?Y32 get-what)) Content) (subclassp (class (send ?Y32 get-what)) Content))) (time ?X3) (truth 1))) (logical (object (is-a Proposition) (subject ?X2&:(eq ?X2 (send ?Y32 get-what))) (predicate ?Y33&:(or (eq (class ?Y33) Has) (subclassp (class ?Y33) Has))&:(or (eq (class (send ?Y33 get-what)) Status) (subclassp (class (send ?Y33 get-what)) Status))) (time ?X5&:(or (eq (class ?X5) Duration) (subclassp (class ?X5) Duration))) (truth 1))) => (send ?X5 put-end 733697) (add-prop (send ?Y32 get-what) (add-pred Has what [public]) (make-instance of Duration (start ?X3) (end -1.0)) 1))
(defrule e9873d400c3c4dca8c86c3cf3a9026e4 (logical (object (is-a Content) (name ?X1))) => (add-prop [manage_perm] (add-pred IsNeeded for_action (add-pred Publish what ?X1)) (make-instance of Duration (start 733697.0) (end -1.0)) 1))
(defrule 79625ecbcff7472dad2adab4fad2c664 (logical (object (is-a Proposition) (subject ?X1&:(or (eq (class ?X1) Person) (subclassp (class ?X1) Person))) (predicate ?Y35&:(or (eq (class ?Y35) Hide) (subclassp (class ?Y35) Hide))&:(or (eq (class (send ?Y35 get-what)) Content) (subclassp (class (send ?Y35 get-what)) Content))) (time ?X3) (truth 1))) (logical (object (is-a Proposition) (subject ?X2&:(eq ?X2 (send ?Y35 get-what))) (predicate ?Y36&:(or (eq (class ?Y36) Has) (subclassp (class ?Y36) Has))&:(or (eq (class (send ?Y36 get-what)) Status) (subclassp (class (send ?Y36 get-what)) Status))) (time ?X5&:(or (eq (class ?X5) Duration) (subclassp (class ?X5) Duration))) (truth 1))) => (send ?X5 put-end 733697) (add-prop (send ?Y35 get-what) (add-pred Has what [private]) (make-instance of Duration (start ?X3) (end -1.0)) 1))
(defrule 141828dd444241dfa6492162779535c6 (logical (object (is-a Proposition) (subject ?X1&:(or (eq (class ?X1) Person) (subclassp (class ?X1) Person))) (predicate ?Y38&:(or (eq (class ?Y38) IsOwner) (subclassp (class ?Y38) IsOwner))&:(or (eq (class (send ?Y38 get-of)) Content) (subclassp (class (send ?Y38 get-of)) Content))) (time ?X3&:(or (eq (class ?X3) Duration) (subclassp (class ?X3) Duration))) (truth 1))) => (add-prop ?X1 (add-pred Can what (add-pred Hide what (send ?Y38 get-of))) ?X3 1))
(reduce-class [john] Person)
john
(reduce-class [pete] Person)
pete
(reduce-class [jane] Person)
jane
(reduce-class [c1] Content)
c1
(reduce-class [c2] Content)
c2
(add-prop [john] (add-pred Has what [manager]) (make-instance of Duration (start 733697.0) (end -1.0)) 1)
john has what manager at from 733697.0 till -1.0
(add-prop [jane] (add-pred Has what [create_perm]) (make-instance of Duration (start 733697.0) (end -1.0)) 1)
jane has what create_perm at from 733697.0 till -1.0
(add-prop [jane] (add-pred Wants to (add-pred Create what [c1])) 733697.0 1)
jane wants to create what c1 at 733697.0
(add-prop [pete] (add-pred Wants to (add-pred Create what [c2])) 733697.0 1)
pete wants to create what c2 at 733697.0
(add-prop [jane] (add-pred Wants to (add-pred Publish what [c1])) 733697.0 1)
jane wants to publish what c1 at 733697.0
(add-prop [pete] (add-pred Wants to (add-pred Publish what [c2])) 733697.0 1)
pete wants to publish what c2 at 733697.0
(add-prop [john] (add-pred Wants to (add-pred Publish what [c1])) 733697.0 1)
john wants to publish what c1 at 733697.0
----------running---------------------
jane create what c1 at 733697.0
jane isowner of c1 at from 733697.0 till -1.0
c1 has what private at from 733697.0 till -1.0
jane can what view what c1 at from 733697.0 till -1.0
manage_perm isneeded for_action view what c1 at from 733697.0 till -1.0
jane can what hide what c1 at from 733697.0 till -1.0
manage_perm isneeded for_action publish what c2 at from 733697.0 till -1.0
manage_perm isneeded for_action publish what c1 at from 733697.0 till -1.0
jane has what member at from 733697.0 till -1.0
jane has what basic_perm at from 733697.0 till -1.0
pete has what member at from 733697.0 till -1.0
pete has what basic_perm at from 733697.0 till -1.0
john has what member at from 733697.0 till -1.0
john has what basic_perm at from 733697.0 till -1.0
manager has what create_perm at from 733697.0 till -1.0
admin has what create_perm at from 733697.0 till -1.0
john has what create_perm at from 733697.0 till -1.0
manager has what manage_perm at from 733697.0 till -1.0
manager can what view what c1 at from 733697.0 till -1.0
manager can what publish what c2 at from 733697.0 till -1.0
manager can what publish what c1 at from 733697.0 till -1.0
admin has what manage_perm at from 733697.0 till -1.0
admin can what view what c1 at from 733697.0 till -1.0
admin can what publish what c2 at from 733697.0 till -1.0
admin can what publish what c1 at from 733697.0 till -1.0
john has what manage_perm at from 733697.0 till -1.0
john can what view what c1 at from 733697.0 till -1.0
john can what publish what c2 at from 733697.0 till -1.0
john can what publish what c1 at from 733697.0 till -1.0
john publish what c1 at 733697.0
c1 has what public at from 733697.0 till -1.0
basic_perm isneeded for_action view what c1 at from 733697.0 till -1.0
pete can what view what c1 at from 733697.0 till -1.0
member can what view what c1 at from 733697.0 till -1.0
manager has what basic_perm at from 733697.0 till -1.0
manager can what view what c1 at from 733697.0 till 733697.0
----------runned---------------------
(find-all-instances ((?prop Proposition) (?Y40 Has) (?Y41 Duration)) (and (eq ?prop:subject [c1]) (eq ?Y40:what [private]) (eq ?prop:predicate ?Y40) (= ?Y41:start 733697.0) (= ?Y41:end -1.0) (eq ?prop:truth 1)))
0


no
(defclass Name (is-a USER))
(deffunction reduce-class (?instance ?class) (if (eq (length$ (find-instance ((?a ?class)) (eq (instance-name ?a) ?instance))) 0) then (make-instance ?instance of ?class) (python-call tonl ?class ?instance)))
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


(deffunction mincomstart (?dur1 ?dur2)
    (return (max (send ?dur1 get-start) (send ?dur2 get-start)))
)



(deffunction maxcomend (?dur1 ?dur2)
    (bind ?e1 (send ?dur1 get-end))
    (bind ?e2 (send ?dur2 get-end))
    (if (= ?e1 ?e2) then (return ?e1))
    (if (= ?e2 -1) then
        (return ?e1)
    )
    (if (= ?e1 -1) then
        (return ?e2)
    )
    (return (min ?e1 ?e2))
)

(defclass Proposition (is-a Name) (slot truth (type INTEGER) (default 1) (pattern-match reactive)) (slot subject (type INSTANCE) (pattern-match reactive)) (slot predicate (type INSTANCE) (pattern-match reactive)) (slot time (type ?VARIABLE) (pattern-match reactive)))

(deffunction add-prop (?s ?p ?t ?r)
       (if (and (python-call ptonl ?s ?p ?t ?r)
                (= (+ (length$ (find-instance ((?prop Proposition) (?dur Duration))
                          (and (eq ?prop:subject ?s)
                               (eq ?prop:predicate ?p)
                               (eq ?prop:time ?dur)
                               (= ?dur:start (send ?t get-start))
                               (= ?dur:end (send ?t get-end))
                               (eq ?prop:truth ?r))))
                      (length$ (find-instance ((?prop Proposition))
                          (and (eq ?prop:subject ?s)
                               (eq ?prop:predicate ?p)
                               (= ?prop:time ?t)
                               (eq ?prop:truth ?r)))))
                 0))
        then (make-instance of Proposition (subject ?s)
                                           (predicate ?p)
                                           (time ?t)
                                           (truth ?r))
        else (return TRUE)))
------------OPEN---------------------
----------F-OPEN---------------------
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
admin
(reduce-class [member] Role)
member
(reduce-class [manager] Role)
manager
(reduce-class [basic_perm] Permission)
basic_perm
(reduce-class [manage_perm] Permission)
manage_perm
(reduce-class [create_perm] Permission)
create_perm
(reduce-class [public] Status)
public
(reduce-class [private] Status)
private
(add-prop [admin] (add-pred Has what [manager]) (make-instance of Duration (start 733697.0) (end -1.0)) 1)
admin has what manager at from 733697.0 till -1.0
(add-prop [member] (add-pred Has what [basic_perm]) (make-instance of Duration (start 733697.0) (end -1.0)) 1)
member has what basic_perm at from 733697.0 till -1.0
(defrule f35577b8109e45798e5c5f4f19d4c55b (logical (object (is-a Proposition) (subject ?X1&:(or (eq (class ?X1) Person) (subclassp (class ?X1) Person))) (predicate ?Y1&:(or (eq (class ?Y1) Wants) (subclassp (class ?Y1) Wants))&:(or (eq (class (send ?Y1 get-to)) Create) (subclassp (class (send ?Y1 get-to)) Create))&:(or (eq (class (send (send ?Y1 get-to) get-what)) Thing) (subclassp (class (send (send ?Y1 get-to) get-what)) Thing))) (time ?X2) (truth 1))) (logical (object (is-a Proposition) (subject ?X1) (predicate ?Y2&:(or (eq (class ?Y2) Has) (subclassp (class ?Y2) Has))&:(eq (send ?Y2 get-what) [create_perm])) (time ?X3&:(or (eq (class ?X3) Duration) (subclassp (class ?X3) Duration))) (truth 1))) (test (and (<= (send ?X3 get-start) ?X2) (or (= (send ?X3 get-end) -1) (>= (send ?X3 get-end) ?X2)))) => (add-prop ?X1 (add-pred Create what (send (send ?Y1 get-to) get-what)) ?X2 1))
(defrule 746fae92dc1540faa45befd6c8830c7c (logical (object (is-a Proposition) (subject ?X1&:(or (eq (class ?X1) Person) (subclassp (class ?X1) Person))) (predicate ?Y4&:(or (eq (class ?Y4) Wants) (subclassp (class ?Y4) Wants))) (time ?X2) (truth 1))) (logical (object (is-a Proposition) (subject ?X1) (predicate ?Y5&:(or (eq (class ?Y5) Can) (subclassp (class ?Y5) Can))&:(eq (send ?Y4 get-to) (send ?Y5 get-what))) (time ?X3&:(or (eq (class ?X3) Duration) (subclassp (class ?X3) Duration))) (truth 1))) (test (and (<= (send ?X3 get-start) ?X2) (or (= (send ?X3 get-end) -1) (>= (send ?X3 get-end) ?X2)))) => (add-prop ?X1 (send ?Y4 get-to) ?X2 1))
(defrule 21966b51f6474e59b9cc8b629a54445a (logical (object (is-a Proposition) (subject ?X2&:(or (eq (class ?X2) Thing) (subclassp (class ?X2) Thing))) (predicate ?Y7&:(or (eq (class ?Y7) IsNeeded) (subclassp (class ?Y7) IsNeeded))) (time ?X3&:(or (eq (class ?X3) Duration) (subclassp (class ?X3) Duration))) (truth 1))) (logical (object (is-a Proposition) (subject ?X1&:(or (eq (class ?X1) Thing) (subclassp (class ?X1) Thing))) (predicate ?Y9&:(or (eq (class ?Y9) Has) (subclassp (class ?Y9) Has))&:(eq (send ?Y9 get-what) ?X2)) (time ?X5&:(or (eq (class ?X5) Duration) (subclassp (class ?X5) Duration))) (truth 1))) (test (or (and (>= (send ?X3 get-start) (send ?X3 get-start)) (or (<= (send ?X3 get-start) (send ?X3 get-end)) (= (send ?X3 get-end) -1))) (and (>= (send ?X3 get-start) (send ?X3 get-start)) (or (<= (send ?X3 get-start) (send ?X3 get-end)) (= (send ?X3 get-end) -1))))) => (add-prop ?X1 (add-pred Can what (send ?Y7 get-for_action)) (make-instance of Duration (start (mincomstart ?X3 ?X5)) (end (maxcomend ?X3 ?X5))) 1))
(defrule 88af148b70de4f1b96962ae681fbb827 (logical (object (is-a Proposition) (subject ?X1&:(or (eq (class ?X1) Thing) (subclassp (class ?X1) Thing))) (predicate ?Y11&:(or (eq (class ?Y11) IsIn) (subclassp (class ?Y11) IsIn))&:(or (eq (class (send ?Y11 get-what)) Thing) (subclassp (class (send ?Y11 get-what)) Thing))) (time ?X4&:(or (eq (class ?X4) Duration) (subclassp (class ?X4) Duration))) (truth 1))) (logical (object (is-a Proposition) (subject ?X2&:(eq ?X2 (send ?Y11 get-what))) (predicate ?Y13&:(or (eq (class ?Y13) IsIn) (subclassp (class ?Y13) IsIn))&:(or (eq (class (send ?Y13 get-what)) Thing) (subclassp (class (send ?Y13 get-what)) Thing))) (time ?X5&:(or (eq (class ?X5) Duration) (subclassp (class ?X5) Duration))) (truth 1))) (test (or (and (>= (send ?X4 get-start) (send ?X4 get-start)) (or (<= (send ?X4 get-start) (send ?X4 get-end)) (= (send ?X4 get-end) -1))) (and (>= (send ?X4 get-start) (send ?X4 get-start)) (or (<= (send ?X4 get-start) (send ?X4 get-end)) (= (send ?X4 get-end) -1))))) => (add-prop ?X1 (add-pred IsIn what (send ?Y13 get-what)) (make-instance of Duration (start (mincomstart ?X4 ?X5)) (end (maxcomend ?X4 ?X5))) 1))
(defrule d0a16734ba924345a17964c098cd083a (logical (object (is-a Proposition) (subject ?X1&:(or (eq (class ?X1) Person) (subclassp (class ?X1) Person))) (predicate ?Y15&:(or (eq (class ?Y15) IsIn) (subclassp (class ?Y15) IsIn))) (time ?X3&:(or (eq (class ?X3) Duration) (subclassp (class ?X3) Duration))) (truth 1))) (logical (object (is-a Proposition) (subject ?X2&:(or (eq (class ?X2) Group) (subclassp (class ?X2) Group))) (predicate ?Y17&:(or (eq (class ?Y17) Has) (subclassp (class ?Y17) Has))&:(or (eq (class (send ?Y17 get-what)) Permission) (subclassp (class (send ?Y17 get-what)) Permission))) (time ?X5&:(or (eq (class ?X5) Duration) (subclassp (class ?X5) Duration))) (truth 1))) (test (or (and (>= (send ?X3 get-start) (send ?X3 get-start)) (or (<= (send ?X3 get-start) (send ?X3 get-end)) (= (send ?X3 get-end) -1))) (and (>= (send ?X3 get-start) (send ?X3 get-start)) (or (<= (send ?X3 get-start) (send ?X3 get-end)) (= (send ?X3 get-end) -1))))) => (add-prop ?X1 (add-pred Has what (send ?Y17 get-what)) (make-instance of Duration (start (mincomstart ?X3 ?X5)) (end (maxcomend ?X3 ?X5))) 1))
(defrule 62b2324b1ce94dfa8276fcccaf27b292 (logical (object (is-a Proposition) (subject ?X1&:(or (eq (class ?X1) Person) (subclassp (class ?X1) Person))) (predicate ?Y19&:(or (eq (class ?Y19) Has) (subclassp (class ?Y19) Has))&:(or (eq (class (send ?Y19 get-what)) Role) (subclassp (class (send ?Y19 get-what)) Role))) (time ?X3&:(or (eq (class ?X3) Duration) (subclassp (class ?X3) Duration))) (truth 1))) (logical (object (is-a Proposition) (subject ?X2&:(eq ?X2 (send ?Y19 get-what))) (predicate ?Y21&:(or (eq (class ?Y21) Has) (subclassp (class ?Y21) Has))&:(or (eq (class (send ?Y21 get-what)) Permission) (subclassp (class (send ?Y21 get-what)) Permission))) (time ?X5&:(or (eq (class ?X5) Duration) (subclassp (class ?X5) Duration))) (truth 1))) (test (or (and (>= (send ?X3 get-start) (send ?X3 get-start)) (or (<= (send ?X3 get-start) (send ?X3 get-end)) (= (send ?X3 get-end) -1))) (and (>= (send ?X3 get-start) (send ?X3 get-start)) (or (<= (send ?X3 get-start) (send ?X3 get-end)) (= (send ?X3 get-end) -1))))) => (add-prop ?X1 (add-pred Has what (send ?Y21 get-what)) (make-instance of Duration (start (mincomstart ?X3 ?X5)) (end (maxcomend ?X3 ?X5))) 1))
(defrule 3dd5a9ffad494cb99f41e0f7c966c192 (logical (object (is-a Person) (name ?X1))) => (add-prop ?X1 (add-pred Has what [member]) (make-instance of Duration (start 733697.0) (end -1.0)) 1))
(defrule 4708888d39744999ab54efaa14d97cfe (logical (object (is-a Proposition) (subject ?X1&:(or (eq (class ?X1) Person) (subclassp (class ?X1) Person))) (predicate ?Y23&:(or (eq (class ?Y23) Create) (subclassp (class ?Y23) Create))&:(or (eq (class (send ?Y23 get-what)) Content) (subclassp (class (send ?Y23 get-what)) Content))) (time ?X3) (truth 1))) => (reduce-class (send ?Y23 get-what) Content) (add-prop ?X1 (add-pred IsOwner of (send ?Y23 get-what)) (make-instance of Duration (start ?X3) (end -1.0)) 1) (add-prop (send ?Y23 get-what) (add-pred Has what [private]) (make-instance of Duration (start ?X3) (end -1.0)) 1))
(defrule 60f9347f48fb432985532bd946bed1ce (logical (object (is-a Permission) (name ?X2))) => (add-prop [manager] (add-pred Has what ?X2) (make-instance of Duration (start 733697.0) (end -1.0)) 1))
(defrule 12a84ef3bbb44f6497a71e1aa3120228 (logical (object (is-a Proposition) (subject ?X1&:(or (eq (class ?X1) Content) (subclassp (class ?X1) Content))) (predicate ?Y24&:(or (eq (class ?Y24) Has) (subclassp (class ?Y24) Has))&:(eq (send ?Y24 get-what) [public])) (time ?X2&:(or (eq (class ?X2) Duration) (subclassp (class ?X2) Duration))) (truth 1))) => (add-prop [basic_perm] (add-pred IsNeeded for_action (add-pred View what ?X1)) ?X2 1))
(defrule 374c2b0ba08f49b1b1d2ecd144ed9336 (logical (object (is-a Proposition) (subject ?X1&:(or (eq (class ?X1) Content) (subclassp (class ?X1) Content))) (predicate ?Y26&:(or (eq (class ?Y26) Has) (subclassp (class ?Y26) Has))&:(eq (send ?Y26 get-what) [private])) (time ?X2&:(or (eq (class ?X2) Duration) (subclassp (class ?X2) Duration))) (truth 1))) => (add-prop [manage_perm] (add-pred IsNeeded for_action (add-pred View what ?X1)) ?X2 1))
(defrule a55c2ba22ee34373ab4f0e1600f95257 (logical (object (is-a Proposition) (subject ?X1&:(or (eq (class ?X1) Content) (subclassp (class ?X1) Content))) (predicate ?Y28&:(or (eq (class ?Y28) Has) (subclassp (class ?Y28) Has))&:(eq (send ?Y28 get-what) [private])) (time ?X3&:(or (eq (class ?X3) Duration) (subclassp (class ?X3) Duration))) (truth 1))) (logical (object (is-a Proposition) (subject ?X2&:(or (eq (class ?X2) Person) (subclassp (class ?X2) Person))) (predicate ?Y30&:(or (eq (class ?Y30) IsOwner) (subclassp (class ?Y30) IsOwner))&:(eq (send ?Y30 get-of) ?X1)) (time ?X4&:(or (eq (class ?X4) Duration) (subclassp (class ?X4) Duration))) (truth 1))) (test (or (and (>= (send ?X3 get-start) (send ?X3 get-start)) (or (<= (send ?X3 get-start) (send ?X3 get-end)) (= (send ?X3 get-end) -1))) (and (>= (send ?X3 get-start) (send ?X3 get-start)) (or (<= (send ?X3 get-start) (send ?X3 get-end)) (= (send ?X3 get-end) -1))))) => (add-prop ?X2 (add-pred Can what (add-pred View what ?X1)) (make-instance of Duration (start (mincomstart ?X3 ?X4)) (end (maxcomend ?X3 ?X4))) 1))
(defrule 2c43aa72d54b46238b703ebde5e38ae1 (logical (object (is-a Proposition) (subject ?X1&:(or (eq (class ?X1) Person) (subclassp (class ?X1) Person))) (predicate ?Y32&:(or (eq (class ?Y32) Publish) (subclassp (class ?Y32) Publish))&:(or (eq (class (send ?Y32 get-what)) Content) (subclassp (class (send ?Y32 get-what)) Content))) (time ?X3) (truth 1))) (logical (object (is-a Proposition) (subject ?X2&:(eq ?X2 (send ?Y32 get-what))) (predicate ?Y33&:(or (eq (class ?Y33) Has) (subclassp (class ?Y33) Has))&:(or (eq (class (send ?Y33 get-what)) Status) (subclassp (class (send ?Y33 get-what)) Status))) (time ?X5&:(or (eq (class ?X5) Duration) (subclassp (class ?X5) Duration))) (truth 1))) => (send ?X5 put-end 733697) (add-prop (send ?Y32 get-what) (add-pred Has what [public]) (make-instance of Duration (start ?X3) (end -1.0)) 1))
(defrule 3bee53278f1b4cf88323521bc2b84912 (logical (object (is-a Content) (name ?X1))) => (add-prop [manage_perm] (add-pred IsNeeded for_action (add-pred Publish what ?X1)) (make-instance of Duration (start 733697.0) (end -1.0)) 1))
(defrule 4dbb12e4ecfc423c9ba9628ec2991630 (logical (object (is-a Proposition) (subject ?X1&:(or (eq (class ?X1) Person) (subclassp (class ?X1) Person))) (predicate ?Y35&:(or (eq (class ?Y35) Hide) (subclassp (class ?Y35) Hide))&:(or (eq (class (send ?Y35 get-what)) Content) (subclassp (class (send ?Y35 get-what)) Content))) (time ?X3) (truth 1))) (logical (object (is-a Proposition) (subject ?X2&:(eq ?X2 (send ?Y35 get-what))) (predicate ?Y36&:(or (eq (class ?Y36) Has) (subclassp (class ?Y36) Has))&:(or (eq (class (send ?Y36 get-what)) Status) (subclassp (class (send ?Y36 get-what)) Status))) (time ?X5&:(or (eq (class ?X5) Duration) (subclassp (class ?X5) Duration))) (truth 1))) => (send ?X5 put-end 733697) (add-prop (send ?Y35 get-what) (add-pred Has what [private]) (make-instance of Duration (start ?X3) (end -1.0)) 1))
(defrule 8079b34cd2244fb4bba7a13c96449aa1 (logical (object (is-a Proposition) (subject ?X1&:(or (eq (class ?X1) Person) (subclassp (class ?X1) Person))) (predicate ?Y38&:(or (eq (class ?Y38) IsOwner) (subclassp (class ?Y38) IsOwner))&:(or (eq (class (send ?Y38 get-of)) Content) (subclassp (class (send ?Y38 get-of)) Content))) (time ?X3&:(or (eq (class ?X3) Duration) (subclassp (class ?X3) Duration))) (truth 1))) => (add-prop ?X1 (add-pred Can what (add-pred Hide what (send ?Y38 get-of))) ?X3 1))
(reduce-class [john] Person)
john
(reduce-class [pete] Person)
pete
(reduce-class [jane] Person)
jane
(reduce-class [c1] Content)
c1
(reduce-class [c2] Content)
c2
(add-prop [john] (add-pred Has what [manager]) (make-instance of Duration (start 733697.0) (end -1.0)) 1)
john has what manager at from 733697.0 till -1.0
(add-prop [jane] (add-pred Has what [create_perm]) (make-instance of Duration (start 733697.0) (end -1.0)) 1)
jane has what create_perm at from 733697.0 till -1.0
(add-prop [jane] (add-pred Wants to (add-pred Create what [c1])) 733697.0 1)
jane wants to create what c1 at 733697.0
(add-prop [pete] (add-pred Wants to (add-pred Create what [c2])) 733697.0 1)
pete wants to create what c2 at 733697.0
(add-prop [jane] (add-pred Wants to (add-pred Publish what [c1])) 733697.0 1)
jane wants to publish what c1 at 733697.0
(add-prop [pete] (add-pred Wants to (add-pred Publish what [c2])) 733697.0 1)
pete wants to publish what c2 at 733697.0
(add-prop [john] (add-pred Wants to (add-pred Publish what [c1])) 733697.0 1)
john wants to publish what c1 at 733697.0
----------running---------------------
jane create what c1 at 733697.0
jane isowner of c1 at from 733697.0 till -1.0
c1 has what private at from 733697.0 till -1.0
jane can what view what c1 at from 733697.0 till -1.0
manage_perm isneeded for_action view what c1 at from 733697.0 till -1.0
jane can what hide what c1 at from 733697.0 till -1.0
manage_perm isneeded for_action publish what c2 at from 733697.0 till -1.0
manage_perm isneeded for_action publish what c1 at from 733697.0 till -1.0
jane has what member at from 733697.0 till -1.0
jane has what basic_perm at from 733697.0 till -1.0
pete has what member at from 733697.0 till -1.0
pete has what basic_perm at from 733697.0 till -1.0
john has what member at from 733697.0 till -1.0
john has what basic_perm at from 733697.0 till -1.0
manager has what create_perm at from 733697.0 till -1.0
admin has what create_perm at from 733697.0 till -1.0
john has what create_perm at from 733697.0 till -1.0
manager has what manage_perm at from 733697.0 till -1.0
manager can what view what c1 at from 733697.0 till -1.0
manager can what publish what c2 at from 733697.0 till -1.0
manager can what publish what c1 at from 733697.0 till -1.0
admin has what manage_perm at from 733697.0 till -1.0
admin can what view what c1 at from 733697.0 till -1.0
admin can what publish what c2 at from 733697.0 till -1.0
admin can what publish what c1 at from 733697.0 till -1.0
john has what manage_perm at from 733697.0 till -1.0
john can what view what c1 at from 733697.0 till -1.0
john can what publish what c2 at from 733697.0 till -1.0
john can what publish what c1 at from 733697.0 till -1.0
john publish what c1 at 733697.0
c1 has what public at from 733697.0 till -1.0
basic_perm isneeded for_action view what c1 at from 733697.0 till -1.0
pete can what view what c1 at from 733697.0 till -1.0
member can what view what c1 at from 733697.0 till -1.0
manager has what basic_perm at from 733697.0 till -1.0
manager can what view what c1 at from 733697.0 till 733697.0
----------runned---------------------
(find-all-instances ((?prop Proposition) (?Y40 Has) (?Y41 Duration)) (and (eq ?prop:subject [c1]) (eq ?Y40:what [private]) (eq ?prop:predicate ?Y40) (= ?Y41:start 733697.0) (= ?Y41:end -1.0) (eq ?prop:truth 1)))
0


no
(defclass Name (is-a USER))
(deffunction reduce-class (?instance ?class) (if (eq (length$ (find-instance ((?a ?class)) (eq (instance-name ?a) ?instance))) 0) then (make-instance ?instance of ?class) (python-call tonl ?class ?instance)))
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


(deffunction mincomstart (?dur1 ?dur2)
    (return (max (send ?dur1 get-start) (send ?dur2 get-start)))
)



(deffunction maxcomend (?dur1 ?dur2)
    (bind ?e1 (send ?dur1 get-end))
    (bind ?e2 (send ?dur2 get-end))
    (if (= ?e1 ?e2) then (return ?e1))
    (if (= ?e2 -1) then
        (return ?e1)
    )
    (if (= ?e1 -1) then
        (return ?e2)
    )
    (return (min ?e1 ?e2))
)

(defclass Proposition (is-a Name) (slot truth (type INTEGER) (default 1) (pattern-match reactive)) (slot subject (type INSTANCE) (pattern-match reactive)) (slot predicate (type INSTANCE) (pattern-match reactive)) (slot time (type ?VARIABLE) (pattern-match reactive)))

(deffunction add-prop (?s ?p ?t ?r)
       (if (and (python-call ptonl ?s ?p ?t ?r)
                (= (+ (length$ (find-instance ((?prop Proposition) (?dur Duration))
                          (and (eq ?prop:subject ?s)
                               (eq ?prop:predicate ?p)
                               (eq ?prop:time ?dur)
                               (= ?dur:start (send ?t get-start))
                               (= ?dur:end (send ?t get-end))
                               (eq ?prop:truth ?r))))
                      (length$ (find-instance ((?prop Proposition))
                          (and (eq ?prop:subject ?s)
                               (eq ?prop:predicate ?p)
                               (= ?prop:time ?t)
                               (eq ?prop:truth ?r)))))
                 0))
        then (make-instance of Proposition (subject ?s)
                                           (predicate ?p)
                                           (time ?t)
                                           (truth ?r))
        else (return TRUE)))
------------OPEN---------------------
----------F-OPEN---------------------
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
admin
(reduce-class [member] Role)
member
(reduce-class [manager] Role)
manager
(reduce-class [basic_perm] Permission)
basic_perm
(reduce-class [manage_perm] Permission)
manage_perm
(reduce-class [create_perm] Permission)
create_perm
(reduce-class [public] Status)
public
(reduce-class [private] Status)
private
(add-prop [admin] (add-pred Has what [manager]) (make-instance of Duration (start 733697.0) (end -1.0)) 1)
admin has what manager at from 733697.0 till -1.0
(add-prop [member] (add-pred Has what [basic_perm]) (make-instance of Duration (start 733697.0) (end -1.0)) 1)
member has what basic_perm at from 733697.0 till -1.0
(defrule 1feb97585c7146cdb307e2e16772472a (logical (object (is-a Proposition) (subject ?X1&:(or (eq (class ?X1) Person) (subclassp (class ?X1) Person))) (predicate ?Y1&:(or (eq (class ?Y1) Wants) (subclassp (class ?Y1) Wants))&:(or (eq (class (send ?Y1 get-to)) Create) (subclassp (class (send ?Y1 get-to)) Create))&:(or (eq (class (send (send ?Y1 get-to) get-what)) Thing) (subclassp (class (send (send ?Y1 get-to) get-what)) Thing))) (time ?X2) (truth 1))) (logical (object (is-a Proposition) (subject ?X1) (predicate ?Y2&:(or (eq (class ?Y2) Has) (subclassp (class ?Y2) Has))&:(eq (send ?Y2 get-what) [create_perm])) (time ?X3&:(or (eq (class ?X3) Duration) (subclassp (class ?X3) Duration))) (truth 1))) (test (and (<= (send ?X3 get-start) ?X2) (or (= (send ?X3 get-end) -1) (>= (send ?X3 get-end) ?X2)))) => (add-prop ?X1 (add-pred Create what (send (send ?Y1 get-to) get-what)) ?X2 1))
(defrule bca013a0c3dd4775bb06d521359bb87e (logical (object (is-a Proposition) (subject ?X1&:(or (eq (class ?X1) Person) (subclassp (class ?X1) Person))) (predicate ?Y4&:(or (eq (class ?Y4) Wants) (subclassp (class ?Y4) Wants))) (time ?X2) (truth 1))) (logical (object (is-a Proposition) (subject ?X1) (predicate ?Y5&:(or (eq (class ?Y5) Can) (subclassp (class ?Y5) Can))&:(eq (send ?Y4 get-to) (send ?Y5 get-what))) (time ?X3&:(or (eq (class ?X3) Duration) (subclassp (class ?X3) Duration))) (truth 1))) (test (and (<= (send ?X3 get-start) ?X2) (or (= (send ?X3 get-end) -1) (>= (send ?X3 get-end) ?X2)))) => (add-prop ?X1 (send ?Y4 get-to) ?X2 1))
(defrule b7f2d2d67e614f158abfaf77402324a9 (logical (object (is-a Proposition) (subject ?X2&:(or (eq (class ?X2) Thing) (subclassp (class ?X2) Thing))) (predicate ?Y7&:(or (eq (class ?Y7) IsNeeded) (subclassp (class ?Y7) IsNeeded))) (time ?X3&:(or (eq (class ?X3) Duration) (subclassp (class ?X3) Duration))) (truth 1))) (logical (object (is-a Proposition) (subject ?X1&:(or (eq (class ?X1) Thing) (subclassp (class ?X1) Thing))) (predicate ?Y9&:(or (eq (class ?Y9) Has) (subclassp (class ?Y9) Has))&:(eq (send ?Y9 get-what) ?X2)) (time ?X5&:(or (eq (class ?X5) Duration) (subclassp (class ?X5) Duration))) (truth 1))) (test (or (and (>= (send ?X3 get-start) (send ?X3 get-start)) (or (<= (send ?X3 get-start) (send ?X3 get-end)) (= (send ?X3 get-end) -1))) (and (>= (send ?X3 get-start) (send ?X3 get-start)) (or (<= (send ?X3 get-start) (send ?X3 get-end)) (= (send ?X3 get-end) -1))))) => (add-prop ?X1 (add-pred Can what (send ?Y7 get-for_action)) (make-instance of Duration (start (mincomstart ?X3 ?X5)) (end (maxcomend ?X3 ?X5))) 1))
(defrule 1aac5dfeebb546b8a9a8f9f441c3e47b (logical (object (is-a Proposition) (subject ?X1&:(or (eq (class ?X1) Thing) (subclassp (class ?X1) Thing))) (predicate ?Y11&:(or (eq (class ?Y11) IsIn) (subclassp (class ?Y11) IsIn))&:(or (eq (class (send ?Y11 get-what)) Thing) (subclassp (class (send ?Y11 get-what)) Thing))) (time ?X4&:(or (eq (class ?X4) Duration) (subclassp (class ?X4) Duration))) (truth 1))) (logical (object (is-a Proposition) (subject ?X2&:(eq ?X2 (send ?Y11 get-what))) (predicate ?Y13&:(or (eq (class ?Y13) IsIn) (subclassp (class ?Y13) IsIn))&:(or (eq (class (send ?Y13 get-what)) Thing) (subclassp (class (send ?Y13 get-what)) Thing))) (time ?X5&:(or (eq (class ?X5) Duration) (subclassp (class ?X5) Duration))) (truth 1))) (test (or (and (>= (send ?X4 get-start) (send ?X4 get-start)) (or (<= (send ?X4 get-start) (send ?X4 get-end)) (= (send ?X4 get-end) -1))) (and (>= (send ?X4 get-start) (send ?X4 get-start)) (or (<= (send ?X4 get-start) (send ?X4 get-end)) (= (send ?X4 get-end) -1))))) => (add-prop ?X1 (add-pred IsIn what (send ?Y13 get-what)) (make-instance of Duration (start (mincomstart ?X4 ?X5)) (end (maxcomend ?X4 ?X5))) 1))
(defrule c6237b92d36746a8a3fac34d0a25133e (logical (object (is-a Proposition) (subject ?X1&:(or (eq (class ?X1) Person) (subclassp (class ?X1) Person))) (predicate ?Y15&:(or (eq (class ?Y15) IsIn) (subclassp (class ?Y15) IsIn))) (time ?X3&:(or (eq (class ?X3) Duration) (subclassp (class ?X3) Duration))) (truth 1))) (logical (object (is-a Proposition) (subject ?X2&:(or (eq (class ?X2) Group) (subclassp (class ?X2) Group))) (predicate ?Y17&:(or (eq (class ?Y17) Has) (subclassp (class ?Y17) Has))&:(or (eq (class (send ?Y17 get-what)) Permission) (subclassp (class (send ?Y17 get-what)) Permission))) (time ?X5&:(or (eq (class ?X5) Duration) (subclassp (class ?X5) Duration))) (truth 1))) (test (or (and (>= (send ?X3 get-start) (send ?X3 get-start)) (or (<= (send ?X3 get-start) (send ?X3 get-end)) (= (send ?X3 get-end) -1))) (and (>= (send ?X3 get-start) (send ?X3 get-start)) (or (<= (send ?X3 get-start) (send ?X3 get-end)) (= (send ?X3 get-end) -1))))) => (add-prop ?X1 (add-pred Has what (send ?Y17 get-what)) (make-instance of Duration (start (mincomstart ?X3 ?X5)) (end (maxcomend ?X3 ?X5))) 1))
(defrule c329caf1463a431cafc0da36511f9369 (logical (object (is-a Proposition) (subject ?X1&:(or (eq (class ?X1) Person) (subclassp (class ?X1) Person))) (predicate ?Y19&:(or (eq (class ?Y19) Has) (subclassp (class ?Y19) Has))&:(or (eq (class (send ?Y19 get-what)) Role) (subclassp (class (send ?Y19 get-what)) Role))) (time ?X3&:(or (eq (class ?X3) Duration) (subclassp (class ?X3) Duration))) (truth 1))) (logical (object (is-a Proposition) (subject ?X2&:(eq ?X2 (send ?Y19 get-what))) (predicate ?Y21&:(or (eq (class ?Y21) Has) (subclassp (class ?Y21) Has))&:(or (eq (class (send ?Y21 get-what)) Permission) (subclassp (class (send ?Y21 get-what)) Permission))) (time ?X5&:(or (eq (class ?X5) Duration) (subclassp (class ?X5) Duration))) (truth 1))) (test (or (and (>= (send ?X3 get-start) (send ?X3 get-start)) (or (<= (send ?X3 get-start) (send ?X3 get-end)) (= (send ?X3 get-end) -1))) (and (>= (send ?X3 get-start) (send ?X3 get-start)) (or (<= (send ?X3 get-start) (send ?X3 get-end)) (= (send ?X3 get-end) -1))))) => (add-prop ?X1 (add-pred Has what (send ?Y21 get-what)) (make-instance of Duration (start (mincomstart ?X3 ?X5)) (end (maxcomend ?X3 ?X5))) 1))
(defrule cca9a68d12bc429486ca8b8d5fb8296a (logical (object (is-a Person) (name ?X1))) => (add-prop ?X1 (add-pred Has what [member]) (make-instance of Duration (start 733697.0) (end -1.0)) 1))
(defrule 269c6a60008147638f509dcb22aa5313 (logical (object (is-a Proposition) (subject ?X1&:(or (eq (class ?X1) Person) (subclassp (class ?X1) Person))) (predicate ?Y23&:(or (eq (class ?Y23) Create) (subclassp (class ?Y23) Create))&:(or (eq (class (send ?Y23 get-what)) Content) (subclassp (class (send ?Y23 get-what)) Content))) (time ?X3) (truth 1))) => (reduce-class (send ?Y23 get-what) Content) (add-prop ?X1 (add-pred IsOwner of (send ?Y23 get-what)) (make-instance of Duration (start ?X3) (end -1.0)) 1) (add-prop (send ?Y23 get-what) (add-pred Has what [private]) (make-instance of Duration (start ?X3) (end -1.0)) 1))
(defrule 7c914696be2c42a69cebfd7e8e4c6fd1 (logical (object (is-a Permission) (name ?X2))) => (add-prop [manager] (add-pred Has what ?X2) (make-instance of Duration (start 733697.0) (end -1.0)) 1))
(defrule a6274859146b40d998c356d0da84ce7c (logical (object (is-a Proposition) (subject ?X1&:(or (eq (class ?X1) Content) (subclassp (class ?X1) Content))) (predicate ?Y24&:(or (eq (class ?Y24) Has) (subclassp (class ?Y24) Has))&:(eq (send ?Y24 get-what) [public])) (time ?X2&:(or (eq (class ?X2) Duration) (subclassp (class ?X2) Duration))) (truth 1))) => (add-prop [basic_perm] (add-pred IsNeeded for_action (add-pred View what ?X1)) ?X2 1))
(defrule fe1a77dcc420471cbc36f796505ac2f8 (logical (object (is-a Proposition) (subject ?X1&:(or (eq (class ?X1) Content) (subclassp (class ?X1) Content))) (predicate ?Y26&:(or (eq (class ?Y26) Has) (subclassp (class ?Y26) Has))&:(eq (send ?Y26 get-what) [private])) (time ?X2&:(or (eq (class ?X2) Duration) (subclassp (class ?X2) Duration))) (truth 1))) => (add-prop [manage_perm] (add-pred IsNeeded for_action (add-pred View what ?X1)) ?X2 1))
(defrule d49af54c01fe4a14b44a3d68fae8793c (logical (object (is-a Proposition) (subject ?X1&:(or (eq (class ?X1) Content) (subclassp (class ?X1) Content))) (predicate ?Y28&:(or (eq (class ?Y28) Has) (subclassp (class ?Y28) Has))&:(eq (send ?Y28 get-what) [private])) (time ?X3&:(or (eq (class ?X3) Duration) (subclassp (class ?X3) Duration))) (truth 1))) (logical (object (is-a Proposition) (subject ?X2&:(or (eq (class ?X2) Person) (subclassp (class ?X2) Person))) (predicate ?Y30&:(or (eq (class ?Y30) IsOwner) (subclassp (class ?Y30) IsOwner))&:(eq (send ?Y30 get-of) ?X1)) (time ?X4&:(or (eq (class ?X4) Duration) (subclassp (class ?X4) Duration))) (truth 1))) (test (or (and (>= (send ?X3 get-start) (send ?X3 get-start)) (or (<= (send ?X3 get-start) (send ?X3 get-end)) (= (send ?X3 get-end) -1))) (and (>= (send ?X3 get-start) (send ?X3 get-start)) (or (<= (send ?X3 get-start) (send ?X3 get-end)) (= (send ?X3 get-end) -1))))) => (add-prop ?X2 (add-pred Can what (add-pred View what ?X1)) (make-instance of Duration (start (mincomstart ?X3 ?X4)) (end (maxcomend ?X3 ?X4))) 1))
(defrule 1e206e2ef713436c87a5ae671291a998 (logical (object (is-a Proposition) (subject ?X1&:(or (eq (class ?X1) Person) (subclassp (class ?X1) Person))) (predicate ?Y32&:(or (eq (class ?Y32) Publish) (subclassp (class ?Y32) Publish))&:(or (eq (class (send ?Y32 get-what)) Content) (subclassp (class (send ?Y32 get-what)) Content))) (time ?X3) (truth 1))) (logical (object (is-a Proposition) (subject ?X2&:(eq ?X2 (send ?Y32 get-what))) (predicate ?Y33&:(or (eq (class ?Y33) Has) (subclassp (class ?Y33) Has))&:(or (eq (class (send ?Y33 get-what)) Status) (subclassp (class (send ?Y33 get-what)) Status))) (time ?X5&:(or (eq (class ?X5) Duration) (subclassp (class ?X5) Duration))) (truth 1))) => (send ?X5 put-end 733697) (add-prop (send ?Y32 get-what) (add-pred Has what [public]) (make-instance of Duration (start ?X3) (end -1.0)) 1))
(defrule 539aba36a926494eb52e469a20e13581 (logical (object (is-a Content) (name ?X1))) => (add-prop [manage_perm] (add-pred IsNeeded for_action (add-pred Publish what ?X1)) (make-instance of Duration (start 733697.0) (end -1.0)) 1))
(defrule 2696913080e14974a8b3c02be6eacb38 (logical (object (is-a Proposition) (subject ?X1&:(or (eq (class ?X1) Person) (subclassp (class ?X1) Person))) (predicate ?Y35&:(or (eq (class ?Y35) Hide) (subclassp (class ?Y35) Hide))&:(or (eq (class (send ?Y35 get-what)) Content) (subclassp (class (send ?Y35 get-what)) Content))) (time ?X3) (truth 1))) (logical (object (is-a Proposition) (subject ?X2&:(eq ?X2 (send ?Y35 get-what))) (predicate ?Y36&:(or (eq (class ?Y36) Has) (subclassp (class ?Y36) Has))&:(or (eq (class (send ?Y36 get-what)) Status) (subclassp (class (send ?Y36 get-what)) Status))) (time ?X5&:(or (eq (class ?X5) Duration) (subclassp (class ?X5) Duration))) (truth 1))) => (send ?X5 put-end 733697) (add-prop (send ?Y35 get-what) (add-pred Has what [private]) (make-instance of Duration (start ?X3) (end -1.0)) 1))
(defrule 8cce6343d00a453d8fdea5f11719f6a0 (logical (object (is-a Proposition) (subject ?X1&:(or (eq (class ?X1) Person) (subclassp (class ?X1) Person))) (predicate ?Y38&:(or (eq (class ?Y38) IsOwner) (subclassp (class ?Y38) IsOwner))&:(or (eq (class (send ?Y38 get-of)) Content) (subclassp (class (send ?Y38 get-of)) Content))) (time ?X3&:(or (eq (class ?X3) Duration) (subclassp (class ?X3) Duration))) (truth 1))) => (add-prop ?X1 (add-pred Can what (add-pred Hide what (send ?Y38 get-of))) ?X3 1))
(reduce-class [john] Person)
john
(reduce-class [pete] Person)
pete
(reduce-class [jane] Person)
jane
(reduce-class [c1] Content)
c1
(reduce-class [c2] Content)
c2
(add-prop [john] (add-pred Has what [manager]) (make-instance of Duration (start 733697.0) (end -1.0)) 1)
john has what manager at from 733697.0 till -1.0
(add-prop [jane] (add-pred Has what [create_perm]) (make-instance of Duration (start 733697.0) (end -1.0)) 1)
jane has what create_perm at from 733697.0 till -1.0
(add-prop [jane] (add-pred Wants to (add-pred Create what [c1])) 733697.0 1)
jane wants to create what c1 at 733697.0
(add-prop [pete] (add-pred Wants to (add-pred Create what [c2])) 733697.0 1)
pete wants to create what c2 at 733697.0
(add-prop [jane] (add-pred Wants to (add-pred Publish what [c1])) 733697.0 1)
jane wants to publish what c1 at 733697.0
(add-prop [pete] (add-pred Wants to (add-pred Publish what [c2])) 733697.0 1)
pete wants to publish what c2 at 733697.0
(add-prop [john] (add-pred Wants to (add-pred Publish what [c1])) 733697.0 1)
john wants to publish what c1 at 733697.0
----------running---------------------
jane create what c1 at 733697.0
jane isowner of c1 at from 733697.0 till -1.0
c1 has what private at from 733697.0 till -1.0
jane can what view what c1 at from 733697.0 till -1.0
manage_perm isneeded for_action view what c1 at from 733697.0 till -1.0
jane can what hide what c1 at from 733697.0 till -1.0
manage_perm isneeded for_action publish what c2 at from 733697.0 till -1.0
manage_perm isneeded for_action publish what c1 at from 733697.0 till -1.0
jane has what member at from 733697.0 till -1.0
jane has what basic_perm at from 733697.0 till -1.0
pete has what member at from 733697.0 till -1.0
pete has what basic_perm at from 733697.0 till -1.0
john has what member at from 733697.0 till -1.0
john has what basic_perm at from 733697.0 till -1.0
manager has what create_perm at from 733697.0 till -1.0
admin has what create_perm at from 733697.0 till -1.0
john has what create_perm at from 733697.0 till -1.0
manager has what manage_perm at from 733697.0 till -1.0
manager can what view what c1 at from 733697.0 till -1.0
manager can what publish what c2 at from 733697.0 till -1.0
manager can what publish what c1 at from 733697.0 till -1.0
admin has what manage_perm at from 733697.0 till -1.0
admin can what view what c1 at from 733697.0 till -1.0
admin can what publish what c2 at from 733697.0 till -1.0
admin can what publish what c1 at from 733697.0 till -1.0
john has what manage_perm at from 733697.0 till -1.0
john can what view what c1 at from 733697.0 till -1.0
john can what publish what c2 at from 733697.0 till -1.0
john can what publish what c1 at from 733697.0 till -1.0
john publish what c1 at 733697.0
c1 has what public at from 733697.0 till -1.0
basic_perm isneeded for_action view what c1 at from 733697.0 till -1.0
pete can what view what c1 at from 733697.0 till -1.0
member can what view what c1 at from 733697.0 till -1.0
manager has what basic_perm at from 733697.0 till -1.0
manager can what view what c1 at from 733697.0 till 733697.0
----------runned---------------------
(find-all-instances ((?prop Proposition) (?Y40 Has) (?Y41 Duration)) (and (eq ?prop:subject [c1]) (eq ?Y40:what [private]) (eq ?prop:predicate ?Y40) (= ?Y41:start 733697.0) (= ?Y41:end -1.0) (eq ?prop:truth 1)))
0


no
(defclass Name (is-a USER))
(deffunction reduce-class (?instance ?class) (if (eq (length$ (find-instance ((?a ?class)) (eq (instance-name ?a) ?instance))) 0) then (make-instance ?instance of ?class) (python-call tonl ?class ?instance)))
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


(deffunction mincomstart (?dur1 ?dur2)
    (return (max (send ?dur1 get-start) (send ?dur2 get-start)))
)



(deffunction maxcomend (?dur1 ?dur2)
    (bind ?e1 (send ?dur1 get-end))
    (bind ?e2 (send ?dur2 get-end))
    (if (= ?e1 ?e2) then (return ?e1))
    (if (= ?e2 -1) then
        (return ?e1)
    )
    (if (= ?e1 -1) then
        (return ?e2)
    )
    (return (min ?e1 ?e2))
)

(defclass Proposition (is-a Name) (slot truth (type INTEGER) (default 1) (pattern-match reactive)) (slot subject (type INSTANCE) (pattern-match reactive)) (slot predicate (type INSTANCE) (pattern-match reactive)) (slot time (type ?VARIABLE) (pattern-match reactive)))

(deffunction add-prop (?s ?p ?t ?r)
       (if (and (python-call ptonl ?s ?p ?t ?r)
                (= (+ (length$ (find-instance ((?prop Proposition) (?dur Duration))
                          (and (eq ?prop:subject ?s)
                               (eq ?prop:predicate ?p)
                               (eq ?prop:time ?dur)
                               (= ?dur:start (send ?t get-start))
                               (= ?dur:end (send ?t get-end))
                               (eq ?prop:truth ?r))))
                      (length$ (find-instance ((?prop Proposition))
                          (and (eq ?prop:subject ?s)
                               (eq ?prop:predicate ?p)
                               (= ?prop:time ?t)
                               (eq ?prop:truth ?r)))))
                 0))
        then (make-instance of Proposition (subject ?s)
                                           (predicate ?p)
                                           (time ?t)
                                           (truth ?r))
        else (return TRUE)))
------------OPEN---------------------
----------F-OPEN---------------------
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
admin
(reduce-class [member] Role)
member
(reduce-class [manager] Role)
manager
(reduce-class [basic_perm] Permission)
basic_perm
(reduce-class [manage_perm] Permission)
manage_perm
(reduce-class [create_perm] Permission)
create_perm
(reduce-class [public] Status)
public
(reduce-class [private] Status)
private
(add-prop [admin] (add-pred Has what [manager]) (make-instance of Duration (start 733697.0) (end -1.0)) 1)
admin has what manager at from 733697.0 till -1.0
(add-prop [member] (add-pred Has what [basic_perm]) (make-instance of Duration (start 733697.0) (end -1.0)) 1)
member has what basic_perm at from 733697.0 till -1.0
(defrule 901032cc1cc84307817743504e40c2e2 (logical (object (is-a Proposition) (subject ?X1&:(or (eq (class ?X1) Person) (subclassp (class ?X1) Person))) (predicate ?Y1&:(or (eq (class ?Y1) Wants) (subclassp (class ?Y1) Wants))&:(or (eq (class (send ?Y1 get-to)) Create) (subclassp (class (send ?Y1 get-to)) Create))&:(or (eq (class (send (send ?Y1 get-to) get-what)) Thing) (subclassp (class (send (send ?Y1 get-to) get-what)) Thing))) (time ?X2) (truth 1))) (logical (object (is-a Proposition) (subject ?X1) (predicate ?Y2&:(or (eq (class ?Y2) Has) (subclassp (class ?Y2) Has))&:(eq (send ?Y2 get-what) [create_perm])) (time ?X3&:(or (eq (class ?X3) Duration) (subclassp (class ?X3) Duration))) (truth 1))) (test (and (<= (send ?X3 get-start) ?X2) (or (= (send ?X3 get-end) -1) (>= (send ?X3 get-end) ?X2)))) => (add-prop ?X1 (add-pred Create what (send (send ?Y1 get-to) get-what)) ?X2 1))
(defrule 8621da07d9a34c28ad0b40dc917041b3 (logical (object (is-a Proposition) (subject ?X1&:(or (eq (class ?X1) Person) (subclassp (class ?X1) Person))) (predicate ?Y4&:(or (eq (class ?Y4) Wants) (subclassp (class ?Y4) Wants))) (time ?X2) (truth 1))) (logical (object (is-a Proposition) (subject ?X1) (predicate ?Y5&:(or (eq (class ?Y5) Can) (subclassp (class ?Y5) Can))&:(eq (send ?Y4 get-to) (send ?Y5 get-what))) (time ?X3&:(or (eq (class ?X3) Duration) (subclassp (class ?X3) Duration))) (truth 1))) (test (and (<= (send ?X3 get-start) ?X2) (or (= (send ?X3 get-end) -1) (>= (send ?X3 get-end) ?X2)))) => (add-prop ?X1 (send ?Y4 get-to) ?X2 1))
(defrule 8a24746ddefa4424a6e459d924a8b34e (logical (object (is-a Proposition) (subject ?X2&:(or (eq (class ?X2) Thing) (subclassp (class ?X2) Thing))) (predicate ?Y7&:(or (eq (class ?Y7) IsNeeded) (subclassp (class ?Y7) IsNeeded))) (time ?X3&:(or (eq (class ?X3) Duration) (subclassp (class ?X3) Duration))) (truth 1))) (logical (object (is-a Proposition) (subject ?X1&:(or (eq (class ?X1) Thing) (subclassp (class ?X1) Thing))) (predicate ?Y9&:(or (eq (class ?Y9) Has) (subclassp (class ?Y9) Has))&:(eq (send ?Y9 get-what) ?X2)) (time ?X5&:(or (eq (class ?X5) Duration) (subclassp (class ?X5) Duration))) (truth 1))) (test (or (and (>= (send ?X3 get-start) (send ?X3 get-start)) (or (<= (send ?X3 get-start) (send ?X3 get-end)) (= (send ?X3 get-end) -1))) (and (>= (send ?X3 get-start) (send ?X3 get-start)) (or (<= (send ?X3 get-start) (send ?X3 get-end)) (= (send ?X3 get-end) -1))))) => (add-prop ?X1 (add-pred Can what (send ?Y7 get-for_action)) (make-instance of Duration (start (mincomstart ?X3 ?X5)) (end (maxcomend ?X3 ?X5))) 1))
(defrule 512f201c210c4b57ada2a19d48f950af (logical (object (is-a Proposition) (subject ?X1&:(or (eq (class ?X1) Thing) (subclassp (class ?X1) Thing))) (predicate ?Y11&:(or (eq (class ?Y11) IsIn) (subclassp (class ?Y11) IsIn))&:(or (eq (class (send ?Y11 get-what)) Thing) (subclassp (class (send ?Y11 get-what)) Thing))) (time ?X4&:(or (eq (class ?X4) Duration) (subclassp (class ?X4) Duration))) (truth 1))) (logical (object (is-a Proposition) (subject ?X2&:(eq ?X2 (send ?Y11 get-what))) (predicate ?Y13&:(or (eq (class ?Y13) IsIn) (subclassp (class ?Y13) IsIn))&:(or (eq (class (send ?Y13 get-what)) Thing) (subclassp (class (send ?Y13 get-what)) Thing))) (time ?X5&:(or (eq (class ?X5) Duration) (subclassp (class ?X5) Duration))) (truth 1))) (test (or (and (>= (send ?X4 get-start) (send ?X4 get-start)) (or (<= (send ?X4 get-start) (send ?X4 get-end)) (= (send ?X4 get-end) -1))) (and (>= (send ?X4 get-start) (send ?X4 get-start)) (or (<= (send ?X4 get-start) (send ?X4 get-end)) (= (send ?X4 get-end) -1))))) => (add-prop ?X1 (add-pred IsIn what (send ?Y13 get-what)) (make-instance of Duration (start (mincomstart ?X4 ?X5)) (end (maxcomend ?X4 ?X5))) 1))
(defrule 3e30daec5aad471ca1e8df99b73cf119 (logical (object (is-a Proposition) (subject ?X1&:(or (eq (class ?X1) Person) (subclassp (class ?X1) Person))) (predicate ?Y15&:(or (eq (class ?Y15) IsIn) (subclassp (class ?Y15) IsIn))) (time ?X3&:(or (eq (class ?X3) Duration) (subclassp (class ?X3) Duration))) (truth 1))) (logical (object (is-a Proposition) (subject ?X2&:(or (eq (class ?X2) Group) (subclassp (class ?X2) Group))) (predicate ?Y17&:(or (eq (class ?Y17) Has) (subclassp (class ?Y17) Has))&:(or (eq (class (send ?Y17 get-what)) Permission) (subclassp (class (send ?Y17 get-what)) Permission))) (time ?X5&:(or (eq (class ?X5) Duration) (subclassp (class ?X5) Duration))) (truth 1))) (test (or (and (>= (send ?X3 get-start) (send ?X3 get-start)) (or (<= (send ?X3 get-start) (send ?X3 get-end)) (= (send ?X3 get-end) -1))) (and (>= (send ?X3 get-start) (send ?X3 get-start)) (or (<= (send ?X3 get-start) (send ?X3 get-end)) (= (send ?X3 get-end) -1))))) => (add-prop ?X1 (add-pred Has what (send ?Y17 get-what)) (make-instance of Duration (start (mincomstart ?X3 ?X5)) (end (maxcomend ?X3 ?X5))) 1))
(defrule e6dbd31da1bd4f15a305f220f6f50530 (logical (object (is-a Proposition) (subject ?X1&:(or (eq (class ?X1) Person) (subclassp (class ?X1) Person))) (predicate ?Y19&:(or (eq (class ?Y19) Has) (subclassp (class ?Y19) Has))&:(or (eq (class (send ?Y19 get-what)) Role) (subclassp (class (send ?Y19 get-what)) Role))) (time ?X3&:(or (eq (class ?X3) Duration) (subclassp (class ?X3) Duration))) (truth 1))) (logical (object (is-a Proposition) (subject ?X2&:(eq ?X2 (send ?Y19 get-what))) (predicate ?Y21&:(or (eq (class ?Y21) Has) (subclassp (class ?Y21) Has))&:(or (eq (class (send ?Y21 get-what)) Permission) (subclassp (class (send ?Y21 get-what)) Permission))) (time ?X5&:(or (eq (class ?X5) Duration) (subclassp (class ?X5) Duration))) (truth 1))) (test (or (and (>= (send ?X3 get-start) (send ?X3 get-start)) (or (<= (send ?X3 get-start) (send ?X3 get-end)) (= (send ?X3 get-end) -1))) (and (>= (send ?X3 get-start) (send ?X3 get-start)) (or (<= (send ?X3 get-start) (send ?X3 get-end)) (= (send ?X3 get-end) -1))))) => (add-prop ?X1 (add-pred Has what (send ?Y21 get-what)) (make-instance of Duration (start (mincomstart ?X3 ?X5)) (end (maxcomend ?X3 ?X5))) 1))
(defrule 14ce2995141d49c28eb48f69309436a0 (logical (object (is-a Person) (name ?X1))) => (add-prop ?X1 (add-pred Has what [member]) (make-instance of Duration (start 733697.0) (end -1.0)) 1))
(defrule a5983caca41c42fc8e68249f513434f2 (logical (object (is-a Proposition) (subject ?X1&:(or (eq (class ?X1) Person) (subclassp (class ?X1) Person))) (predicate ?Y23&:(or (eq (class ?Y23) Create) (subclassp (class ?Y23) Create))&:(or (eq (class (send ?Y23 get-what)) Content) (subclassp (class (send ?Y23 get-what)) Content))) (time ?X3) (truth 1))) => (reduce-class (send ?Y23 get-what) Content) (add-prop ?X1 (add-pred IsOwner of (send ?Y23 get-what)) (make-instance of Duration (start ?X3) (end -1.0)) 1) (add-prop (send ?Y23 get-what) (add-pred Has what [private]) (make-instance of Duration (start ?X3) (end -1.0)) 1))
(defrule 89e34e9dc6e744b09d66ff2758a2b3bb (logical (object (is-a Permission) (name ?X2))) => (add-prop [manager] (add-pred Has what ?X2) (make-instance of Duration (start 733697.0) (end -1.0)) 1))
(defrule a6c5041b29d6424787bcc0638804b037 (logical (object (is-a Proposition) (subject ?X1&:(or (eq (class ?X1) Content) (subclassp (class ?X1) Content))) (predicate ?Y24&:(or (eq (class ?Y24) Has) (subclassp (class ?Y24) Has))&:(eq (send ?Y24 get-what) [public])) (time ?X2&:(or (eq (class ?X2) Duration) (subclassp (class ?X2) Duration))) (truth 1))) => (add-prop [basic_perm] (add-pred IsNeeded for_action (add-pred View what ?X1)) ?X2 1))
(defrule 001c4ed807a54b32b16d673d26243ab2 (logical (object (is-a Proposition) (subject ?X1&:(or (eq (class ?X1) Content) (subclassp (class ?X1) Content))) (predicate ?Y26&:(or (eq (class ?Y26) Has) (subclassp (class ?Y26) Has))&:(eq (send ?Y26 get-what) [private])) (time ?X2&:(or (eq (class ?X2) Duration) (subclassp (class ?X2) Duration))) (truth 1))) => (add-prop [manage_perm] (add-pred IsNeeded for_action (add-pred View what ?X1)) ?X2 1))
(defrule 3f601df2c2ad478d88b28abb66bd1453 (logical (object (is-a Proposition) (subject ?X1&:(or (eq (class ?X1) Content) (subclassp (class ?X1) Content))) (predicate ?Y28&:(or (eq (class ?Y28) Has) (subclassp (class ?Y28) Has))&:(eq (send ?Y28 get-what) [private])) (time ?X3&:(or (eq (class ?X3) Duration) (subclassp (class ?X3) Duration))) (truth 1))) (logical (object (is-a Proposition) (subject ?X2&:(or (eq (class ?X2) Person) (subclassp (class ?X2) Person))) (predicate ?Y30&:(or (eq (class ?Y30) IsOwner) (subclassp (class ?Y30) IsOwner))&:(eq (send ?Y30 get-of) ?X1)) (time ?X4&:(or (eq (class ?X4) Duration) (subclassp (class ?X4) Duration))) (truth 1))) (test (or (and (>= (send ?X3 get-start) (send ?X3 get-start)) (or (<= (send ?X3 get-start) (send ?X3 get-end)) (= (send ?X3 get-end) -1))) (and (>= (send ?X3 get-start) (send ?X3 get-start)) (or (<= (send ?X3 get-start) (send ?X3 get-end)) (= (send ?X3 get-end) -1))))) => (add-prop ?X2 (add-pred Can what (add-pred View what ?X1)) (make-instance of Duration (start (mincomstart ?X3 ?X4)) (end (maxcomend ?X3 ?X4))) 1))
(defrule e109be1307f942eda414ef24d494b72f (logical (object (is-a Proposition) (subject ?X1&:(or (eq (class ?X1) Person) (subclassp (class ?X1) Person))) (predicate ?Y32&:(or (eq (class ?Y32) Publish) (subclassp (class ?Y32) Publish))&:(or (eq (class (send ?Y32 get-what)) Content) (subclassp (class (send ?Y32 get-what)) Content))) (time ?X3) (truth 1))) (logical (object (is-a Proposition) (subject ?X2&:(eq ?X2 (send ?Y32 get-what))) (predicate ?Y33&:(or (eq (class ?Y33) Has) (subclassp (class ?Y33) Has))&:(or (eq (class (send ?Y33 get-what)) Status) (subclassp (class (send ?Y33 get-what)) Status))) (time ?X5&:(or (eq (class ?X5) Duration) (subclassp (class ?X5) Duration))) (truth 1))) => (send ?X5 put-end 733697) (add-prop (send ?Y32 get-what) (add-pred Has what [public]) (make-instance of Duration (start ?X3) (end -1.0)) 1))
(defrule 4e24984a3ead44a9919c3ef809aac1cf (logical (object (is-a Content) (name ?X1))) => (add-prop [manage_perm] (add-pred IsNeeded for_action (add-pred Publish what ?X1)) (make-instance of Duration (start 733697.0) (end -1.0)) 1))
(defrule ccf39781452b45a4a1aa697962c3add6 (logical (object (is-a Proposition) (subject ?X1&:(or (eq (class ?X1) Person) (subclassp (class ?X1) Person))) (predicate ?Y35&:(or (eq (class ?Y35) Hide) (subclassp (class ?Y35) Hide))&:(or (eq (class (send ?Y35 get-what)) Content) (subclassp (class (send ?Y35 get-what)) Content))) (time ?X3) (truth 1))) (logical (object (is-a Proposition) (subject ?X2&:(eq ?X2 (send ?Y35 get-what))) (predicate ?Y36&:(or (eq (class ?Y36) Has) (subclassp (class ?Y36) Has))&:(or (eq (class (send ?Y36 get-what)) Status) (subclassp (class (send ?Y36 get-what)) Status))) (time ?X5&:(or (eq (class ?X5) Duration) (subclassp (class ?X5) Duration))) (truth 1))) => (send ?X5 put-end 733697) (add-prop (send ?Y35 get-what) (add-pred Has what [private]) (make-instance of Duration (start ?X3) (end -1.0)) 1))
(defrule bc1fe653b66743028d05c2e232486815 (logical (object (is-a Proposition) (subject ?X1&:(or (eq (class ?X1) Person) (subclassp (class ?X1) Person))) (predicate ?Y38&:(or (eq (class ?Y38) IsOwner) (subclassp (class ?Y38) IsOwner))&:(or (eq (class (send ?Y38 get-of)) Content) (subclassp (class (send ?Y38 get-of)) Content))) (time ?X3&:(or (eq (class ?X3) Duration) (subclassp (class ?X3) Duration))) (truth 1))) => (add-prop ?X1 (add-pred Can what (add-pred Hide what (send ?Y38 get-of))) ?X3 1))
(reduce-class [john] Person)
john
(reduce-class [pete] Person)
pete
(reduce-class [jane] Person)
jane
(reduce-class [c1] Content)
c1
(reduce-class [c2] Content)
c2
(add-prop [john] (add-pred Has what [manager]) (make-instance of Duration (start 733697.0) (end -1.0)) 1)
john has what manager at from 733697.0 till -1.0
(add-prop [jane] (add-pred Has what [create_perm]) (make-instance of Duration (start 733697.0) (end -1.0)) 1)
jane has what create_perm at from 733697.0 till -1.0
(add-prop [jane] (add-pred Wants to (add-pred Create what [c1])) 733697.0 1)
jane wants to create what c1 at 733697.0
(add-prop [pete] (add-pred Wants to (add-pred Create what [c2])) 733697.0 1)
pete wants to create what c2 at 733697.0
(add-prop [jane] (add-pred Wants to (add-pred Publish what [c1])) 733697.0 1)
jane wants to publish what c1 at 733697.0
(add-prop [pete] (add-pred Wants to (add-pred Publish what [c2])) 733697.0 1)
pete wants to publish what c2 at 733697.0
(add-prop [john] (add-pred Wants to (add-pred Publish what [c1])) 733697.0 1)
john wants to publish what c1 at 733697.0
----------running---------------------
jane create what c1 at 733697.0
jane isowner of c1 at from 733697.0 till -1.0
c1 has what private at from 733697.0 till -1.0
jane can what view what c1 at from 733697.0 till -1.0
manage_perm isneeded for_action view what c1 at from 733697.0 till -1.0
jane can what hide what c1 at from 733697.0 till -1.0
manage_perm isneeded for_action publish what c2 at from 733697.0 till -1.0
manage_perm isneeded for_action publish what c1 at from 733697.0 till -1.0
jane has what member at from 733697.0 till -1.0
jane has what basic_perm at from 733697.0 till -1.0
pete has what member at from 733697.0 till -1.0
pete has what basic_perm at from 733697.0 till -1.0
john has what member at from 733697.0 till -1.0
john has what basic_perm at from 733697.0 till -1.0
manager has what create_perm at from 733697.0 till -1.0
admin has what create_perm at from 733697.0 till -1.0
john has what create_perm at from 733697.0 till -1.0
manager has what manage_perm at from 733697.0 till -1.0
manager can what view what c1 at from 733697.0 till -1.0
manager can what publish what c2 at from 733697.0 till -1.0
manager can what publish what c1 at from 733697.0 till -1.0
admin has what manage_perm at from 733697.0 till -1.0
admin can what view what c1 at from 733697.0 till -1.0
admin can what publish what c2 at from 733697.0 till -1.0
admin can what publish what c1 at from 733697.0 till -1.0
john has what manage_perm at from 733697.0 till -1.0
john can what view what c1 at from 733697.0 till -1.0
john can what publish what c2 at from 733697.0 till -1.0
john can what publish what c1 at from 733697.0 till -1.0
john publish what c1 at 733697.0
c1 has what public at from 733697.0 till -1.0
basic_perm isneeded for_action view what c1 at from 733697.0 till -1.0
pete can what view what c1 at from 733697.0 till -1.0
member can what view what c1 at from 733697.0 till -1.0
manager has what basic_perm at from 733697.0 till -1.0
manager can what view what c1 at from 733697.0 till 733697.0
----------runned: 38---------------------
(find-all-instances ((?prop Proposition) (?Y40 Has) (?Y41 Duration)) (and (eq ?prop:subject [c1]) (eq ?Y40:what [private]) (eq ?prop:predicate ?Y40) (= ?Y41:start 733697.0) (= ?Y41:end -1.0) (eq ?prop:truth 1)))
0


no
(defclass Name (is-a USER))
(deffunction reduce-class (?instance ?class) (if (eq (length$ (find-instance ((?a ?class)) (eq (instance-name ?a) ?instance))) 0) then (make-instance ?instance of ?class) (python-call tonl ?class ?instance)))
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


(deffunction mincomstart (?dur1 ?dur2)
    (return (max (send ?dur1 get-start) (send ?dur2 get-start)))
)



(deffunction maxcomend (?dur1 ?dur2)
    (bind ?e1 (send ?dur1 get-end))
    (bind ?e2 (send ?dur2 get-end))
    (if (= ?e1 ?e2) then (return ?e1))
    (if (= ?e2 -1) then
        (return ?e1)
    )
    (if (= ?e1 -1) then
        (return ?e2)
    )
    (return (min ?e1 ?e2))
)

(defclass Proposition (is-a Name) (slot truth (type INTEGER) (default 1) (pattern-match reactive)) (slot subject (type INSTANCE) (pattern-match reactive)) (slot predicate (type INSTANCE) (pattern-match reactive)) (slot time (type ?VARIABLE) (pattern-match reactive)))

(deffunction add-prop (?s ?p ?t ?r)
       (if (and (python-call ptonl ?s ?p ?t ?r)
                (= (+ (length$ (find-instance ((?prop Proposition) (?dur Duration))
                          (and (eq ?prop:subject ?s)
                               (eq ?prop:predicate ?p)
                               (eq ?prop:time ?dur)
                               (= ?dur:start (send ?t get-start))
                               (= ?dur:end (send ?t get-end))
                               (eq ?prop:truth ?r))))
                      (length$ (find-instance ((?prop Proposition))
                          (and (eq ?prop:subject ?s)
                               (eq ?prop:predicate ?p)
                               (= ?prop:time ?t)
                               (eq ?prop:truth ?r)))))
                 0))
        then (make-instance of Proposition (subject ?s)
                                           (predicate ?p)
                                           (time ?t)
                                           (truth ?r))
        else (return TRUE)))
------------OPEN---------------------
----------F-OPEN---------------------
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
admin
(reduce-class [member] Role)
member
(reduce-class [manager] Role)
manager
(reduce-class [basic_perm] Permission)
basic_perm
(reduce-class [manage_perm] Permission)
manage_perm
(reduce-class [create_perm] Permission)
create_perm
(reduce-class [public] Status)
public
(reduce-class [private] Status)
private
(add-prop [admin] (add-pred Has what [manager]) (make-instance of Duration (start 733697.0) (end -1.0)) 1)
admin has what manager at from 733697.0 till -1.0
(add-prop [member] (add-pred Has what [basic_perm]) (make-instance of Duration (start 733697.0) (end -1.0)) 1)
member has what basic_perm at from 733697.0 till -1.0
(defrule a97bcbe6634546a3bf59906030e14a06 (logical (object (is-a Proposition) (subject ?X1&:(or (eq (class ?X1) Person) (subclassp (class ?X1) Person))) (predicate ?Y1&:(or (eq (class ?Y1) Wants) (subclassp (class ?Y1) Wants))&:(or (eq (class (send ?Y1 get-to)) Create) (subclassp (class (send ?Y1 get-to)) Create))&:(or (eq (class (send (send ?Y1 get-to) get-what)) Thing) (subclassp (class (send (send ?Y1 get-to) get-what)) Thing))) (time ?X2) (truth 1))) (logical (object (is-a Proposition) (subject ?X1) (predicate ?Y2&:(or (eq (class ?Y2) Has) (subclassp (class ?Y2) Has))&:(eq (send ?Y2 get-what) [create_perm])) (time ?X3&:(or (eq (class ?X3) Duration) (subclassp (class ?X3) Duration))) (truth 1))) (test (and (<= (send ?X3 get-start) ?X2) (or (= (send ?X3 get-end) -1) (>= (send ?X3 get-end) ?X2)))) => (add-prop ?X1 (add-pred Create what (send (send ?Y1 get-to) get-what)) ?X2 1))
(defrule e8726c210c4b4de09033348b5e9b018d (logical (object (is-a Proposition) (subject ?X1&:(or (eq (class ?X1) Person) (subclassp (class ?X1) Person))) (predicate ?Y4&:(or (eq (class ?Y4) Wants) (subclassp (class ?Y4) Wants))) (time ?X2) (truth 1))) (logical (object (is-a Proposition) (subject ?X1) (predicate ?Y5&:(or (eq (class ?Y5) Can) (subclassp (class ?Y5) Can))&:(eq (send ?Y4 get-to) (send ?Y5 get-what))) (time ?X3&:(or (eq (class ?X3) Duration) (subclassp (class ?X3) Duration))) (truth 1))) (test (and (<= (send ?X3 get-start) ?X2) (or (= (send ?X3 get-end) -1) (>= (send ?X3 get-end) ?X2)))) => (add-prop ?X1 (send ?Y4 get-to) ?X2 1))
(defrule f1fe468398df41879ba8fd8451213d9c (logical (object (is-a Proposition) (subject ?X2&:(or (eq (class ?X2) Thing) (subclassp (class ?X2) Thing))) (predicate ?Y7&:(or (eq (class ?Y7) IsNeeded) (subclassp (class ?Y7) IsNeeded))) (time ?X3&:(or (eq (class ?X3) Duration) (subclassp (class ?X3) Duration))) (truth 1))) (logical (object (is-a Proposition) (subject ?X1&:(or (eq (class ?X1) Thing) (subclassp (class ?X1) Thing))) (predicate ?Y9&:(or (eq (class ?Y9) Has) (subclassp (class ?Y9) Has))&:(eq (send ?Y9 get-what) ?X2)) (time ?X5&:(or (eq (class ?X5) Duration) (subclassp (class ?X5) Duration))) (truth 1))) (test (or (and (>= (send ?X3 get-start) (send ?X3 get-start)) (or (<= (send ?X3 get-start) (send ?X3 get-end)) (= (send ?X3 get-end) -1))) (and (>= (send ?X3 get-start) (send ?X3 get-start)) (or (<= (send ?X3 get-start) (send ?X3 get-end)) (= (send ?X3 get-end) -1))))) => (add-prop ?X1 (add-pred Can what (send ?Y7 get-for_action)) (make-instance of Duration (start (mincomstart ?X3 ?X5)) (end (maxcomend ?X3 ?X5))) 1))
(defrule 41b6095bc98d409d8084eb040a369eee (logical (object (is-a Proposition) (subject ?X1&:(or (eq (class ?X1) Thing) (subclassp (class ?X1) Thing))) (predicate ?Y11&:(or (eq (class ?Y11) IsIn) (subclassp (class ?Y11) IsIn))&:(or (eq (class (send ?Y11 get-what)) Thing) (subclassp (class (send ?Y11 get-what)) Thing))) (time ?X4&:(or (eq (class ?X4) Duration) (subclassp (class ?X4) Duration))) (truth 1))) (logical (object (is-a Proposition) (subject ?X2&:(eq ?X2 (send ?Y11 get-what))) (predicate ?Y13&:(or (eq (class ?Y13) IsIn) (subclassp (class ?Y13) IsIn))&:(or (eq (class (send ?Y13 get-what)) Thing) (subclassp (class (send ?Y13 get-what)) Thing))) (time ?X5&:(or (eq (class ?X5) Duration) (subclassp (class ?X5) Duration))) (truth 1))) (test (or (and (>= (send ?X4 get-start) (send ?X4 get-start)) (or (<= (send ?X4 get-start) (send ?X4 get-end)) (= (send ?X4 get-end) -1))) (and (>= (send ?X4 get-start) (send ?X4 get-start)) (or (<= (send ?X4 get-start) (send ?X4 get-end)) (= (send ?X4 get-end) -1))))) => (add-prop ?X1 (add-pred IsIn what (send ?Y13 get-what)) (make-instance of Duration (start (mincomstart ?X4 ?X5)) (end (maxcomend ?X4 ?X5))) 1))
(defrule dcfa772840b544db8361f117b64bddba (logical (object (is-a Proposition) (subject ?X1&:(or (eq (class ?X1) Person) (subclassp (class ?X1) Person))) (predicate ?Y15&:(or (eq (class ?Y15) IsIn) (subclassp (class ?Y15) IsIn))) (time ?X3&:(or (eq (class ?X3) Duration) (subclassp (class ?X3) Duration))) (truth 1))) (logical (object (is-a Proposition) (subject ?X2&:(or (eq (class ?X2) Group) (subclassp (class ?X2) Group))) (predicate ?Y17&:(or (eq (class ?Y17) Has) (subclassp (class ?Y17) Has))&:(or (eq (class (send ?Y17 get-what)) Permission) (subclassp (class (send ?Y17 get-what)) Permission))) (time ?X5&:(or (eq (class ?X5) Duration) (subclassp (class ?X5) Duration))) (truth 1))) (test (or (and (>= (send ?X3 get-start) (send ?X3 get-start)) (or (<= (send ?X3 get-start) (send ?X3 get-end)) (= (send ?X3 get-end) -1))) (and (>= (send ?X3 get-start) (send ?X3 get-start)) (or (<= (send ?X3 get-start) (send ?X3 get-end)) (= (send ?X3 get-end) -1))))) => (add-prop ?X1 (add-pred Has what (send ?Y17 get-what)) (make-instance of Duration (start (mincomstart ?X3 ?X5)) (end (maxcomend ?X3 ?X5))) 1))
(defrule d79119a8dd7c481f914d26f2a141e861 (logical (object (is-a Proposition) (subject ?X1&:(or (eq (class ?X1) Person) (subclassp (class ?X1) Person))) (predicate ?Y19&:(or (eq (class ?Y19) Has) (subclassp (class ?Y19) Has))&:(or (eq (class (send ?Y19 get-what)) Role) (subclassp (class (send ?Y19 get-what)) Role))) (time ?X3&:(or (eq (class ?X3) Duration) (subclassp (class ?X3) Duration))) (truth 1))) (logical (object (is-a Proposition) (subject ?X2&:(eq ?X2 (send ?Y19 get-what))) (predicate ?Y21&:(or (eq (class ?Y21) Has) (subclassp (class ?Y21) Has))&:(or (eq (class (send ?Y21 get-what)) Permission) (subclassp (class (send ?Y21 get-what)) Permission))) (time ?X5&:(or (eq (class ?X5) Duration) (subclassp (class ?X5) Duration))) (truth 1))) (test (or (and (>= (send ?X3 get-start) (send ?X3 get-start)) (or (<= (send ?X3 get-start) (send ?X3 get-end)) (= (send ?X3 get-end) -1))) (and (>= (send ?X3 get-start) (send ?X3 get-start)) (or (<= (send ?X3 get-start) (send ?X3 get-end)) (= (send ?X3 get-end) -1))))) => (add-prop ?X1 (add-pred Has what (send ?Y21 get-what)) (make-instance of Duration (start (mincomstart ?X3 ?X5)) (end (maxcomend ?X3 ?X5))) 1))
(defrule 640355a95eb043f28af54579de8dac36 (logical (object (is-a Person) (name ?X1))) => (add-prop ?X1 (add-pred Has what [member]) (make-instance of Duration (start 733697.0) (end -1.0)) 1))
(defrule 0f7a16214f1d42cca19c345c44c84c9f (logical (object (is-a Proposition) (subject ?X1&:(or (eq (class ?X1) Person) (subclassp (class ?X1) Person))) (predicate ?Y23&:(or (eq (class ?Y23) Create) (subclassp (class ?Y23) Create))&:(or (eq (class (send ?Y23 get-what)) Content) (subclassp (class (send ?Y23 get-what)) Content))) (time ?X3) (truth 1))) => (reduce-class (send ?Y23 get-what) Content) (add-prop ?X1 (add-pred IsOwner of (send ?Y23 get-what)) (make-instance of Duration (start ?X3) (end -1.0)) 1) (add-prop (send ?Y23 get-what) (add-pred Has what [private]) (make-instance of Duration (start ?X3) (end -1.0)) 1))
(defrule d5d6e82b455f4990a24d5b435498b0bf (logical (object (is-a Permission) (name ?X2))) => (add-prop [manager] (add-pred Has what ?X2) (make-instance of Duration (start 733697.0) (end -1.0)) 1))
(defrule 3edb5736c5c04d47ab4d2e18c6e3084f (logical (object (is-a Proposition) (subject ?X1&:(or (eq (class ?X1) Content) (subclassp (class ?X1) Content))) (predicate ?Y24&:(or (eq (class ?Y24) Has) (subclassp (class ?Y24) Has))&:(eq (send ?Y24 get-what) [public])) (time ?X2&:(or (eq (class ?X2) Duration) (subclassp (class ?X2) Duration))) (truth 1))) => (add-prop [basic_perm] (add-pred IsNeeded for_action (add-pred View what ?X1)) ?X2 1))
(defrule 1350c1258ef54e9980928215424fcfdb (logical (object (is-a Proposition) (subject ?X1&:(or (eq (class ?X1) Content) (subclassp (class ?X1) Content))) (predicate ?Y26&:(or (eq (class ?Y26) Has) (subclassp (class ?Y26) Has))&:(eq (send ?Y26 get-what) [private])) (time ?X2&:(or (eq (class ?X2) Duration) (subclassp (class ?X2) Duration))) (truth 1))) => (add-prop [manage_perm] (add-pred IsNeeded for_action (add-pred View what ?X1)) ?X2 1))
(defrule c273cc45aea344c6bb1aed994dfeade3 (logical (object (is-a Proposition) (subject ?X1&:(or (eq (class ?X1) Content) (subclassp (class ?X1) Content))) (predicate ?Y28&:(or (eq (class ?Y28) Has) (subclassp (class ?Y28) Has))&:(eq (send ?Y28 get-what) [private])) (time ?X3&:(or (eq (class ?X3) Duration) (subclassp (class ?X3) Duration))) (truth 1))) (logical (object (is-a Proposition) (subject ?X2&:(or (eq (class ?X2) Person) (subclassp (class ?X2) Person))) (predicate ?Y30&:(or (eq (class ?Y30) IsOwner) (subclassp (class ?Y30) IsOwner))&:(eq (send ?Y30 get-of) ?X1)) (time ?X4&:(or (eq (class ?X4) Duration) (subclassp (class ?X4) Duration))) (truth 1))) (test (or (and (>= (send ?X3 get-start) (send ?X3 get-start)) (or (<= (send ?X3 get-start) (send ?X3 get-end)) (= (send ?X3 get-end) -1))) (and (>= (send ?X3 get-start) (send ?X3 get-start)) (or (<= (send ?X3 get-start) (send ?X3 get-end)) (= (send ?X3 get-end) -1))))) => (add-prop ?X2 (add-pred Can what (add-pred View what ?X1)) (make-instance of Duration (start (mincomstart ?X3 ?X4)) (end (maxcomend ?X3 ?X4))) 1))
(defrule c0e45d6009ab4321a4ef197e543061e6 (logical (object (is-a Proposition) (subject ?X1&:(or (eq (class ?X1) Person) (subclassp (class ?X1) Person))) (predicate ?Y32&:(or (eq (class ?Y32) Publish) (subclassp (class ?Y32) Publish))&:(or (eq (class (send ?Y32 get-what)) Content) (subclassp (class (send ?Y32 get-what)) Content))) (time ?X3) (truth 1))) (logical (object (is-a Proposition) (subject ?X2&:(eq ?X2 (send ?Y32 get-what))) (predicate ?Y33&:(or (eq (class ?Y33) Has) (subclassp (class ?Y33) Has))&:(or (eq (class (send ?Y33 get-what)) Status) (subclassp (class (send ?Y33 get-what)) Status))) (time ?X5&:(or (eq (class ?X5) Duration) (subclassp (class ?X5) Duration))) (truth 1))) => (send ?X5 put-end 733697) (add-prop (send ?Y32 get-what) (add-pred Has what [public]) (make-instance of Duration (start ?X3) (end -1.0)) 1))
(defrule 599977bc9d9d40aab2e0d49b041f0314 (logical (object (is-a Content) (name ?X1))) => (add-prop [manage_perm] (add-pred IsNeeded for_action (add-pred Publish what ?X1)) (make-instance of Duration (start 733697.0) (end -1.0)) 1))
(defrule 8091759169b946699175bb2d275c14fa (logical (object (is-a Proposition) (subject ?X1&:(or (eq (class ?X1) Person) (subclassp (class ?X1) Person))) (predicate ?Y35&:(or (eq (class ?Y35) Hide) (subclassp (class ?Y35) Hide))&:(or (eq (class (send ?Y35 get-what)) Content) (subclassp (class (send ?Y35 get-what)) Content))) (time ?X3) (truth 1))) (logical (object (is-a Proposition) (subject ?X2&:(eq ?X2 (send ?Y35 get-what))) (predicate ?Y36&:(or (eq (class ?Y36) Has) (subclassp (class ?Y36) Has))&:(or (eq (class (send ?Y36 get-what)) Status) (subclassp (class (send ?Y36 get-what)) Status))) (time ?X5&:(or (eq (class ?X5) Duration) (subclassp (class ?X5) Duration))) (truth 1))) => (send ?X5 put-end 733697) (add-prop (send ?Y35 get-what) (add-pred Has what [private]) (make-instance of Duration (start ?X3) (end -1.0)) 1))
(defrule 714be04579e143bd8398bda27f4c40f1 (logical (object (is-a Proposition) (subject ?X1&:(or (eq (class ?X1) Person) (subclassp (class ?X1) Person))) (predicate ?Y38&:(or (eq (class ?Y38) IsOwner) (subclassp (class ?Y38) IsOwner))&:(or (eq (class (send ?Y38 get-of)) Content) (subclassp (class (send ?Y38 get-of)) Content))) (time ?X3&:(or (eq (class ?X3) Duration) (subclassp (class ?X3) Duration))) (truth 1))) => (add-prop ?X1 (add-pred Can what (add-pred Hide what (send ?Y38 get-of))) ?X3 1))
(reduce-class [john] Person)
john
(reduce-class [pete] Person)
pete
(reduce-class [jane] Person)
jane
(reduce-class [c1] Content)
c1
(reduce-class [c2] Content)
c2
(add-prop [john] (add-pred Has what [manager]) (make-instance of Duration (start 733697.0) (end -1.0)) 1)
john has what manager at from 733697.0 till -1.0
(add-prop [jane] (add-pred Has what [create_perm]) (make-instance of Duration (start 733697.0) (end -1.0)) 1)
jane has what create_perm at from 733697.0 till -1.0
(add-prop [jane] (add-pred Wants to (add-pred Create what [c1])) 733697.0 1)
jane wants to create what c1 at 733697.0
(add-prop [pete] (add-pred Wants to (add-pred Create what [c2])) 733697.0 1)
pete wants to create what c2 at 733697.0
(add-prop [jane] (add-pred Wants to (add-pred Publish what [c1])) 733697.0 1)
jane wants to publish what c1 at 733697.0
(add-prop [pete] (add-pred Wants to (add-pred Publish what [c2])) 733697.0 1)
pete wants to publish what c2 at 733697.0
----------running---------------------
jane create what c1 at 733697.0
jane isowner of c1 at from 733697.0 till -1.0
c1 has what private at from 733697.0 till -1.0
jane can what view what c1 at from 733697.0 till -1.0
manage_perm isneeded for_action view what c1 at from 733697.0 till -1.0
jane can what hide what c1 at from 733697.0 till -1.0
manage_perm isneeded for_action publish what c2 at from 733697.0 till -1.0
manage_perm isneeded for_action publish what c1 at from 733697.0 till -1.0
jane has what member at from 733697.0 till -1.0
jane has what basic_perm at from 733697.0 till -1.0
pete has what member at from 733697.0 till -1.0
pete has what basic_perm at from 733697.0 till -1.0
john has what member at from 733697.0 till -1.0
john has what basic_perm at from 733697.0 till -1.0
manager has what create_perm at from 733697.0 till -1.0
admin has what create_perm at from 733697.0 till -1.0
john has what create_perm at from 733697.0 till -1.0
manager has what manage_perm at from 733697.0 till -1.0
manager can what view what c1 at from 733697.0 till -1.0
manager can what publish what c2 at from 733697.0 till -1.0
manager can what publish what c1 at from 733697.0 till -1.0
admin has what manage_perm at from 733697.0 till -1.0
admin can what view what c1 at from 733697.0 till -1.0
admin can what publish what c2 at from 733697.0 till -1.0
admin can what publish what c1 at from 733697.0 till -1.0
john has what manage_perm at from 733697.0 till -1.0
john can what view what c1 at from 733697.0 till -1.0
john can what publish what c2 at from 733697.0 till -1.0
john can what publish what c1 at from 733697.0 till -1.0
manager has what basic_perm at from 733697.0 till -1.0
admin has what basic_perm at from 733697.0 till -1.0
admin has what member at from 733697.0 till -1.0
----------runned: 33---------------------
(add-prop [john] (add-pred Wants to (add-pred Publish what [c1])) 733697.0 1)
john wants to publish what c1 at 733697.0
----------running---------------------
john publish what c1 at 733697.0
c1 has what public at from 733697.0 till -1.0
basic_perm isneeded for_action view what c1 at from 733697.0 till -1.0
pete can what view what c1 at from 733697.0 till -1.0
member can what view what c1 at from 733697.0 till -1.0
----------runned: 10---------------------
(find-all-instances ((?prop Proposition) (?Y40 Has) (?Y41 Duration)) (and (eq ?prop:subject [c1]) (eq ?Y40:what [private]) (eq ?prop:predicate ?Y40) (= ?Y41:start 733697.0) (= ?Y41:end -1.0) (eq ?prop:truth 1)))
41


c1 has what private at from 733697.0 till 733697.0
c1 has what private at from 733697.0 till 733697.0
c1 has what private at from 733697.0 till 733697.0
c1 has what private at from 733697.0 till 733697.0
c1 has what private at from 733697.0 till 733697.0
c1 has what private at from 733697.0 till 733697.0
c1 has what private at from 733697.0 till 733697.0
c1 has what private at from 733697.0 till 733697.0
c1 has what private at from 733697.0 till 733697.0
c1 has what private at from 733697.0 till 733697.0
c1 has what private at from 733697.0 till 733697.0
c1 has what private at from 733697.0 till 733697.0
c1 has what private at from 733697.0 till 733697.0
c1 has what private at from 733697.0 till 733697.0
c1 has what private at from 733697.0 till 733697.0
c1 has what private at from 733697.0 till 733697.0
c1 has what private at from 733697.0 till 733697.0
c1 has what private at from 733697.0 till 733697.0
c1 has what private at from 733697.0 till 733697.0
c1 has what private at from 733697.0 till 733697.0
c1 has what private at from 733697.0 till 733697.0
c1 has what private at from 733697.0 till 733697.0
c1 has what private at from 733697.0 till 733697.0
c1 has what private at from 733697.0 till 733697.0
c1 has what private at from 733697.0 till 733697.0
c1 has what private at from 733697.0 till 733697.0
c1 has what private at from 733697.0 till 733697.0
c1 has what private at from 733697.0 till 733697.0
c1 has what private at from 733697.0 till 733697.0
c1 has what private at from 733697.0 till 733697.0
c1 has what private at from 733697.0 till 733697.0
c1 has what private at from 733697.0 till 733697.0
c1 has what private at from 733697.0 till 733697.0
c1 has what private at from 733697.0 till 733697.0
c1 has what private at from 733697.0 till 733697.0
c1 has what private at from 733697.0 till 733697.0
c1 has what private at from 733697.0 till 733697.0
c1 has what private at from 733697.0 till 733697.0
c1 has what private at from 733697.0 till 733697.0
c1 has what private at from 733697.0 till 733697.0
c1 has what private at from 733697.0 till 733697.0
(defclass Name (is-a USER))
(deffunction reduce-class (?instance ?class) (if (eq (length$ (find-instance ((?a ?class)) (eq (instance-name ?a) ?instance))) 0) then (make-instance ?instance of ?class) (python-call tonl ?class ?instance)))
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


(deffunction mincomstart (?dur1 ?dur2)
    (return (max (send ?dur1 get-start) (send ?dur2 get-start)))
)



(deffunction maxcomend (?dur1 ?dur2)
    (bind ?e1 (send ?dur1 get-end))
    (bind ?e2 (send ?dur2 get-end))
    (if (= ?e1 ?e2) then (return ?e1))
    (if (= ?e2 -1) then
        (return ?e1)
    )
    (if (= ?e1 -1) then
        (return ?e2)
    )
    (return (min ?e1 ?e2))
)

(defclass Proposition (is-a Name) (slot truth (type INTEGER) (default 1) (pattern-match reactive)) (slot subject (type INSTANCE) (pattern-match reactive)) (slot predicate (type INSTANCE) (pattern-match reactive)) (slot time (type ?VARIABLE) (pattern-match reactive)))

(deffunction add-prop (?s ?p ?t ?r)
       (if (and (python-call ptonl ?s ?p ?t ?r)
                (= (+ (length$ (find-instance ((?prop Proposition) (?dur Duration))
                          (and (eq ?prop:subject ?s)
                               (eq ?prop:predicate ?p)
                               (eq ?prop:time ?dur)
                               (= ?dur:start (send ?t get-start))
                               (= ?dur:end (send ?t get-end))
                               (eq ?prop:truth ?r))))
                      (length$ (find-instance ((?prop Proposition))
                          (and (eq ?prop:subject ?s)
                               (eq ?prop:predicate ?p)
                               (= ?prop:time ?t)
                               (eq ?prop:truth ?r)))))
                 0))
        then (make-instance of Proposition (subject ?s)
                                           (predicate ?p)
                                           (time ?t)
                                           (truth ?r))
        else (return TRUE)))
------------OPEN---------------------
----------F-OPEN---------------------
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
admin
(reduce-class [member] Role)
member
(reduce-class [manager] Role)
manager
(reduce-class [basic_perm] Permission)
basic_perm
(reduce-class [manage_perm] Permission)
manage_perm
(reduce-class [create_perm] Permission)
create_perm
(reduce-class [public] Status)
public
(reduce-class [private] Status)
private
(add-prop [admin] (add-pred Has what [manager]) (make-instance of Duration (start 733697.0) (end -1.0)) 1)
admin has what manager at from 733697.0 till -1.0
(add-prop [member] (add-pred Has what [basic_perm]) (make-instance of Duration (start 733697.0) (end -1.0)) 1)
member has what basic_perm at from 733697.0 till -1.0
(defrule 7f0cfd307f0b4a76acfa490f8f93c79b (logical (object (is-a Proposition) (subject ?X1&:(or (eq (class ?X1) Person) (subclassp (class ?X1) Person))) (predicate ?Y1&:(or (eq (class ?Y1) Wants) (subclassp (class ?Y1) Wants))&:(or (eq (class (send ?Y1 get-to)) Create) (subclassp (class (send ?Y1 get-to)) Create))&:(or (eq (class (send (send ?Y1 get-to) get-what)) Thing) (subclassp (class (send (send ?Y1 get-to) get-what)) Thing))) (time ?X2) (truth 1))) (logical (object (is-a Proposition) (subject ?X1) (predicate ?Y2&:(or (eq (class ?Y2) Has) (subclassp (class ?Y2) Has))&:(eq (send ?Y2 get-what) [create_perm])) (time ?X3&:(or (eq (class ?X3) Duration) (subclassp (class ?X3) Duration))) (truth 1))) (test (and (<= (send ?X3 get-start) ?X2) (or (= (send ?X3 get-end) -1) (>= (send ?X3 get-end) ?X2)))) => (add-prop ?X1 (add-pred Create what (send (send ?Y1 get-to) get-what)) ?X2 1))
(defrule 79c59fa06bbe4f7a934d97e2017f8b76 (logical (object (is-a Proposition) (subject ?X1&:(or (eq (class ?X1) Person) (subclassp (class ?X1) Person))) (predicate ?Y4&:(or (eq (class ?Y4) Wants) (subclassp (class ?Y4) Wants))) (time ?X2) (truth 1))) (logical (object (is-a Proposition) (subject ?X1) (predicate ?Y5&:(or (eq (class ?Y5) Can) (subclassp (class ?Y5) Can))&:(eq (send ?Y4 get-to) (send ?Y5 get-what))) (time ?X3&:(or (eq (class ?X3) Duration) (subclassp (class ?X3) Duration))) (truth 1))) (test (and (<= (send ?X3 get-start) ?X2) (or (= (send ?X3 get-end) -1) (>= (send ?X3 get-end) ?X2)))) => (add-prop ?X1 (send ?Y4 get-to) ?X2 1))
(defrule 491a0c6a08744d2aa7fabd43d460f773 (logical (object (is-a Proposition) (subject ?X2&:(or (eq (class ?X2) Thing) (subclassp (class ?X2) Thing))) (predicate ?Y7&:(or (eq (class ?Y7) IsNeeded) (subclassp (class ?Y7) IsNeeded))) (time ?X3&:(or (eq (class ?X3) Duration) (subclassp (class ?X3) Duration))) (truth 1))) (logical (object (is-a Proposition) (subject ?X1&:(or (eq (class ?X1) Thing) (subclassp (class ?X1) Thing))) (predicate ?Y9&:(or (eq (class ?Y9) Has) (subclassp (class ?Y9) Has))&:(eq (send ?Y9 get-what) ?X2)) (time ?X5&:(or (eq (class ?X5) Duration) (subclassp (class ?X5) Duration))) (truth 1))) (test (or (and (>= (send ?X3 get-start) (send ?X3 get-start)) (or (<= (send ?X3 get-start) (send ?X3 get-end)) (= (send ?X3 get-end) -1))) (and (>= (send ?X3 get-start) (send ?X3 get-start)) (or (<= (send ?X3 get-start) (send ?X3 get-end)) (= (send ?X3 get-end) -1))))) => (add-prop ?X1 (add-pred Can what (send ?Y7 get-for_action)) (make-instance of Duration (start (mincomstart ?X3 ?X5)) (end (maxcomend ?X3 ?X5))) 1))
(defrule 0cf58d9d054641a4b11032f3c7d86732 (logical (object (is-a Proposition) (subject ?X1&:(or (eq (class ?X1) Thing) (subclassp (class ?X1) Thing))) (predicate ?Y11&:(or (eq (class ?Y11) IsIn) (subclassp (class ?Y11) IsIn))&:(or (eq (class (send ?Y11 get-what)) Thing) (subclassp (class (send ?Y11 get-what)) Thing))) (time ?X4&:(or (eq (class ?X4) Duration) (subclassp (class ?X4) Duration))) (truth 1))) (logical (object (is-a Proposition) (subject ?X2&:(eq ?X2 (send ?Y11 get-what))) (predicate ?Y13&:(or (eq (class ?Y13) IsIn) (subclassp (class ?Y13) IsIn))&:(or (eq (class (send ?Y13 get-what)) Thing) (subclassp (class (send ?Y13 get-what)) Thing))) (time ?X5&:(or (eq (class ?X5) Duration) (subclassp (class ?X5) Duration))) (truth 1))) (test (or (and (>= (send ?X4 get-start) (send ?X4 get-start)) (or (<= (send ?X4 get-start) (send ?X4 get-end)) (= (send ?X4 get-end) -1))) (and (>= (send ?X4 get-start) (send ?X4 get-start)) (or (<= (send ?X4 get-start) (send ?X4 get-end)) (= (send ?X4 get-end) -1))))) => (add-prop ?X1 (add-pred IsIn what (send ?Y13 get-what)) (make-instance of Duration (start (mincomstart ?X4 ?X5)) (end (maxcomend ?X4 ?X5))) 1))
(defrule 35a026f441ec476f86a63129a38a9892 (logical (object (is-a Proposition) (subject ?X1&:(or (eq (class ?X1) Person) (subclassp (class ?X1) Person))) (predicate ?Y15&:(or (eq (class ?Y15) IsIn) (subclassp (class ?Y15) IsIn))) (time ?X3&:(or (eq (class ?X3) Duration) (subclassp (class ?X3) Duration))) (truth 1))) (logical (object (is-a Proposition) (subject ?X2&:(or (eq (class ?X2) Group) (subclassp (class ?X2) Group))) (predicate ?Y17&:(or (eq (class ?Y17) Has) (subclassp (class ?Y17) Has))&:(or (eq (class (send ?Y17 get-what)) Permission) (subclassp (class (send ?Y17 get-what)) Permission))) (time ?X5&:(or (eq (class ?X5) Duration) (subclassp (class ?X5) Duration))) (truth 1))) (test (or (and (>= (send ?X3 get-start) (send ?X3 get-start)) (or (<= (send ?X3 get-start) (send ?X3 get-end)) (= (send ?X3 get-end) -1))) (and (>= (send ?X3 get-start) (send ?X3 get-start)) (or (<= (send ?X3 get-start) (send ?X3 get-end)) (= (send ?X3 get-end) -1))))) => (add-prop ?X1 (add-pred Has what (send ?Y17 get-what)) (make-instance of Duration (start (mincomstart ?X3 ?X5)) (end (maxcomend ?X3 ?X5))) 1))
(defrule 3eb05905e41f40ea838e4d84f57dcdb6 (logical (object (is-a Proposition) (subject ?X1&:(or (eq (class ?X1) Person) (subclassp (class ?X1) Person))) (predicate ?Y19&:(or (eq (class ?Y19) Has) (subclassp (class ?Y19) Has))&:(or (eq (class (send ?Y19 get-what)) Role) (subclassp (class (send ?Y19 get-what)) Role))) (time ?X3&:(or (eq (class ?X3) Duration) (subclassp (class ?X3) Duration))) (truth 1))) (logical (object (is-a Proposition) (subject ?X2&:(eq ?X2 (send ?Y19 get-what))) (predicate ?Y21&:(or (eq (class ?Y21) Has) (subclassp (class ?Y21) Has))&:(or (eq (class (send ?Y21 get-what)) Permission) (subclassp (class (send ?Y21 get-what)) Permission))) (time ?X5&:(or (eq (class ?X5) Duration) (subclassp (class ?X5) Duration))) (truth 1))) (test (or (and (>= (send ?X3 get-start) (send ?X3 get-start)) (or (<= (send ?X3 get-start) (send ?X3 get-end)) (= (send ?X3 get-end) -1))) (and (>= (send ?X3 get-start) (send ?X3 get-start)) (or (<= (send ?X3 get-start) (send ?X3 get-end)) (= (send ?X3 get-end) -1))))) => (add-prop ?X1 (add-pred Has what (send ?Y21 get-what)) (make-instance of Duration (start (mincomstart ?X3 ?X5)) (end (maxcomend ?X3 ?X5))) 1))
(defrule 9c6211b527374816a8ff290ba8741f72 (logical (object (is-a Person) (name ?X1))) => (add-prop ?X1 (add-pred Has what [member]) (make-instance of Duration (start 733697.0) (end -1.0)) 1))
(defrule 8f8f2ec089494e79ab7091b4bf392a9b (logical (object (is-a Proposition) (subject ?X1&:(or (eq (class ?X1) Person) (subclassp (class ?X1) Person))) (predicate ?Y23&:(or (eq (class ?Y23) Create) (subclassp (class ?Y23) Create))&:(or (eq (class (send ?Y23 get-what)) Content) (subclassp (class (send ?Y23 get-what)) Content))) (time ?X3) (truth 1))) => (reduce-class (send ?Y23 get-what) Content) (add-prop ?X1 (add-pred IsOwner of (send ?Y23 get-what)) (make-instance of Duration (start ?X3) (end -1.0)) 1) (add-prop (send ?Y23 get-what) (add-pred Has what [private]) (make-instance of Duration (start ?X3) (end -1.0)) 1))
(defrule 5361f4e4745c4b2ab48eb3d9599ad67d (logical (object (is-a Permission) (name ?X2))) => (add-prop [manager] (add-pred Has what ?X2) (make-instance of Duration (start 733697.0) (end -1.0)) 1))
(defrule 5d2120c8469e40439b8789782fde69e6 (logical (object (is-a Proposition) (subject ?X1&:(or (eq (class ?X1) Content) (subclassp (class ?X1) Content))) (predicate ?Y24&:(or (eq (class ?Y24) Has) (subclassp (class ?Y24) Has))&:(eq (send ?Y24 get-what) [public])) (time ?X2&:(or (eq (class ?X2) Duration) (subclassp (class ?X2) Duration))) (truth 1))) => (add-prop [basic_perm] (add-pred IsNeeded for_action (add-pred View what ?X1)) ?X2 1))
(defrule 49f9c865b6104c939529c533dcc3c6b9 (logical (object (is-a Proposition) (subject ?X1&:(or (eq (class ?X1) Content) (subclassp (class ?X1) Content))) (predicate ?Y26&:(or (eq (class ?Y26) Has) (subclassp (class ?Y26) Has))&:(eq (send ?Y26 get-what) [private])) (time ?X2&:(or (eq (class ?X2) Duration) (subclassp (class ?X2) Duration))) (truth 1))) => (add-prop [manage_perm] (add-pred IsNeeded for_action (add-pred View what ?X1)) ?X2 1))
(defrule a008f9c9da764d5d86637d31aab41bf6 (logical (object (is-a Proposition) (subject ?X1&:(or (eq (class ?X1) Content) (subclassp (class ?X1) Content))) (predicate ?Y28&:(or (eq (class ?Y28) Has) (subclassp (class ?Y28) Has))&:(eq (send ?Y28 get-what) [private])) (time ?X3&:(or (eq (class ?X3) Duration) (subclassp (class ?X3) Duration))) (truth 1))) (logical (object (is-a Proposition) (subject ?X2&:(or (eq (class ?X2) Person) (subclassp (class ?X2) Person))) (predicate ?Y30&:(or (eq (class ?Y30) IsOwner) (subclassp (class ?Y30) IsOwner))&:(eq (send ?Y30 get-of) ?X1)) (time ?X4&:(or (eq (class ?X4) Duration) (subclassp (class ?X4) Duration))) (truth 1))) (test (or (and (>= (send ?X3 get-start) (send ?X3 get-start)) (or (<= (send ?X3 get-start) (send ?X3 get-end)) (= (send ?X3 get-end) -1))) (and (>= (send ?X3 get-start) (send ?X3 get-start)) (or (<= (send ?X3 get-start) (send ?X3 get-end)) (= (send ?X3 get-end) -1))))) => (add-prop ?X2 (add-pred Can what (add-pred View what ?X1)) (make-instance of Duration (start (mincomstart ?X3 ?X4)) (end (maxcomend ?X3 ?X4))) 1))
(defrule 4071f4fb71c3473dad6c2a605c4f9fb6 (logical (object (is-a Proposition) (subject ?X1&:(or (eq (class ?X1) Person) (subclassp (class ?X1) Person))) (predicate ?Y32&:(or (eq (class ?Y32) Publish) (subclassp (class ?Y32) Publish))&:(or (eq (class (send ?Y32 get-what)) Content) (subclassp (class (send ?Y32 get-what)) Content))) (time ?X3) (truth 1))) (logical (object (is-a Proposition) (subject ?X2&:(eq ?X2 (send ?Y32 get-what))) (predicate ?Y33&:(or (eq (class ?Y33) Has) (subclassp (class ?Y33) Has))&:(or (eq (class (send ?Y33 get-what)) Status) (subclassp (class (send ?Y33 get-what)) Status))) (time ?X5&:(or (eq (class ?X5) Duration) (subclassp (class ?X5) Duration))) (truth 1))) => (send ?X5 put-end 733697) (add-prop (send ?Y32 get-what) (add-pred Has what [public]) (make-instance of Duration (start ?X3) (end -1.0)) 1))
(defrule 2f083f0b3bca4e8eb71994b3c5b798f3 (logical (object (is-a Content) (name ?X1))) => (add-prop [manage_perm] (add-pred IsNeeded for_action (add-pred Publish what ?X1)) (make-instance of Duration (start 733697.0) (end -1.0)) 1))
(defrule 55fb72e94702487ab876042bfd82ecbb (logical (object (is-a Proposition) (subject ?X1&:(or (eq (class ?X1) Person) (subclassp (class ?X1) Person))) (predicate ?Y35&:(or (eq (class ?Y35) Hide) (subclassp (class ?Y35) Hide))&:(or (eq (class (send ?Y35 get-what)) Content) (subclassp (class (send ?Y35 get-what)) Content))) (time ?X3) (truth 1))) (logical (object (is-a Proposition) (subject ?X2&:(eq ?X2 (send ?Y35 get-what))) (predicate ?Y36&:(or (eq (class ?Y36) Has) (subclassp (class ?Y36) Has))&:(or (eq (class (send ?Y36 get-what)) Status) (subclassp (class (send ?Y36 get-what)) Status))) (time ?X5&:(or (eq (class ?X5) Duration) (subclassp (class ?X5) Duration))) (truth 1))) => (send ?X5 put-end 733697) (add-prop (send ?Y35 get-what) (add-pred Has what [private]) (make-instance of Duration (start ?X3) (end -1.0)) 1))
(defrule df8f205b7f4d404b84eb9f8b7627b260 (logical (object (is-a Proposition) (subject ?X1&:(or (eq (class ?X1) Person) (subclassp (class ?X1) Person))) (predicate ?Y38&:(or (eq (class ?Y38) IsOwner) (subclassp (class ?Y38) IsOwner))&:(or (eq (class (send ?Y38 get-of)) Content) (subclassp (class (send ?Y38 get-of)) Content))) (time ?X3&:(or (eq (class ?X3) Duration) (subclassp (class ?X3) Duration))) (truth 1))) => (add-prop ?X1 (add-pred Can what (add-pred Hide what (send ?Y38 get-of))) ?X3 1))
(reduce-class [john] Person)
john
(reduce-class [pete] Person)
pete
(reduce-class [jane] Person)
jane
(reduce-class [c1] Content)
c1
(reduce-class [c2] Content)
c2
(add-prop [john] (add-pred Has what [manager]) (make-instance of Duration (start 733697.0) (end -1.0)) 1)
john has what manager at from 733697.0 till -1.0
(add-prop [jane] (add-pred Has what [create_perm]) (make-instance of Duration (start 733697.0) (end -1.0)) 1)
jane has what create_perm at from 733697.0 till -1.0
(add-prop [jane] (add-pred Wants to (add-pred Create what [c1])) 733697.0 1)
jane wants to create what c1 at 733697.0
(add-prop [pete] (add-pred Wants to (add-pred Create what [c2])) 733697.0 1)
pete wants to create what c2 at 733697.0
(add-prop [jane] (add-pred Wants to (add-pred Publish what [c1])) 733697.0 1)
jane wants to publish what c1 at 733697.0
(add-prop [pete] (add-pred Wants to (add-pred Publish what [c2])) 733697.0 1)
pete wants to publish what c2 at 733697.0
----------running---------------------
jane create what c1 at 733697.0
jane isowner of c1 at from 733697.0 till -1.0
c1 has what private at from 733697.0 till -1.0
jane can what view what c1 at from 733697.0 till -1.0
manage_perm isneeded for_action view what c1 at from 733697.0 till -1.0
jane can what hide what c1 at from 733697.0 till -1.0
manage_perm isneeded for_action publish what c2 at from 733697.0 till -1.0
manage_perm isneeded for_action publish what c1 at from 733697.0 till -1.0
jane has what member at from 733697.0 till -1.0
jane has what basic_perm at from 733697.0 till -1.0
pete has what member at from 733697.0 till -1.0
pete has what basic_perm at from 733697.0 till -1.0
john has what member at from 733697.0 till -1.0
john has what basic_perm at from 733697.0 till -1.0
manager has what create_perm at from 733697.0 till -1.0
admin has what create_perm at from 733697.0 till -1.0
john has what create_perm at from 733697.0 till -1.0
manager has what manage_perm at from 733697.0 till -1.0
manager can what view what c1 at from 733697.0 till -1.0
manager can what publish what c2 at from 733697.0 till -1.0
manager can what publish what c1 at from 733697.0 till -1.0
admin has what manage_perm at from 733697.0 till -1.0
admin can what view what c1 at from 733697.0 till -1.0
admin can what publish what c2 at from 733697.0 till -1.0
admin can what publish what c1 at from 733697.0 till -1.0
john has what manage_perm at from 733697.0 till -1.0
john can what view what c1 at from 733697.0 till -1.0
john can what publish what c2 at from 733697.0 till -1.0
john can what publish what c1 at from 733697.0 till -1.0
manager has what basic_perm at from 733697.0 till -1.0
admin has what basic_perm at from 733697.0 till -1.0
admin has what member at from 733697.0 till -1.0
----------runned: 33---------------------
(add-prop [john] (add-pred Wants to (add-pred Publish what [c1])) 733697.0 1)
john wants to publish what c1 at 733697.0
----------running---------------------
john publish what c1 at 733697.0
c1 has what public at from 733697.0 till -1.0
basic_perm isneeded for_action view what c1 at from 733697.0 till -1.0
pete can what view what c1 at from 733697.0 till -1.0
member can what view what c1 at from 733697.0 till -1.0
----------runned: 10---------------------
(find-all-instances ((?prop Proposition) (?Y40 Has) (?Y41 Duration)) (and (eq ?prop:subject [c1]) (eq ?Y40:what [private]) (eq ?prop:predicate ?Y40) (= ?Y41:start 733697.0) (= ?Y41:end -1.0) (eq ?prop:truth 1)))
41


c1 has what private at from 733697.0 till 733697.0
c1 has what private at from 733697.0 till 733697.0
c1 has what private at from 733697.0 till 733697.0
c1 has what private at from 733697.0 till 733697.0
c1 has what private at from 733697.0 till 733697.0
c1 has what private at from 733697.0 till 733697.0
c1 has what private at from 733697.0 till 733697.0
c1 has what private at from 733697.0 till 733697.0
c1 has what private at from 733697.0 till 733697.0
c1 has what private at from 733697.0 till 733697.0
c1 has what private at from 733697.0 till 733697.0
c1 has what private at from 733697.0 till 733697.0
c1 has what private at from 733697.0 till 733697.0
c1 has what private at from 733697.0 till 733697.0
c1 has what private at from 733697.0 till 733697.0
c1 has what private at from 733697.0 till 733697.0
c1 has what private at from 733697.0 till 733697.0
c1 has what private at from 733697.0 till 733697.0
c1 has what private at from 733697.0 till 733697.0
c1 has what private at from 733697.0 till 733697.0
c1 has what private at from 733697.0 till 733697.0
c1 has what private at from 733697.0 till 733697.0
c1 has what private at from 733697.0 till 733697.0
c1 has what private at from 733697.0 till 733697.0
c1 has what private at from 733697.0 till 733697.0
c1 has what private at from 733697.0 till 733697.0
c1 has what private at from 733697.0 till 733697.0
c1 has what private at from 733697.0 till 733697.0
c1 has what private at from 733697.0 till 733697.0
c1 has what private at from 733697.0 till 733697.0
c1 has what private at from 733697.0 till 733697.0
c1 has what private at from 733697.0 till 733697.0
c1 has what private at from 733697.0 till 733697.0
c1 has what private at from 733697.0 till 733697.0
c1 has what private at from 733697.0 till 733697.0
c1 has what private at from 733697.0 till 733697.0
c1 has what private at from 733697.0 till 733697.0
c1 has what private at from 733697.0 till 733697.0
c1 has what private at from 733697.0 till 733697.0
c1 has what private at from 733697.0 till 733697.0
c1 has what private at from 733697.0 till 733697.0

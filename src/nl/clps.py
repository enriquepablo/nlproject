

import clips
from nl.log import logger

#clips.DebugConfig.ExternalTraceback = True
#clips.EngineConfig.ResetGlobals = True
#clips.EngineConfig.IncrementalReset = True


# CLIPS SNIPPETS
################

class_constraint = '?%(val)s&:(or (eq (class ?%(val)s) %(cls)s) (subclassp (class ?%(val)s) %(cls)s))'


# CLIPS DEFINITIONS
###################

_name_def = '(defclass Name (is-a USER))'
logger.info(_name_def)
clips.Build(_name_def)

_reduce_class = '''
(deffunction reduce-class (?instance ?class)
    (if (eq (length$
                (find-all-instances ((?a ?class))(eq (instance-name ?a) ?instance)))
             0)
    then (make-instance ?instance of ?class)
    else (return TRUE)))'''
logger.info(_reduce_class)
clips.Build(_reduce_class)


_duration_clps = '(defclass Duration (is-a Name) (slot start (type NUMBER) (pattern-match reactive)) (slot end (type NUMBER) (pattern-match reactive)))'
logger.info(_duration_clps)
clips.Build(_duration_clps)

_extract_clp = '''
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
'''
logger.info(_extract_clp)
clips.Build(_extract_clp)

_maxcomend_clp = '''
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
'''
logger.info(_maxcomend_clp)
clips.Build(_maxcomend_clp)

_mincomstart_clp = '''
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
'''
logger.info(_mincomstart_clp)
clips.Build(_mincomstart_clp)

clp = '(defclass State (is-a USER))'
logger.info(clp)
clips.Build(clp)

_set_tal = '(set-sequence-operator-recognition TRUE)'
logger.info(_set_tal)
clips.Eval(_set_tal)

_set_slots = """(defmessage-handler State set-slots primary ($?slots)
        (while (> (length$ ?slots) 0) do
            (bind ?slot (first$ ?slots))
            (bind ?vslots (rest$ ?slots))
            (bind ?value (first$ ?vslots))
            (bind ?slots (rest$ ?vslots))
            (dynamic-put $?slot $?value))
        (return (instance-name ?self)))
"""
logger.info(_set_slots)
clips.Build(_set_slots)

_add_pred ="""
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
"""
logger.info(_add_pred)
clips.Build(_add_pred)

_fact_clp = '(defclass Fact (is-a Name) (slot truth (type INTEGER) (default 1) (pattern-match reactive)) (slot subject (type INSTANCE) (pattern-match reactive)) (slot predicate (type INSTANCE) (pattern-match reactive)) (slot time (type ?VARIABLE) (pattern-match reactive)))'
logger.info(_fact_clp)
clips.Build(_fact_clp)

#def ptonl(subj, pred, time, truth):
#    """
#       (python-call ptonl ?s ?p ?t ?r)
#    """
#    key = str(subj) + str(pred) + str(time) + str(truth)
#    logger.info(key)
#    return clips.Symbol('TRUE')

#clips.RegisterPythonFunction(ptonl)

_add_prop = '''
(deffunction add-prop (?s ?p ?t ?r)
       (bind ?count 0)
       (do-for-all-instances ((?prop Fact))
                          (and (eq ?prop:subject ?s)
                               (eq ?prop:predicate ?p)
                               (or (and (eq (class ?t) Duration)
                                        (= (send (send ?prop get-time) get-start) (send ?t get-start))
                                        (= (send (send ?prop get-time) get-end) (send ?t get-end)))
                                   (eq ?prop:time ?t))
                               (= ?prop:truth ?r))
               (bind ?count (+ ?count 1)))
        (if (= ?count 0)
        then (make-instance of Fact (subject ?s)
                                           (predicate ?p)
                                           (time ?t)
                                           (truth ?r))
        else (return TRUE)))'''

logger.info(_add_prop)
clips.Build(_add_prop)

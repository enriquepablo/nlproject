# -*- coding: utf-8 -*-
# Copyright (c) 2007-2008 by Enrique Pérez Arnaud <enriquepablo@gmail.com>
#
# This file is part of ln.
#
# ln is free software: you can redistribute it and/or modify
# it under the terms of the GNU Lesser General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# ln is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU Lesser General Public License for more details.
#
# You should have received a copy of the GNU Lesser General Public License
# along with ln.  If not, see <http://www.gnu.org/licenses/>.

import clips
from nl.log import logger
from nl import conf


# CLIPS SNIPPETS
################

class_constraint = '?%(val)s&:(or (eq (class ?%(val)s) %(cls)s) (subclassp (class ?%(val)s) %(cls)s))'


# CLIPS DEFINITIONS
###################

_name_def = '(defclass Namable (is-a USER))'
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


_duration_clps = '(defclass Duration (is-a Namable) (slot start (type NUMBER) (pattern-match reactive)) (slot end (type NUMBER) (pattern-match reactive)))'
logger.info(_duration_clps)
clips.Build(_duration_clps)

_minend_clp = '''
(deffunction min-end ($?durations)
    (bind ?end (send (nth$ 1 ?durations) get-end))
    (bind ?now (python-call ptime))
    (if (= ?end -1.0) then (bind ?end ?now))
    (progn$ (?dur (rest$ ?durations))
        (bind ?this-end (send ?dur get-end))
        (if (= ?this-end -1.0) then (bind ?this-end ?now))
        (if (< ?this-end ?end)
            then (bind ?end ?this-end))
    )
    (if (= ?end ?now) then (bind ?end -1.0))
    (return ?end)
)
'''
logger.info(_minend_clp)
clips.Build(_minend_clp)

_maxstart_clp = '''
(deffunction max-start ($?durations)
    (bind ?start (send (nth$ 1 ?durations) get-start))
    (progn$ (?dur (rest$ ?durations))
        (bind ?this-start (send ?dur get-start))
        (if (> ?this-start ?start)
            then (bind ?start ?this-start))
    )
    (return ?start)
)
'''
logger.info(_maxstart_clp)
clips.Build(_maxstart_clp)

clp = '(defclass Exists (is-a USER))'
logger.info(clp)
clips.Build(clp)

_set_tal = '(set-sequence-operator-recognition TRUE)'
logger.info(_set_tal)
clips.Eval(_set_tal)

_set_slots = """(defmessage-handler Exists set-slots primary ($?slots)
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

_fact_clp = '(defclass Fact (is-a Namable) (slot truth (type INTEGER) (default 1) (pattern-match reactive)) (slot subject (type ?VARIABLE) (pattern-match reactive)) (slot predicate (type INSTANCE) (pattern-match reactive)) (slot time (type ?VARIABLE) (pattern-match reactive)))'
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

if conf.with_callback:
    import utils
    utils.load_plugins()
    callback = '(python-call factback ?s ?p ?t ?r)'
else:
    callback = ''

_add_prop = '''
(deffunction add-prop (?s ?p ?t ?r)
       (bind ?count 0)
       (do-for-all-instances ((?prop Fact))
                          (and (eq ?prop:subject ?s)
                               (eq ?prop:predicate ?p)
                               (or (and (eq (class ?t) Duration)
                                        (eq (class (send ?prop get-time)) Duration)
                                        (<= (send (send ?prop get-time) get-start) (send ?t get-start))
                                        (or (= (send (send ?prop get-time) get-end) (send ?t get-end))
                                            (and (= (send (send ?prop get-time) get-end) -1.0)
                                                (>= (python-call ptime) (send ?t get-end)))
                                            (and (= (send ?t get-end) -1.0)
                                                (<= (python-call ptime) (send (send ?prop get-time) get-end)))
                                            (and (<> (send ?t get-end) -1.0)
                                                (>= (send (send ?prop get-time) get-end) (send ?t get-end)))))
                                   (eq ?prop:time ?t))
                               (= ?prop:truth ?r))
               (bind ?count (+ ?count 1)))
        (if (= ?count 0)
            then (delayed-do-for-all-instances ((?prop Fact))
                          (and (eq ?prop:subject ?s)
                               (eq ?prop:predicate ?p)
                               (or (and (eq (class ?t) Duration)
                                        (eq (class (send ?prop get-time)) Duration)
                                        (<= (send ?t get-start) (send (send ?prop get-time) get-start))
                                        (or (= (send (send ?prop get-time) get-end) (send ?t get-end))
                                            (and (= (send ?t get-end) -1.0)
                                                (>= (python-call ptime) (send (send ?prop get-time) get-end)))
                                            (and (= (send (send ?prop get-time) get-end) -1.0)
                                                (<= (python-call ptime) (send (send ?prop get-time) get-end)))
                                            (and (<> (send (send ?prop get-time) get-end) -1.0)
                                                (>= (send ?t get-end) (send (send ?prop get-time) get-end)))))
                                   (eq ?prop:time ?t))
                               (= ?prop:truth ?r))
                        (send ?prop delete))
                 (make-instance of Fact (subject ?s)
                                           (predicate ?p)
                                           (time ?t)
                                           (truth ?r))
             %s
        else (return TRUE)))''' % callback

logger.info(_add_prop)
clips.Build(_add_prop)

_resolvetime = '''
(deffunction resolvetime (?t)
    (if (eq ?t -1.0)
        then (return (python-call ptime))
        else (return ?t)
    )
)
'''

logger.info(_resolvetime)
clips.Build(_resolvetime)


_count_sentences = '''
(deffunction count-sentences ($?sentences)
    (length$ ?sentences))
'''

_count_in_sentences = '''
(deffunction count-in-sentences ($?instancesets)
    (bind ?res (create$))
    (progn$ (?sentences ?instancesets)
        (bind ?instanceset (rest$ ?sentences))
        (bind ?not-in TRUE)
        (bind ?index 1)
        (progn$ (?already ?res)
            (bind ?equals TRUE)
            (progn$ (?instance ?instanceset)
                (bind ?second (nth$ ?index ?already))
                (if (neq ?instance ?second)
                    then (bind ?equals FALSE)
                        (BREAK)
                    )
                )
            (if ?equals
                then (bind ?not-in FALSE)
                    (BREAK)
                )
            )
        (if ?not-in
            then (insert$ ?res 1 ?instance)
            )
        )
    (return (length$ ?res))
    )
'''

_max_count = '''
(deffunction max-count (?numvars $?instance-sets)
    (bind ?maxcount 0)
    (bind ?instances (create$))
    (progn$ (?instance-set ?instance-sets)
        (bind ?not-in TRUE)
        (bind ?news (slice 1 ?numvars ?instance-set))
        (bind ?count 0)
        (progn$ (?instance ?instances)
            (bind ?new-count 0)
            (progn$ (?new ?news)
                (if (eq ?new (nth$ ?new-count ?instance))
                    then (if (eq ?new-count ?numvars)
                        then (bind ?count (nth$ 2 ?instance))
                            (insert$ ?instance 2 (+ 1 ?count))
                            (bind ?not-in FALSE)
                        )
                    else (BREAK)
                    )
                (bind ?new-count (+ 1 ?new-count))
                )
            (if (not ?not-in) (BREAK))
            )
        (if ?not-in
            then (insert$ ?instances 1 (create$ ?news 1))
            (bind ?count 1)
            )
        (if (< ?maxcount ?count)
            then (bind ?maxcount ?count)
            )
        )
    (return ?maxcount)
)
'''

_min_count = '''
(deffunction min-count ($?instance-sets)
    (bind ?instances (create$))
    (progn$ (?instance-set ?instance-sets)
        (bind ?not-in TRUE)
        (bind ?news (slice 1 ?numvars ?instance-set))
        (bind ?count 0)
        (progn$ (?instance ?instances)
            (bind ?new-count 0)
            (progn$ (?new ?news)
                (if (eq ?new (nth$ ?new-count ?instance))
                    then (if (eq ?new-count ?numvars)
                        then (bind ?count (nth$ 2 ?instance))
                            (insert$ ?instance 2 (+ 1 ?count))
                            (bind ?not-in FALSE)
                        )
                    else (BREAK)
                    )
                (bind ?new-count (+ 1 ?new-count))
                )
            (if (not ?not-in) (BREAK))
            )
        (if ?not-in
            then (insert$ ?instances 1 (create$ ?news 1))
            )
        )
    (if (= (lenght$ ?instances) 0)
        then (return 0)
        )
    (bind ?min-count (nth$ 2 (first$ ?instances)))
    (bind ?instances (rest$ ?instances))
    (progn$ (?instance ?instances)
        (bind ?count (nth$ 2 ?instance))
        (if (< ?count ?min-count)
            then (bind ?min-count ?count))
        )
    (return ?min-count)
)
'''

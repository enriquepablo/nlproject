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

# import logging
from log import logger
from nl.utils import clips, register, Name, varpat
from nl.arith import Number, Time
from nl.thing import Thing
from nl.state import State

_m = []
now = Time('now')

class Proposition(Name):
    """
    """
    clp = '(defclass Proposition (is-a Name) (slot truth (type INTEGER) (default 1) (pattern-match reactive)) (slot subject (type INSTANCE) (pattern-match reactive)) (slot predicate (type INSTANCE) (pattern-match reactive)) (slot time (type ?VARIABLE) (pattern-match reactive)))'
    logger.info(clp)
    clips.Build(clp)
    _v_clips_class = clips.FindClass('Proposition')

    def __init__(self, subj, pred, time=now, truth=1):
        self.truth = truth
        self.subject = subj
        self.predicate = pred
        self.time = isinstance(time, Number) and time or Time(time)

    def __str__(self):
        negation = not self.truth and ' not' or ''
        return '%s%s %s at %s' % (str(self.subject),
                                negation,
                                str(self.predicate),
                                str(self.time))

    @classmethod
    def from_clips(cls, instance):
        if not isinstance(instance, clips._clips_wrap.Instance):
            instance = clips.FindInstance(instance)
        s = Name.from_clips(instance.GetSlot('subject'))
        p = State.from_clips(instance.GetSlot('predicate'))
        t = Time.from_clips(instance.GetSlot('time'))
        truth = instance.GetSlot('truth')
        return Prop(s, p, t, truth=truth)

    def negate(self):
        self.truth = not self.truth and 1 or 0
        negated = str(self)
        self.truth = not self.truth and 1 or 0
        return negated

    def get_ism(self, templs, queries, vrs, newvar='prop'):
        """
        get instance-set method;
        return (instance-set templates, instance-set queries)
        """
        templs.append('(?%s %s)' % (newvar, self.__class__.__name__))
        s = self.subject.get_isc(templs, queries, vrs)
        queries.append('(eq ?%s:subject %s)' % (newvar, s))
        p = self.predicate.get_isc(templs, queries, vrs)
        queries.append('(eq ?%s:predicate %s)' % (newvar, p))
        t = self.time.get_isc(templs, queries, vrs)
        if not varpat.match(t[1:]):
            queries.append('(eq ?%s:time %s)' % (newvar, t))
        queries.append('(eq ?%s:truth %s)' % (newvar,
                                         self.truth))
        return newvar

    def get_ce(self, vrs=None):
        """
        put proposition in clips as a conditional element of a rule
        """
        ce = '(logical (object (is-a Proposition) (subject %s) (predicate %s) (time %s) (truth %s)))'
        return ce % (self.subject.get_slot_constraint(vrs),
                     self.predicate.get_slot_constraint(vrs),
                     self.time.get_slot_constraint(vrs),
                     self.truth)


    def put_action(self, vrs):
        """
        put proposition in clips as an action that makes the proposition
        """
        s = self.subject.put(vrs)
        p = self.predicate.put(vrs)
        t = self.time.put(vrs)
        return '(add-prop %s %s %s %s)' % (s, p, t, self.truth)

    def remove_action(self, vrs):
        templs = []
        queries = []
        self.get_ism(templs, queries, vrs, newvar='prop')
        if len(queries) > 1:
            q = '(do-for-instance (%s) (and %s) (unmake-instance ?prop))' % (' '.join(templs),
                                                        ' '.join(queries))
        else:
            q = '(do-for-instance (%s) %s (unmake-instance ?prop))' % (' '.join(templs),
                                                queries and queries[0] or 'TRUE')
        return q

register('Proposition', Proposition)
Prop = Proposition

#_add_prop = '(deffunction add-prop (?s ?p ?t ?r) (if (python-call ptonl ?s ?p ?t ?r) then (make-instance of Proposition (subject ?s) (predicate ?p) (time ?t) (truth ?r)) else (return TRUE)))'

_add_prop = '''
(deffunction add-prop (?s ?p ?t ?r)
       (if (and (python-call ptonl ?s ?p ?t ?r)
                (eq (length$ (find-instance ((?prop Proposition))
                     (and (eq ?prop:subject ?s)
                               (eq ?prop:predicate ?p)
                               (eq ?prop:time ?t)
                               (eq ?prop:truth ?r)))) 0))
        then (make-instance of Proposition (subject ?s)
                                           (predicate ?p)
                                           (time ?t)
                                           (truth ?r))
        else (return TRUE)))'''

clips.Build(_add_prop)
logger.info(_add_prop)

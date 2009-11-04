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
# import logging
from nl.log import logger
from nl.utils import register, Name, varpat, subclasses
from nl.arith import Number
from nl.time import Time, Instant
from nl.thing import Thing
from nl.state import State

_m = []

class Fact(Name):
    """
    """
    _v_clips_class = clips.FindClass('Fact')

    def __init__(self, subj, pred, time=Instant('now'), truth=1):
        self.truth = truth
        self.subject = subj
        self.predicate = pred
        self.time = time

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
        return Fact(s, p, t, truth=truth)

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
        templs.append((newvar, self.__class__.__name__))
        s = self.subject.get_isc(templs, queries, vrs)
        queries.append('(eq ?%s:subject %s)' % (newvar, s))
        p = self.predicate.get_isc(templs, queries, vrs)
        queries.append('(eq ?%s:predicate %s)' % (newvar, p))
        t = self.time.get_isc(templs, queries, vrs, parent=newvar)
        queries.append('(eq ?%s:truth %s)' % (newvar,
                                         self.truth))
        return newvar

    def get_ce(self, vrs=None):
        """
        put proposition in clips as a conditional element of a rule
        """
        ce = '(logical (object (is-a Fact) (subject %s) (predicate %s) (time %s) (truth %s)))'
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
            q = '(do-for-instance (%s) (and %s) (unmake-instance ?prop))' % \
                     (' '.join(['(?%s %s)' % templ for templ in templs]),
                                ' '.join(queries))
        else:
            q = '(do-for-instance (%s) %s (unmake-instance ?prop))' % \
                     (' '.join(['(?%s %s)' % templ for templ in templs]),
                                queries and queries[0] or 'TRUE')
        return q

register('Fact', Fact)

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

class Proposition(Name):
    """
    """
    clp = '(defclass Proposition (is-a USER) (slot subject (type INSTANCE)) (slot predicate (type INSTANCE)) (slot time (type ?VARIABLE)))'
    logger.info(clp)
    clips.Build(clp)
    clips_class = clips.FindClass('Proposition')
    def __init__(self, subj, pred, time):
        self.subject = subj
        self.predicate = pred
        self.time = isinstance(time, Number) and time or Time(time)

    def __str__(self):
        mods = []
        for mod,cls in self.predicate.mods.items():
            if getattr(self.predicate, mod, _m) is not _m:
              mods.append('%s=%s' % (mod, getattr(self.predicate, mod).value))
        return '%s %s %s at %s' % (self.subject.value,
                                   self.predicate.__class__.__name__,
                                   mods,
                                   self.time.value)

    @classmethod
    def from_clips(cls, instance):
        i = clips.FindInstance(instance)
        s = Thing.from_clips(i.GetSlot('subject'))
        p = State.from_clips(i.GetSlot('predicate'))
        t = Time.from_clips(i.GetSlot('time'))
        return Prop(s, p, t)

    def get_ism(self, templs, queries, newvar='prop'):
        """
        get instance-set method;
        return (instance-set templates, instance-set queries)
        """
        templs.append('(?%s %s)' % (newvar, self.__class__.__name__))
        s = self.subject.get_isc(templs, queries)
        queries.append('(eq ?%s:subject %s)' % (newvar, s))
        p = self.predicate.get_isc(templs, queries)
        queries.append('(eq ?%s:predicate %s)' % (newvar, p))
        t = self.time.get_isc(templs, queries)
        if not varpat.match(t[1:]):
            queries.append('(eq ?%s:time %s)' % (newvar,
                                         self.time.get_isc(templs, queries)))
        return newvar

    def get_ce(self, vrs=None):
        """
        put proposition in clips as a conditional element of a rule
        """
        ce = '(logical (object (is-a Proposition) (subject %s) (predicate %s) (time %s)))'
        return ce % (self.subject.get_slot_constraint(vrs),
                     self.predicate.get_slot_constraint(vrs),
                     self.time.get_slot_constraint(vrs))


    def put_action(self, vrs):
        """
        put proposition in clips as an action that makes the proposition
        """
        s = self.subject.put(vrs)
        p = self.predicate.put(vrs)
        t = self.time.put(vrs)
        return '(make-instance of Proposition (subject %s) (predicate %s) (time %s))' % (s, p, t)

register('Proposition', Proposition)
Prop = Proposition

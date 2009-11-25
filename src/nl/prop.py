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
'''
we can use classes:
    as cls (and as var within rules)
        in rules (prems and cons) and atomic facts
                as subject in fact
                as object in a predicate in fact
'''
import clips
from nl.log import logger
from nl import utils
from nl.metanl import Namable, Word
from nl.time import Time, Instant
from nl.thing import Thing
from nl.state import State

_m = []

class Fact(Namable):
    """
    A fact is built with a subj and a pred positional arguments.
    subj is either an object of Thing or any of its subclasses,
    or a class chosen among Thing, State, or any subclass of them.
    It can also take a named argument, often given as a 3rd
    positional arg, that has to be of type Time,
    and a named arg 'truth', that is either 0 or 1.
    """
    def __init__(self, subj, pred, time=Instant('now'), truth=1):
        self.truth = truth
        if isinstance(subj, str):
            if utils.varpat.match(subj):
                self.subject = Thing(subj)
            else:
                from nl import kb
                self.subject = kb.ask_obj(Thing(subj))[0]
        else:
            self.subject = subj
        if isinstance(pred, str):
            self.predicate = State(pred)
        else:
            self.predicate = pred
        if isinstance(time, Time):
            self.time = time
        else:
            self.time = Instant(time)


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
        s = instance.GetSlot('subject')
        if isinstance(instance, clips._clips_wrap.Class):
            s = Word.from_clips(s)
        else:
            s = Namable.from_clips(s)
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
        isc_meth = getattr(self.subject, 'get_isc_cls', self.subject.get_isc)
        s = isc_meth(templs, queries, vrs)
        queries.append('(eq ?%s:subject %s)' % (newvar, s))
        p = self.predicate.get_isc(templs, queries, vrs)
        queries.append('(eq ?%s:predicate %s)' % (newvar, p))
        self.time.get_isc(templs, queries, vrs, parent=newvar)
        queries.append('(eq ?%s:truth %s)' % (newvar,
                                         self.truth))
        return newvar

    def get_ce(self, vrs=None):
        """
        put proposition in clips as a conditional element of a rule
        """
        ce = '(logical (object (is-a Fact) (subject %s) (predicate %s) (time %s) (truth %s)))'
        constraint_meth = getattr(self.subject, 'clsput', self.subject.get_slot_constraint)
        s = constraint_meth(vrs)
        constraint_meth = getattr(self.predicate, 'clsput', self.predicate.get_slot_constraint)
        p = constraint_meth(vrs)
        return ce % (s, p,
                     self.time.get_slot_constraint(vrs),
                     self.truth)


    def put_action(self, vrs=None):
        """
        put proposition in clips as an action that makes the proposition
        """
        if vrs is None:
            vrs = {}
        put_meth = getattr(self.subject, 'clsput', self.subject.put)
        s = put_meth(vrs)
        put_meth = getattr(self.predicate, 'clsput', self.predicate.put)
        p = put_meth(vrs)
        t = self.time.put(vrs)
        return '(add-prop %s %s %s %s)' % (s, p, t, self.truth)


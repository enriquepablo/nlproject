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
from nl.metanl import ClassVar, ClassVarVar
from nl.nltime import Time, Instant
from nl.thing import Thing
from nl.state import Exists

_m = []

class Fact(Namable):
    """
    A fact is built with a subj and a pred positional arguments.
    subj is either an object of Thing or any of its subclasses,
    or a class chosen among Thing, Exists, or any subclass of them.
    It can also take a named argument, often given as a 3rd
    positional arg, that has to be of type Time,
    and a named arg 'truth', that is either 0 or 1.
    """
    def __init__(self, subj, pred, t=Instant('now'), truth=1):
        self.truth = truth
        if isinstance(pred, str):
            self.predicate = Exists(pred)
        else:
            if not isinstance(pred, Exists) and \
               not isinstance(pred, ClassVarVar):
                raise ValueError('The predicate of a fact has to be a verb')
            self.predicate = pred
        if isinstance(subj, str):
            if utils.varpat.match(subj):
                self.subject = self.pred.subject(subj)
            else:
                from nl import kb
                self.subject = kb.ask_obj(Thing(subj))[0]
                if not self.subject:
                    raise ValueError('If Fact receives a string as subject'
                                     'It has to be a variable or a thing'
                                     'already defined in the knowledge base')
        else:
            if not isinstance(subj, Thing) and \
               not isinstance(subj, Word) and \
               not isinstance(subj, ClassVar) and \
               not isinstance(subj, ClassVarVar):
                raise ValueError('Not a proper subject for fact')
            self.subject = subj
        if not isinstance(subj, ClassVar) and \
           not isinstance(subj, ClassVarVar) and \
               (not isinstance(self.predicate, ClassVar) and
                not isinstance(self.predicate, ClassVarVar) and
                not isinstance(subj, self.predicate.subject)):
            raise ValueError('Not a proper subject for %s' %
                              self.predicate.__class__.__name__)
        if isinstance(t, Namable):
            self.time = t
        else:
            self.time = Instant(t)


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
        p = Exists.from_clips(instance.GetSlot('predicate'))
        t = Time.from_clips(instance.GetSlot('time'))
        truth = instance.GetSlot('truth')
        return Fact(s, p, t, truth=truth)

    def negate(self):
        self.truth = not self.truth and 1 or 0
        negated = str(self)
        self.truth = not self.truth and 1 or 0
        return negated

    def get_ism(self, templs, queries, vrs, newvar='fact'):
        """
        get instance-set method;
        return (instance-set templates, instance-set queries)
        """
        templs.append((newvar, self.__class__.__name__))
        isc_meth = getattr(self.subject, 'get_isc_cls', self.subject.get_isc)
        isc_meth(queries, vrs, newvar, ('subject',))
        self.predicate.get_isc(queries, vrs, newvar, ('predicate',))
        self.time.get_isc(queries, vrs, newvar, ('time',))
        queries.append('(eq ?%s:truth %s)' % (newvar,
                                         self.truth))
        return newvar

    def get_ce(self, vrs=None):
        """
        put proposition in clips as a conditional element of a rule
        """
        if vrs is None: vrs = []
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



def factback(csubj, cpred, ctime, ctruth):
    """
    """
    try:
        subj = Namable.from_clips(csubj)
    except clips.ClipsError:
        subj = Word.from_clips(csubj)
    pred = Namable.from_clips(cpred)
    t = Time.from_clips(ctime)
    truth = int(str(ctruth))
    fact = Fact(subj, pred, t, truth=truth)
    pred.in_fact(fact)
    for plugin in utils.plugins:
        plugin(fact)

clips.RegisterPythonFunction(factback)

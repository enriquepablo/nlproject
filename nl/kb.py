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
import re
import clips

from exceptions import NlError

# vars are always XNUM
varpat = re.compile(r'^X(\d+)$')

# marker object
_m = []

# registry of subclasses
_subclasses = {}
def register(clsname, cls):
    _subclasses[clsname] = cls

def parens(expr):
    if expr[0] != '(':
        return expr
    depth = 0
    term = ''
    terms = []
    for c in expr:
        if depth == 1 and c == ' ':
            terms.append(term)
            term = ''
        elif c == '(':
            depth += 1
            if depth > 1:
                term += c
        elif c == ')':
            depth -= 1
            if depth > 0:
                term += c
        else:
            term += c
    terms.append(term)
    return terms

class Number(object):
    """
    A number can be of 4 classes: number, operation, var, & op with vars.
    A number can be in three places: in a bare prop, in a prem, in a con.
    in a prop it can be a number (and possibly an op).
    in a prem it can be in 2 places: in a prop, or in an arith condition.
    in a prem prop, it is a single number.
    in an arith prem, it is anything at all.
    in a cond, it is anything at all.
    so we need:
    put gives us prop and con; with a vrs arg that tell where to get the vals.
    get_constraint gives a var or a number, and if var and in vrs, a simple
    comparison with the referenced mod of the vrs.
    and we need an ArithPred that gives us CEs. with vrs.
    (double check the newvar passing).
    """
    def __init__(self, value, arg1='', arg2=''):
        try:
            self.value = str(int(value))
        except ValueError:
            if value[0] == '(':
                args = parens(value)
                self.value = args[0]
                self.arg1 = Number(args[1])
                self.arg2 = Number(args[2])
            else:
                self.value = value
                self.arg1 = arg1
                self.arg2 = arg2


    def get_slot_constraint(self, vrs):
        """
        in a make-instance of a proposition
        """
        if varpat.match(self.value):
            return '?%s' % self.value
        try:
            return str(int(self.value))
        except ValueError:
            if self.value == 'now':
                return 'now'
            arg1 = self.arg1.get_slot_constraint(vrs)
            arg2 =  self.arg2.get_slot_constraint(vrs)
            return '(%s %s %s)' % (self.value, arg1, arg2)

    def put(self):
        return self.get_slot_constraint(_m)

    def get_isc(self, templs, queries):
        """
        get instance-set condition;
        return (instance-set templates, instance-set queries)
        """
        return self.put()

register('Number', Number)


class Arith(Number):
    """
    Arithmetic predicate
    """
    def __init__(self, value, arg1='', arg2=''):
        if value[0] == '(':
            args = parens(value)
            self.value = args[0]
            self.arg1 = Number(args[1])
            self.arg2 = Number(args[2])
        else:
            self.value = value
            self.arg1 = arg1
            self.arg2 = arg2

    def get_ce(self, vrs):
        arg1 = self.arg1.put()
        arg2 = self.arg2.put()
        return '(test (%s %s %s))' % (self.value, arg1, arg2)

    def get_isc(self, templs, queries):
        arg1 =  self.arg1.put()
        arg2 =  self.arg2.put()
        queries.append('(%s %s %s)' % (self.value, arg1, arg2))

register('Arith', Arith)


class _Name(object):
    """
    """
    clips_class = clips.USER_CLASS.BuildSubclass('Name')

    def __init__(self, value):
        self.value = value


class MetaThing(type):
    """
    When Name is extended, this adds 1 defclass to clips
    creating a subclass of Name.
    And registers the class in _subclasses
    """
    def __init__(cls, classname, bases, newdict):
        super(MetaThing, cls).__init__(classname, bases, newdict)
        cls.clips_class = bases[0].clips_class.BuildSubclass(classname)
        register(classname, cls)

# XXX ponerle adjetivos a thing
class Thing(_Name):
    """
    """
    __metaclass__ = MetaThing

    @classmethod
    def from_clips(cls, instance):
        inst = clips.FindInstance(instance)
        cls = _subclasses[str(inst.Class.Name)]
        return cls(str(inst))

    def __str__(self):
        return '%s is a %s' % (self.value, self.__class__.__name__)

    def get_ce(self, vrs):
        """
        build CE for clips
        """
        if varpat.match(self.value):
            vrs.append(self.value)
            return '(logical (object (is-a %s)(name ?%s)))' % (self.__class__.__name__,
                                     self.value)
        return '(logical (object (is-a %s)(name %s)))' % (self.__class__.__name__,
                                    self.value)

    def get_slot_constraint(self, vrs):
        """
        build rule CE constraint for clips
        for a slot constraint for a prop in a rule
        """
        if varpat.match(self.value):
            if self.value in vrs:
                return '?%s' % self.value
            else:
                vrs.append(self.value)
                return '?%(val)s&:(superclassp %(cls)s (class ?%(val)s))|:(eq %(cls)s (class ?%(val)s))' % {'val': self.value, 'cls': self.__class__.__name__}
        else:
            return '[%s]' % self.value

    def put_action(self):
        """
        put name in clips as a make-instance action.
        """
        val = varpat.match(self.value) and '?%s' % self.value or '[%s]' % self.value
# XXX chequear que no existe y no pertenece ya a una clase más específica. Entonces se podrán contruir props con nombres nuevos, y unificar los put
        return '(make-instance %s of %s)' % (val, self.__class__.__name__)

    def put(self):
        return self.get_slot_constraint([self.value])

    def get_isc(self, templs, queries):
        """
        get instance-set condition;
        return (instance-set templates, instance-set queries)
        """
        if varpat.match(self.value):
            templs.append('(?%s %s)' % (self.value, self.__class__.__name__))
            return '?%s' % self.value
        else:
            return '[%s]' % self.value

    def get_ism(self,  templs, queries, newvar='sen'):
        """
        get instance-set method;
        return (instance-set templates, instance-set queries)
        """
        if varpat.match(self.value):
            templs.append('(?%s %s)' % (self.value, self.__class__.__name__))
        else:
            templs.append('(?%s %s)' % (newvar, self.__class__.__name__))
            queries.append('(eq ?%s [%s])' % (newvar, self.value))

register('Thing', Thing)


class MetaState(type):
    """
    When State is extended, this registers the class in _subclasses
    """
    def __init__(cls, classname, bases, newdict):
        super(MetaState, cls).__init__(classname, bases, newdict)
        register(classname, cls)


class State(object):
    """
    """
    __metaclass__ = MetaState

    mods = {}

    def __init__(self, **kwargs):
        for mod,cls in self.mods.items():
            if kwargs.get(mod, _m) is not _m:
                if isinstance(kwargs[mod], cls):
                    setattr(self, mod, kwargs[mod])
                else:
                    setattr(self, mod, cls(kwargs[mod]))
            else:
                raise NlError("wrong modifier for verb")

    @classmethod
    def from_clips(cls, instance):
        clsname = str(instance[0])
        instance = instance[1:]
        cls = _subclasses[clsname]
        kwargs = {}
        for mod,mcls in cls.mods.items():
            kwargs[mod] = mcls(str(instance[0]))
            instance = instance[1:]
        return cls(**kwargs)

    def put(self):
        """
        put predicate in clips, as part of a proposition
        in a make-instance
        """
        return ' '.join([self.__class__.__name__] + [getattr(self, mod).put() for mod in self.mods.keys()])

    def get_slot_constraint(self, vrs):
        """
        put predicate in clips, as part of a proposition
        in a conditional element in rule or ism
        """
        return ' '.join([self.__class__.__name__] + [getattr(self, mod).get_slot_constraint(vrs) for mod in self.mods.keys()])

    def get_isc(self, templs, queries):
        """
        get instance-set condition;
        return (instance-set templates, instance-set queries)
        """
        mods = [self.__class__.__name__]
        for mod,mcls in self.mods.items():
            m = getattr(self, mod)
            mods.append(m.get_isc(templs, queries))
        return mods




class Time(Number):
    """
    """

    @classmethod
    def from_clips(cls, instance):
        return Time(instance)


class Proposition(object):
    """
    """
    clips_class = clips.USER_CLASS.BuildSubclass('Proposition',
           '(slot subject (type INSTANCE)) (multislot predicate) (slot time (type ?VARIABLE))')
    def __init__(self, subj, pred, time):
        self.subject = subj
        self.predicate = pred
        self.time = isinstance(time, Number) and time or Time(time)

    def __str__(self):
        mods = []
        for mod,cls in self.predicate.mods.items():
            mods.append('%s=%s' % (mod, getattr(self.predicate, mod).value))
        return '%s %s %s at %s' % (self.subject.value,
                                   self.predicate.__class__.__name__,
                                   mods,
                                   self.time.value)

    @classmethod
    def from_clips(cls, instance):
        s = clips.FindInstance(instance)
        s = Thing.from_clips(s.GetSlot('subject'))
        p = clips.FindInstance(instance)
        p = State.from_clips(p.GetSlot('predicate'))
        t = clips.FindInstance(instance)
        t = Time.from_clips(t.GetSlot('time'))
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
        for n,t in enumerate(p):
            if not varpat.match(t[1:]):
                queries.append('(eq (nth$ %d ?%s:predicate) %s)' % (n+1, newvar, t))
        t = self.time.get_isc(templs, queries)
        if not varpat.match(t[1:]):
            queries.append('(eq ?%s:time %s)' % (newvar, self.time.get_isc(templs, queries)))
        return newvar

    def get_ce(self, vrs=None):
        """
        put proposition in clips as a conditional element of a rule
        """
        if vrs is None:
            vrs = []
        return '(logical (object (is-a Proposition)(subject %s)(predicate %s)(time %s)))' % (self.subject.get_slot_constraint(vrs),
                                self.predicate.get_slot_constraint(vrs),
                                self.time.get_slot_constraint(vrs))


    def put_action(self):
        """
        put proposition in clips as an action that makes the proposition
        """
        return '(make-instance of Proposition (subject %s)(predicate %s)(time %s))' % (self.subject.put(),
                                                    self.predicate.put(),
                                                  self.time.put())

Prop = Proposition

class Rule(object):
    """
    """
    def __init__(self, name, prems, cons):
        self.name = name
        self.prems = prems
        self.cons = cons

    def put_action(self):
        """
        put rule in clips
        """
        vrs = []
        cprems = [prem.get_ce(vrs) for prem in self.prems]
        ccons = [con.put_action() for con in self.cons]
        return '(defrule %s %s => %s)' % (self.name,
                                          ' '.join(cprems),
                                          ' '.join(ccons))

#     def put_ism(self):
#         vrs = {}
#         templs = []
#         queries = []
#         cons = []
#         for p in self.prems:
#             p.get_isc(vrs, templs, queries)
#         for c in self.cons:
#             cons.append(c.put_isc(vrs))
#         if len(queries) > 1:
#             return '(do-for-all-instances (%s) (and %s) %s)' % (' '.join(templs), ' '.join(queries), ' '.join(cons))
#         else:
#             return '(do-for-all-instances (%s) %s %s)' % (' '.join(templs), ' '.join(queries), ' '.join(cons))


def tell(sentence):
    s = sentence.put_action()
    # f = open('clips', 'a')
    # f.write(s+'\n')
    # f.close()
    if isinstance(sentence, Rule):
        clips.Build(s)
    else:
        clips.Eval(s)

def get_instancesn(sentence):
    templs = []
    queries = []
    sentence.get_ism(templs, queries)
    if len(queries) > 1:
        q = '(find-all-instances (%s) (and %s))' % (' '.join(templs), ' '.join(queries))
    else:
        q = '(find-all-instances (%s) %s)' % (' '.join(templs), queries and queries[0] or 'TRUE')
    return q

def get_instances(sentence):
    templs = []
    queries = []
    sentence.get_ism(templs, queries)
    if len(queries) > 1:
        q = '(find-all-instances (%s) (and %s))' % (' '.join(templs), ' '.join(queries))
    else:
        q = '(find-all-instances (%s) %s)' % (' '.join(templs), queries and queries[0] or 'TRUE')
    return clips.Eval(q)

def retract(sentence):
    for ins in get_instances(sentence):
        clips.FindInstance(ins).Remove()


def ask(sentence):
    clps = get_instances(sentence)
    if clps:
        sens = []
        for ins in clps:
            if isinstance(sentence, Thing):
                sens.append(str(Thing.from_clips(ins)))
            else:
                sens.append(str(Proposition.from_clips(ins)))
        return "\n".join(sens)
    else:
        return 'no'


def ask_objs(sentence):
    clps = get_instances(sentence)
    if clps:
        sens = []
        for ins in clps:
            if isinstance(sentence, Thing):
                sens.append(Thing.from_clips(ins))
            else:
                sens.append(Proposition.from_clips(ins))
        return sens
    else:
        return 'no'

def extend():
    return clips.Run()

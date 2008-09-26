# -*- coding: utf-8 -*-

from __future__ import with_statement
import logging
import re
import time
import clips

from exceptions import NlError

# vars are always XN+
varpat = re.compile(r'^X(\d+)$')

# marker object
_m = []

_newvar_lock = False
_newvar_num = 0

def get_newvar():
    global _newvar_lock, _newvar_num
    try:
        while _newvar_lock:
            time.sleep(0.1)
        else:
            _newvar_lock = True
            var = 'Y%d' % _newvar_num
            _newvar_num += 1
            return var
    finally:
        _newvar_lock = False


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
            self.value = value
            self.arg1 = arg1
            self.arg2 = arg2

    @classmethod
    def from_clips(cls, instance):
        val = instance.GetSlot('value')
        try:
            return cls(str(int(val)))
        except ValueError:
            if val == 'now':
                return cls(val)
            a1 = Number.from_clips(instance.GetSlot('arg1'))
            a2 = Number.from_clips(instance.GetSlot('arg2'))
            return cls(val, arg1=a1, arg2=a2)

    def get_constraint(self, vrs, newvar):
        if varpat.match(self.value):
            return '?%s' % self.value
        return self.value


    def put_isc(self, vrs):
        if varpat.match(self.value):
            return '?%s' % self.value
        try:
            return str(int(self.value))
        except ValueError:
            if self.value == 'now':
                return 'now'
            arg1 = isinstance(self.arg1, str) and self.arg1 or self.arg1.put_isc(vrs)
            arg2 = isinstance(self.arg2, str) and self.arg2 or self.arg2.put_isc(vrs)
            return '(%s %s %s)' % (self.value, arg1, arg2)


    def put(self, vrs):
        if varpat.match(self.value):
            if self.value in vrs.keys():
                if vrs[self.value][1]:
                    return '(send ?%(pred)s get-%(oldmod)s)' % {'pred': vrs[self.value][0], 'oldmod': vrs[self.value][1]}
                else:
                    return '?%s' % vrs[self.value][0]
            vrs[self.value] = [self.value, '']
            return '?%s' % self.value
        try:
            return str(int(self.value))
        except ValueError:
            if self.value == 'now':
                return 'now'
            arg1 = isinstance(self.arg1, str) and self.arg1 or self.arg1.put(vrs)
            arg2 = isinstance(self.arg2, str) and self.arg2 or self.arg2.put(vrs)
            return '(%s %s %s)' % (self.value, arg1, arg2)


class Arith(Number):
    """
    Arithmetic predicate
    """
    def __init__(self, value, arg1='', arg2=''):
        self.value = value
        self.arg1 = arg1
        self.arg2 = arg2

    def get_ce(self, vrs, newvar):
        arg1 = isinstance(self.arg1, str) and self.arg1 or self.arg1.put(vrs)
        arg2 = isinstance(self.arg2, str) and self.arg2 or self.arg2.put(vrs)
        return '(test (%s %s %s))' % (self.value, arg1, arg2)

    def get_isc(self, vrs, templs, queries):
        arg1 = isinstance(self.arg1, str) and self.arg1 or self.arg1.put_isc(vrs)
        arg2 = isinstance(self.arg2, str) and self.arg2 or self.arg2.put_isc(vrs)
        queries.append('(%s %s %s)' % (self.value, arg1, arg2))



class _Name(object):
    """
    """
    clips_class = clips.USER_CLASS.BuildSubclass('Name', '(slot value)')

    def __init__(self, value):
        self.value = value


class MetaThing(type):
    """
    When Name is extended, this adds 1 defclass to clips
    creating a subclass of Name.
    """
    def __init__(cls, classname, bases, newdict):
        super(MetaThing, cls).__init__(classname, bases, newdict)
        cls.clips_class = bases[0].clips_class.BuildSubclass(classname)

# XXX ponerle adjetivos a thing
class Thing(_Name):
    """
    """
    __metaclass__ = MetaThing

    @classmethod
    def from_clips(cls, instance):
        return cls(instance.GetSlot('value'))

    def get_isc(self, vrs, templs, queries):
        """
        get instance-set condition;
        return (instance-set templates, instance-set queries)
        """
        newvar = get_newvar()
        templs.append('(?%s %s)' % (newvar, self.__class__.__name__))
        if varpat.match(self.value):
            if self.value in vrs.keys():
                queries.append('(eq ?%s:value ?%s:value)' % (vrs[self.value], newvar))
            else:
                vrs[self.value] = newvar
        else:
            queries.append('(eq ?%s:value %s)' % (newvar, self.value))
        return newvar


    def get_ce(self, vrs, newvar):
        """
        build CE for clips
        """
        m = varpat.match(self.value)
        if m and self.value not in vrs:
            vrs[self.value] = [self.value, '']
        return '(object (is-a %s)(value %s))' % (self.__class__.__name__,
                                    m and '?%s' % m.group(0) or self.value)

    def get_constraint(self, vrs, newvar):
        """
        build rule CE constraint for clips
        for a slot constraint for a prop in a rule
        """
        constraint = self.value
        m = varpat.match(constraint)
        if m:
            var = m.group(0)
            if var in vrs.keys():
                if vrs[var][1] == 'value':
                    constraint = '?%(newvar)s&:(eq (send ?%(newvar)s get-value) (send ?%(var)s get-value))' % {'var': var, 'newvar': newvar}
                elif vrs[var][1]:
                    constraint = '?%(newvar)s&:(eq (send ?%(newvar)s get-value) (send (send ?%(var)s get-%(mod)s) get-value)' % {'var': vrs[var][0], 'newvar': newvar, 'mod': vrs[var][1]}
                else:
                    constraint = '?%(newvar)s&:(eq (send ?%(newvar)s get-value) ?%(var)s)' % {'var': var, 'newvar': newvar}
            else:
                constraint = '?%s' % var
                vrs[var] = [var, 'value']
        else:
            constraint = '?%(newvar)s&:(eq (send ?%(newvar)s get-value) %(var)s)' % {'var': constraint, 'newvar': newvar}
        return constraint

    def put(self, vrs):
        """
        put name in clips
        """
        m = varpat.match(self.value)
        if m:
            if vrs[self.value][1]=='value':
                return '(make-instance of %s (value (send ?%s get-value)))' % (self.__class__.__name__, vrs[self.value][0])
            elif vrs[self.value][1]:
                return '(make-instance of %s (value (send (send ?%s get-%s) get-value)))' % (self.__class__.__name__, vrs[self.value][0], vrs[self.value][1])
            else:
                return '(make-instance of %s (value ?%s))' % (self.__class__.__name__, vrs[self.value][0])
        return '(make-instance of %s (value %s))' % (self.__class__.__name__,
                                    self.value)

    def put_isc(self, vrs):
        """
        put name in clips
        """
        if varpat.match(self.value):
            return '(make-instance of %s (value (send ?%s get-value)))' % (self.__class__.__name__, vrs[self.value])
        return '(make-instance of %s (value %s))' % (self.__class__.__name__,
                                    self.value)


class _Verb(object):
    """
    """
    clips_class = clips.USER_CLASS.BuildSubclass('Verb')


class MetaState(type):
    """
    When Name is extended, this adds 1 defclass to clips
    creating a subclass of Verb.
    """
    def __init__(cls, classname, bases, newdict):
#         slots = []
#         for mod,cls in self.mods.items():
#             # build slot for clips class
#             if Number in cls.mro():
#                 tipo = 'NUMBER'
#             else:
#                 tipo = 'INSTANCE'
#             slots.append('(slot %s (type %s))' % (mod, tipo))
        slots = ['(slot %s (type %s))' % (mod,
                           issubclass(modclass, Number) and '?VARIABLE' or 'INSTANCE')
                  for mod,modclass in cls.mods.items()]
        slots = ' '.join(slots)
        cls.clips_class = bases[0].clips_class.BuildSubclass(classname,
                                    '%s' % slots)


class State(_Verb):
    """
    """
    __metaclass__ = MetaState

    mods = {}

    def __init__(self, **kwargs):
        for mod,cls in self.mods.items():
            if kwargs.get(mod, _m) is not _m and isinstance(kwargs[mod], cls):
                setattr(self, mod, kwargs[mod])
            else:
                raise NlError("wrong modifier for verb")

    @classmethod
    def from_clips(cls, instance):
        kwargs = {}
        for mod,mcls in cls.mods.items():
            kwargs[mod] = mcls.from_clips(instance.GetSlot(mod))
        return cls(**kwargs)

    def get_isc(self, vrs, templs, queries):
        """
        get instance-set condition;
        return (instance-set templates, instance-set queries)
        """
        newvar = get_newvar()
        templs.append('(?%s %s)' % (newvar, self.__class__.__name__))
        for mod,mcls in self.__class__.mods.items():
            val = getattr(self, mod)
            var = val.value
            if issubclass(mcls, Thing):
                mnewvar = get_newvar()
                templs.append('(?%s %s)' % (mnewvar, mcls.__name__))
                queries.append('(eq ?%s:%s ?%s)' % (newvar, mod, mnewvar))
                if varpat.match(var):
                    if var in vrs.keys():
                        queries.append('(eq ?%s:value ?%s:value)' % (vrs[var], mnewvar))
                    else:
                        vrs[var] = mnewvar
                else:
                    queries.append('(eq ?%s:value %s)' % (mnewvar, var))
            else:
                if varpat.match(var):
                    queries.append('(eq ?%s:%s ?%s)' % (newvar, mod, var))
                else:
                    queries.append('(eq ?%:%s %s)' % (newvar, mod, var))
        return newvar



    def get_constraint(self, vrs, newvar):
        """
        build rule CE constraint for clips for a prop in a rule
        """
        constraint = '?%(newvar)s&:(superclassp %(sclass)s (class ?%(newvar)s))|:(eq %(sclass)s (class ?%(newvar)s))' % {'newvar': newvar,
                                                'sclass': self.__class__.__name__}
        constraints = []
        for mod,cls in self.mods.items():
            val = getattr(self, mod)
            var = val.value
            m = varpat.match(var)
            if m:
                if var not in vrs.keys():
                    vrs[var] = [newvar, mod]
                else:
                    if vrs[var][1] == 'value':
                        constraints.append('&:(eq (send ?%(newvar)s get-%(mod)s) (send ?%(prev)s get-value))' % {'newvar': newvar, 'mod': mod, 'prev': vrs[var][0]})
                    elif vrs[var][1]:
                        constraints.append('&:(eq (send ?%(newvar)s get-%(mod)s) (send (send ?%(prev)s get-%(oldmod)s) get-value))' % {'newvar': newvar, 'mod': mod, 'prev': vrs[var][0], 'oldmod': vrs[var][1]})
                    else:
                        if issubclass(cls, Thing):
                            constraints.append('&:(eq (send ?%(newvar)s get-%(mod)s) ?%(prev)s)' % {'newvar': newvar, 'mod': mod, 'prev': vrs[var][0]})
                        else:
                            constraints.append('&:(eq (send ?%(newvar)s get-%(mod)s) (send ?%(prev)s get-%(oldmod)s))' % {'newvar': newvar, 'mod': mod, 'prev': vrs[var][0], 'oldmod': vrs[var][1]})
            else:
                context = {'newvar': newvar, 'mod': mod, 'var': var}
                if issubclass(cls, Number):
                    constraints.append(
            '&:(eq (send ?%(newvar)s get-%(mod)s) %(var)s)' % context)

                else:
                    constraints.append(
      '&:(eq (send (send ?%(newvar)s get-%(mod)s) get-value) %(var)s)' % context)
        return '%s%s' % (constraint, ''.join(constraints))

    def put(self, vrs):
        """
        put predicate in clips
        """
        slots = []
        for mod,cls in self.mods.items():
            mod_o = getattr(self, mod)
            slot = mod_o.value
            if issubclass(cls, Thing):
                if varpat.match(slot):
                    if vrs[slot][1] == 'value':
                        slot = '(make-instance of %s (value (send ?%s get-value)))' % (cls.__name__, vrs[slot][0])
                    elif vrs[slot][1]:
                        slot = '(make-instance of %s (value (send (send ?%s get-%s) get-value)))' % (cls.__name__, vrs[slot][0], vrs[slot][1])
                    else:
                        slot = '(make-instance of %s (value ?%s))' % (cls.__name__, vrs[slot][0])
                else:
                    slot = '(make-instance of %s (value %s))' % (cls.__name__, slot)
            elif varpat.match(slot):
                if vrs[slot][1]:
                    slot = '(send ?%s get-%s)' % (vrs[slot][0], vrs[slot][1])
                else:
                    slot = '?%s' % vrs[slot][0]
            else:
                try:
                    slot = str(int(slot))
                except ValueError:
                    slot = mod_o.put(vrs)
            slots.append('(%s %s)' % (mod, slot))
        return '(make-instance of %s %s)' % (self.__class__.__name__,
                                               ' '.join(slots))

    def put_isc(self, vrs):
        """
        put predicate in clips
        """
        slots = []
        for mod,cls in self.mods.items():
            mod_o = getattr(self, mod)
            slots.append('(%s %s)' % (mod, mod_o.put_isc(vrs)))
        return '(make-instance of %s %s)' % (self.__class__.__name__,
                                               ' '.join(slots))


class Time(Number):
    """
    """

    def get_constraint(self, vrs, newvar):
        constraint = self.value
        m = varpat.match(constraint)
        if m:
            var = m.group(0)
            if var in vrs.keys():
                if not vrs[var][1]:
                    return '?%(newvar)s&:(eq ?%(newvar)s ?%(var)s)' % {'var': var, 'newvar': newvar}
                else:
                    return '?%(newvar)s&:(eq ?%(newvar)s (send ?%(var)s get-%(mod)s))' % {'var': vrs[var][0], 'newvar': newvar, 'mod': vrs[var][1]}
            else:
                vrs[var] = [var, '']
                return '?%s' % var
        try:
            return str(int(constraint)) # no se pueden poner operaciones en numeros en premisas, hay que pasarlo todo a arith preds.
        except ValueError:
            if constraint == 'now':
                return 'now'
            raise NlError("Can't use operators in premises; put operations as arithmetic predicates.")
        #    return '(%s %s %s)' % (constraint, self.arg1.get_constraint(), self.arg2.get_constraint())


class Proposition(object):
    """
    """
    clips_class = clips.USER_CLASS.BuildSubclass('Proposition',
           '(slot subject (type INSTANCE)) (slot predicate (type INSTANCE)) (slot time (type ?VARIABLE))')
    def __init__(self, subj, pred, time):
        self.subject = subj
        self.predicate = pred
        self.time = time

    @classmethod
    def from_clips(cls, instance):
        s = Thing.from_clips(instance.GetSlot('subject'))
        p = State.from_clips(instance.GetSlot('predicate'))
        t = Time.from_clips(instance.GetSlot('time'))
        return Prop(s, p, t)

    def get_isc(self, vrs, templs, queries):
        """
        get instance-set condition;
        return (instance-set templates, instance-set queries)
        """
        newvar = get_newvar()
        templs.append('(?%s %s)' % (newvar, self.__class__.__name__))
        s = self.subject.get_isc(vrs, templs, queries)
        queries.append('(eq ?%s:subject ?%s)' % (newvar, s))
        p = self.predicate.get_isc(vrs, templs, queries)
        queries.append('(eq ?%s:predicate ?%s)' % (newvar, p))
        queries.append('(eq ?%s:time %s)' % (newvar, self.time.put_isc(vrs)))
        return newvar

    def get_ce(self, vrs, newvar):
        return '(object (is-a Proposition)(subject %s)(predicate %s)(time %s))' % (self.subject.get_constraint(vrs, 's'+newvar),
                                self.predicate.get_constraint(vrs, 'p'+newvar),
                                self.time.get_constraint(vrs, 't'+newvar))

    def put(self, vrs):
        """
        put proposition in clips
        """
        return '(make-instance of Proposition (subject %s)(predicate %s)(time %s))' % (self.subject.put(vrs),
                                                    self.predicate.put(vrs),
                                                  self.time.put(vrs))

    def put_isc(self, vrs):
        """
        put proposition in clips
        """
        return '(make-instance of Proposition (subject %s)(predicate %s)(time %s))' % (self.subject.put_isc(vrs),
                                                    self.predicate.put_isc(vrs),
                                                  self.time.put_isc(vrs))

Prop = Proposition

class Rule(object):
    """
    """
    def __init__(self, name, prems, cons):
        self.name = name
        self.prems = prems
        self.cons = cons

    def put(self, vrs=None):
        """
        put rule in clips
        """
        if vrs is None:
            vrs = {}
        cprems = []
        for n, prem in enumerate(self.prems):
            cprems.append(prem.get_ce(vrs, self.name+str(n)))
        ccons = [con.put(vrs) for con in self.cons]
        return '(defrule %s %s => %s)' % (self.name,
                                          ' '.join(cprems),
                                          ' '.join(ccons))

    def put_ism(self):
        vrs = {}
        templs = []
        queries = []
        cons = []
        for p in self.prems:
            p.get_isc(vrs, templs, queries)
        for c in self.cons:
            cons.append(c.put_isc(vrs))
        if len(queries) > 1:
            return '(do-for-all-instances (%s) (and %s) %s)' % (' '.join(templs), ' '.join(queries), ' '.join(cons))
        else:
            return '(do-for-all-instances (%s) %s %s)' % (' '.join(templs), ' '.join(queries), ' '.join(cons))


def tell(sentence):
    s = sentence.put({})
    logging.info(s)
    with open('clips', 'a') as f:
        f.write(s+'\n')
    if isinstance(sentence, Rule):
        clips.Build(s)
    else:
        clips.Eval(s)

def retract(sentence): # XXX logical
    pass

def ask(sentence):
    templs = []
    queries = []
    isc = sentence.get_isc({}, templs, queries)
    if len(queries) > 1:
        return clips.Eval('(find-all-instances (%s) (and %s))' % (' '.join(templs), ' '.join(queries)))
    else:
        return clips.Eval('(find-all-instances (%s) %s)' % (' '.join(templs), queries and queries[0] or 'TRUE'))

def extend():
    return clips.Run()

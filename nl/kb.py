import logging
import re
import clips

from exceptions import LnError

# vars are always XN+
varpat = re.compile(r'^X(\d+)$')

_m = []


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


    def get_ce(self, vrs, newvar):
        """
        build CE for clips
        """
        m = varpat.match(self.value)
        if m and self.value not in vrs:
            vrs[self.value] = [self.value, 'value']
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
                else:
                    constraint = '?%(newvar)s&:(eq (send ?%(newvar)s get-value) (send (send ?%(var)s get-%(mod)s) get-value)' % {'var': vrs[var][0], 'newvar': newvar, 'mod': vrs[var][1]}
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
            else:
                return '(make-instance of %s (value (send (send ?%s get-%s) get-value)))' % (self.__class__.__name__, vrs[self.value][0], vrs[self.value][1])
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
                raise LnError("wrong modifier for verb")


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
                        constraints.append('&:(eq (send ?%(newvar)s, get-%(mod)s) (send ?%(prev)s get-value))' % {'newvar': newvar, 'mod': mod, 'prev': vrs[var][0]})
                    else:
                        if issubclass(cls, Thing):
                            constraints.append('&:(eq (send ?%(newvar)s, get-%(mod)s) (send (send ?%(prev)s get-%(oldmod)s) get-value))' % {'newvar': newvar, 'mod': mod, 'prev': vrs[var][0], 'oldmod': vrs[var][1]})
                        else:
                            constraints.append('&:(eq (send ?%(newvar)s, get-%(mod)s) (send ?%(prev)s get-%(oldmod)s))' % {'newvar': newvar, 'mod': mod, 'prev': vrs[var][0], 'oldmod': vrs[var][1]})
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
                    else:
                        slot = '(make-instance of %s (value (send (send ?%s get-%s) get-value)))' % (cls.__name__, vrs[slot][0], vrs[slot][1])
                else:
                    slot = '(make-instance of %s (value ?%s))' % (cls.__name__, slot)
            elif varpat.match(slot):
                if vrs[slot][1]:
                    slot = '(send ?%s get-%s)' % (vrs[slot][0], vrs[slot][1])
                else:
                    slot = vrs[slot][0]
            try:
                slot = str(int(slot))
            except ValueError:
                slot = mod_o.put(vrs)
            slots.append('(%s %s)' % (mod, slot))
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
            raise LnError("Can't use operators in premises; put operations as arithmetic predicates.")
        #    return '(%s %s %s)' % (constraint, self.arg1.get_constraint(), self.arg2.get_constraint())


class Proposition(object):
    """
    """
    def __init__(self, subj, pred, time):
        self.subject = subj
        self.predicate = pred
        self.time = time

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

Prop = Proposition

class Rule(object):
    """
    """
    def __init__(self, name, prems, cons):
        self.name = name
        self.prems = prems
        self.cons = cons

    def put(self):
        """
        put rule in clips
        """
        vrs = {}
        cprems = []
        for n, prem in enumerate(self.prems):
            cprems.append(prem.get_ce(vrs, self.name+str(n)))
        ccons = [con.put(vrs) for con in self.cons]
        return '(defrule %s => %s)' % (' '.join(cprems), ' '.join(ccons))


def tell(sentence):
    s = sentence.put()
    logging.info(s)
    clips.Build(s)

def retract(sentence):
    pass

def ask(sentence):
    pass

def extend():
    clips.run()

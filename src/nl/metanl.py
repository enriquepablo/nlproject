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
from nl import utils
from nl.clps import class_constraint


# marker object
_m = []

class Word(type):
    """
    When Namable is extended,
    __init__ adds 1 defclass to clips
    creating a subclass of Namable.
    And registers the class in utils.subclasses.
    If utils.varpat matches the classname,
    however, __new__ builds a ClassVar intance
    and returns it, thus bypassing __init__.
    In that case, bases can be a subclass of
    Exists or Thing.
    """
    def __new__(cls, classname, bases=None, newdict=None):

        if utils.varpat.match(classname):
            if bases:
                return ClassVar(classname, bases)
            elif getattr(cls, 'cls', False):
                return ClassVar(classname, cls.cls)
            return ClassVar(classname, utils.get_class('Namable'))
        cls.value = ''
        return super(Word, cls).__new__(cls, classname, bases, newdict)

    def __init__(cls, classname, bases, newdict, clp=''):
        super(Word, cls).__init__(classname, bases, newdict)
        if clp:
            logger.info(clp)
            clips.Build(clp)
        utils.register(classname, cls)

    def clsput(self, vrs, ancestor=None, mod_path=None):
        '''
        return clips snippet to be used
        when the class is the subject in a fact
        or a mod in a predicate
        and the sentence is being asserted
        or is in the head of a rule.
        Also when the class is used as subject
        of a fact in the tail of a rule.
        '''
        return self.__name__

    def get_constraint_cls(self, vrs, ancestor=None, mod_path=None):
        '''
        return clips snippet to be used
        when the class is a mod in a predicate
        and the sentence is in the tail of a rule.
        '''
        ci = utils.clips_instance(ancestor, mod_path)
        return '&:(eq %s %s)' % (self.__name__, ci)

    def get_isc_cls(self, templs, queries, vrs):
        """
        build clips snippet
        to be used when the class is the subject in a fact
        or a mod in a predicate
        and the sentence is in a query.
        return (instance-set templates, instance-set queries)
        """
        return self.__name__

    @classmethod
    def from_clips(cls, instance):
        '''
        build nl instance starting from a clips instance
        or instance name
        '''
        if not isinstance(instance, clips._clips_wrap.Class):
            instance = clips.FindClass(instance)
        clsname = str(instance.Name)
        return utils.get_class(clsname)

utils.register('Word', Word)

class ClassVar(object):
    '''
    Used in rules, in the head or tail,
    as subject or mod in a predicate,
    when Word is called with a string
    that matches varpat.
    Intances can be called, and give back
    an instance of ClassVarVar,
    that can handle clips for
    the case in which we have both
    the class and the name or mods
    as variables.

    This is not to be part of the public api
    '''
    def __init__(self, var, cls=None):
        self.value = var
        self.cls = cls and cls or utils.get_class('Namable')

    def __call__(self, var='', **kwargs):
        if utils.varpat.match(self.value):
            return ClassVarVar(self.value, self.cls, var, **kwargs)
        return self.cls(self.value)

    def get_constraint(self, vrs, ancestor, mod_path):
        '''
        return clips snippet to be used
        when the class var is a mod in a predicate
        and the sentence is in the tail of a rule.
        '''
        ci = utils.clips_instance(ancestor, mod_path)
        if self.value in vrs:
            return self.cls(self.value).get_constraint(vrs, ancestor, mod_path)
        else:
            vrs[self.value] = (ancestor, mod_path)
            return '''&:(or (eq %(ci)s %(cls)s)
                            (subclassp %(ci)s %(cls)s))''' % {
                                    'ci': ci,
                                    'cls': self.cls.__name__}

    def get_slot_constraint(self, vrs):
        """
        return clips snippet to be used
        when the class var is predicate or subject
        and the sentence is in the tail of a rule.
        """
        if self.value in vrs:
            return self.cls(self.value).get_var_slot_constraint(vrs, self.value)
        else:
            vrs[self.value] = ()
            return '''?%(cvar)s&:(or (eq ?%(cvar)s %(cls)s)
                                  subclassp ?%(cvar)s %(cls)s)''' % {
                                    'cvar': self.value,
                                    'cls': self.cls.__name__}

    def get_isc(self, templs, queries, vrs):
        """
        build clips snippet
        to be used when the class var is the subject in a fact
        or a mod in a predicate
        and the sentence is in a query.
        return (instance-set templates, instance-set queries)
        """
        if self.value in vrs and vrs[self.value]:
            queries.append('(eq ?%s %s)' % (self.value,
                                 utils.clips_instance(*(vrs[self.value]))))
        else:
            queries.append('''(or
                                 (eq %(val)s %(cls)s)
                                 (subclassp %(val)s %(cls)s))''' % {
                                           'val': self.value,
                                           'cls': self.cls.__name__})
        vrs[self.value] = ()
        return '?%s' % self.value

    def put(self, vrs):
        # a hack
        return self.cls(self.value).put(vrs)


class ClassVarVar(object):
    '''
    Intances of ClasVar can be called, and give back
    an instance of ClassVarVar,
    that can handle clips for
    the case in which we have both
    the class and the name or mods
    as variables.

    This is not to be part of the public api
    '''
    def __init__(self, clsvar, cls, var, **kwargs):
        self.value = var
        self.clsvar = clsvar
        self.cls = cls
        self.ob = cls(var, **kwargs)

    def get_constraint(self, vrs, ancestor, mod_path):
        '''
        return clips snippet to be used
        when the variable is a mod in a predicate
        and the sentence is in the tail of a rule.
        '''
        ci = utils.clips_instance(ancestor, mod_path)
        constraint = []
        if not self.value or self.value in vrs:
            return self.ob.get_constraint(vrs, ancestor, mod_path)
        else:
            vrs[self.value] = (ancestor, mod_path)
            vrs[self.clsvar] = (ancestor, mod_path, ['class'])
            return '''&:(or
                          (eq (class %(ci)s) %(cls)s)
                          (subclassp (class %(ci)s) %(cls)s))''' % {
                                    'ci': ci,
                                    'cls': self.cls.__name__}

    def get_slot_constraint(self, vrs):
        """
        return clips snippet to be used
        when the variable is predicate or subject
        and the sentence is in the tail of a rule.
        """
        if not self.value or self.value in vrs:
            return self.ob.get_var_slot_constraint(vrs, self.value)
        else:
            vrs[self.value] = ()
            vrs[self.clsvar] = (self.value, [], ['class'])
            return '''?%(var)s&:(or
                                 (eq (class ?%(var)s) %(cls)s)
                                 (subclassp (class ?%(var)s) %(cls)s))''' % {
                                    'var': self.value,
                                    'cls': self.cls.__name__}

    def put(self, vrs):
        return self.ob.put(vrs)

    def get_isc(self, templs, queries, vrs):
        """
        get instance-set condition;
        return (instance-set templates, instance-set queries)
        """
        if self.value in vrs and vrs[self.value]:
            templs.append((self.value, self.clsvar))
            queries.append('(eq ?%s %s)' % (self.value,
                                 utils.clips_instance(*(vrs[self.value]))))
        try:
            self.ob.get_mod_isc(self.value, templs, queries, vrs)
        except AttributeError: pass
        vrs[self.value] = ()
        return '?%s' % self.value


class Subword(object):
    '''
    Instantiated with two subclasses of Namable,
    Or of ClassVar created with Word(var),
    it can be used as a premise in a rule.
    '''
    def __init__(self, sub, sup):
        self.sub = sub
        self.sup = sup

    def get_ce(self, vrs):
        '''
        '''
        sub = getattr(self.sub, 'clsput', self.sub.put)(vrs)
        sup = getattr(self.sup, 'clsput', self.sup.put)(vrs)
        return ''' (test (or (eq %(sub)s %(sup)s)
                             (subclassp %(sub)s %(sup)s)))
               ''' % {'sub': sub,
                      'sup': sup}


class Noun(Word):
    """
    When Thing is extended, this adds 1 defclass to clips
    creating a subclass of Namable.
    And registers the class in utils.subclasses.

    If utils.varpat matches classname,
    it sets cls to Thing so that Word.__new__
    can return the right ClassVar,
    unless bases is a Thing subclass.
    """
    def __new__(cls, classname, bases=None, newdict=None):
        if utils.varpat.match(classname) and not bases:
            cls.cls = utils.get_class('Thing')
        return Word.__new__(cls, classname, bases, newdict)

    def __init__(cls, classname, bases, newdict):
        superclassname = bases[0].__name__
        clp = '(defclass %s (is-a %s))' % (classname, bases[0].__name__)
        super(Noun, cls).__init__(classname, bases, newdict, clp=clp)

utils.register('Noun', Noun)



class Verb(Word):
    """
    When Exists is extended, this adds 1 defclass to clips
    creating a subclass of Namable.
    And registers the class in utils.subclasses.

    The new class mods dictionary is also
    initialized with the given mods
    and the mods of all its bases.

    If utils.varpat matches classname,
    it sets cls to Exists so that Word.__new__
    can return the right ClassVar,
    unless bases is a Exists subclass.
    """
    def __new__(cls, classname, bases=None, newdict=None):
        if utils.varpat.match(classname) and not bases:
            cls.cls = utils.get_class('Exists')
        return Word.__new__(cls, classname, bases, newdict)

    def __init__(cls, classname, bases, newdict):
        if classname == 'Exists':
            # due to the ordering of things in clps.py
            utils.register(classname, cls)
            return
        slots = ['(slot %s (type %s) (visibility public) (pattern-match reactive))' % (mod,
            issubclass(utils.get_class(modclass), Number) and \
                                    '?VARIABLE' or 'INSTANCE')
                  for mod,modclass in cls.mods.items()]
        slots = ' '.join(slots)
        clp = '(defclass %s (is-a %s) %s)' % (classname,
                                              bases[0].__name__,
                                              slots)
        for mod,modclass in cls.mods.items():
            if isinstance(modclass, type):
                cls.mods[mod] = modclass.__name__
        for kls in bases:
            if getattr(kls, 'mods', _m):
                cls.mods.update(kls.mods)
        super(Verb, cls).__init__(classname, bases, newdict, clp=clp)

utils.register('Verb', Verb)



class Namable(object):
    """
    abstract class ancestor of all excluding
    the metawords.
    """
    __metaclass__ = Word

    @classmethod
    def from_clips(cls, instance):
        if not isinstance(instance, clips._clips_wrap.Instance):
            instance = clips.FindInstance(instance)
        clsname = str(instance.Class.Name)
        cls = utils.get_class(clsname)
        if clsname == 'Namable':
            return cls(str(instance))
        else:
            return cls.from_clips(instance)

    def get_var_constraint(self, vrs, ancestor, mod_path, ci):
        constraint = ''
        if self.value in vrs:
            if vrs[self.value]:
                v_ci = utils.clips_instance(*(vrs[self.value]))
                constraint = '&:(eq %s %s)' % (v_ci, ci)
            else:
                constraint = '&:(eq %s ?%s)' % (ci, self.value)
        elif not isinstance(self, Number):
            constraint = '&:(or (eq (class %(ci)s) %(cls)s) (subclassp (class %(ci)s) %(cls)s))' % {'ci': ci, 'cls': self.__class__.__name__}
        vrs[self.value] = (ancestor, mod_path)
        return constraint

    def get_var_slot_constraint(self, vrs, val):
        if self.value in vrs:
            if vrs[self.value]:
                return '?%(val)s&:(eq ?%(val)s %(var)s)' % {'val': val,
                                       'var': utils.clips_instance(*(vrs[self.value]))}
            else:
                return '?%s' % self.value
        else:
            vrs[self.value]= ()
            return class_constraint % {'val': self.value,
                                           'cls': self.__class__.__name__}

    def put_var(self, vrs):
        if self.value in vrs and vrs[self.value]:
            return utils.clips_instance(*(vrs[self.value]))
        return '?%s' % self.value


class ArithSafeTime(Namable):
    pass

class Number(Namable):
    """
    A number or an arithmetic operation.
    As value it can take a number, a str(number),
    a string containing a single arithmetic operator,
    within ['+', '-', '*', '/', '**']
    or a string with an arithmetic operation
    of the form '(<op> <arg1> <arg2>)'
    where op is one of the operators
    and arg can be a number or another operation.
    If the value is an operator,
    the args are either instances of Number,
    or strings to be converted in such instances.

    Within rules, variables can take the place of
    value, any of the args, or in the value in the position
    of numbers when it is a string representing an operation.
    """
    def __init__(self, value, arg1='', arg2=''):
        self.arg1 = str(arg1)
        self.arg2 = str(arg2)
        try:
            self.value = str(float(value))
        except ValueError:
            if value[0] == '(':
                args = utils.parens(value)
                self.value = args[0]
                self.arg1 = Number(args[1])
                self.arg2 = Number(args[2])
            else:
                self.value = value
                if self.arg1 != '':
                    self.arg1 = isinstance(arg1, Number) and arg1 or Number(arg1)
                if self.arg2 != '':
                    self.arg2 = isinstance(arg2, Number) and arg2 or Number(arg2)

    @classmethod
    def from_clips(cls, instance):
        return Number(instance)

    def _get_number(self, vrs):
        """
        """
        if utils.varpat.match(str(self.value)):
            return self.put_var(vrs)
        try:
            return str(float(self.value))
        except ValueError:
            arg1 = self.arg1 != '' and self.arg1._get_number(vrs) or ''
            arg2 = self.arg2 != '' and self.arg2._get_number(vrs) or ''
            return '(%s %s %s)' % (self.value, arg1, arg2)

    def get_slot_constraint(self, vrs):
        return self._get_number(vrs)

    def get_constraint(self, vrs, ancestor, mod_path):
        ci = utils.clips_instance(ancestor, mod_path)
        if utils.varpat.match(self.value):
            constraint = self.get_var_constraint(vrs, ancestor, mod_path, ci)
        else:
            constraint = '&:(eq %s %s)' % (ci, self.get_slot_constraint(vrs))
        return constraint

    def put(self, vrs):
        return self._get_number(vrs)

    def get_isc(self, templs, queries, vrs):
        """
        get instance-set condition;
        return (instance-set templates, instance-set queries)
        """
        return self._get_number(vrs)

    def __str__(self):
        return self._get_number({})



class Arith(Number):
    """
    Arithmetic predicate.
    It has the same basic form as number
    except that value can only be, either
    a string with an arithmetic predicate symbol
    from ['=', '<=', '>=', '!=', '<', '>'],
    or a string with a predication of the same form
    as the strings for number, but with the outermost
    operator substituted for a predicate.

    Instances can only be used as premises in rules.
    """
    def __init__(self, value, arg1='', arg2=''):
        if value[0] == '(':
            args = utils.parens(value)
            self.value = args[0]
            self.arg1 = Number(args[1])
            self.arg2 = Number(args[2])
        else:
            self.value = value
            if arg1 != '':
                self.arg1 = isinstance(arg1, Number) and arg1 or Number(arg1)
            if arg2 != '':
                self.arg2 = isinstance(arg2, Number) and arg2 or Number(arg2)

    def get_ce(self, vrs=None):
        arg1 = self.arg1.put(vrs)
        arg2 = self.arg2.put(vrs)
        return '(test (%s %s %s))' % (self.value, arg1, arg2)

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
    When Namable is extended, this adds 1 defclass to clips
    creating a subclass of Namable.
    And registers the class in subclasses
    """
    def __new__(cls, classname, bases, newdict=None):

        if utils.varpat.match(classname):
            return ClassVar(classname, bases)
        cls.value = ''
        return super(Word, cls).__new__(cls, classname, bases, newdict)

    def __init__(cls, classname, bases, newdict, clp=''):
        super(Word, cls).__init__(classname, bases, newdict)
        if clp:
            logger.info(clp)
            clips.Build(clp)
        utils.register(classname, cls)

    def clsput(self, vrs, ancestor=None, mod_path=None):
        return self.__name__

    def get_constraint_cls(self, vrs, ancestor=None, mod_path=None):
        ci = utils.clips_instance(ancestor, mod_path)
        return '&:(eq %s %s)' % (self.__name__, ci)

    def get_isc_cls(self, templs, queries, vrs):
        """
        get instance-set condition;
        return (instance-set templates, instance-set queries)
        """
        return self.__name__

    @classmethod
    def from_clips(cls, instance):
        if not isinstance(instance, clips._clips_wrap.Class):
            instance = clips.FindClass(instance)
        clsname = str(instance.Name)
        return utils.get_class(clsname)

utils.register('Word', Word)

class ClassVar(object):
    '''
    falta registrar noun y verb
    y dar los métodos __init__, get_slot_constraint,
    y un __call__ que construya un objeto de la clase apropiada.
    y claro, hace falta que las clases (es decir, noun y verb)
    tengan los métodos get_slot_constraint etc.
    '''
    def __init__(self, var, cls):
        self.value = var
        self.cls = cls

    def __call__(self, var):
        return ClassVarVar(self.value, self.cls, var)

    def get_constraint(self, vrs, ancestor, mod_path):
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
        build rule CE constraint for clips
        for a slot constraint for a pred in a prop in a rule
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
        get instance-set condition;
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
        return self.cls(self.value).put(vrs)


class ClassVarVar(object):
    '''
    '''
    def __init__(self, clsvar, cls, var):
        self.value = var
        self.clsvar = clsvar
        self.cls = cls

    def get_constraint(self, vrs, ancestor, mod_path):
        ci = utils.clips_instance(ancestor, mod_path)
        constraint = []
        if self.value in vrs:
            return self.cls(self.value).get_constraint(vrs, ancestor, mod_path)
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
        build rule CE constraint for clips
        for a slot constraint for a pred in a prop in a rule
        """
        if self.value in vrs:
            return self.cls(self.value).get_var_slot_constraint(vrs, self.value)
        else:
            vrs[self.value] = ()
            vrs[self.clsvar] = (self.value, [], ['class'])
            return '''?%(var)s&:(or
                                 (eq (class ?%(var)s) %(cls)s)
                                 (subclassp (class ?%(var)s) %(cls)s))''' % {
                                    'var': self.value,
                                    'cls': self.cls.__name__}

    def put(self, vrs):
        return self.cls(self.value).put(vrs)

    def get_isc(self, templs, queries, vrs):
        """
        get instance-set condition;
        return (instance-set templates, instance-set queries)
        """
        if self.value in vrs and vrs[self.value]:
            templs.append((self.value, self.clsvar))
            queries.append('(eq ?%s %s)' % (self.value,
                                 utils.clips_instance(*(vrs[self.value]))))
        vrs[self.value] = ()
        return '?%s' % self.value


class Noun(Word):
    """
    When Namable is extended, this adds 1 defclass to clips
    creating a subclass of Namable.
    And registers the class in subclasses
    """
    def __init__(cls, classname, bases, newdict):
        superclassname = bases[0].__name__
        clp = '(defclass %s (is-a %s))' % (classname, bases[0].__name__)
        super(Noun, cls).__init__(classname, bases, newdict, clp=clp)

utils.register('Noun', Noun)



class Verb(Word):
    """
    When State is extended, this registers the class in _subclasses
    """
    def __init__(cls, classname, bases, newdict):
        if classname == 'State':
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


class Number(Namable):
    """

    """
    def __init__(self, value, arg1='', arg2=''):
        self.arg1 = arg1
        self.arg2 = arg2
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
        if utils.varpat.match(self.value):
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
    Arithmetic predicate
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

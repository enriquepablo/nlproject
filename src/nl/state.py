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

#import uuid
import clips
from log import logger
from nl.clps import class_constraint
from nl.utils import register, subclasses, Namable, varpat, clips_instance, get_class, _newvar
from nl.arith import Number
from nl.thing import Thing


# marker object
_m = []


class Verb(type):
    """
    When State is extended, this registers the class in _subclasses
    """
    def __init__(cls, classname, bases, newdict):
        super(Verb, cls).__init__(classname, bases, newdict)
        if classname == 'State':
            return
        slots = ['(slot %s (type %s) (visibility public) (pattern-match reactive))' % (mod,
            issubclass(get_class(modclass), Number) and \
                                    '?VARIABLE' or 'INSTANCE')
                  for mod,modclass in cls.mods.items()]
        slots = ' '.join(slots)
        clp = '(defclass %s (is-a %s) %s)' % (classname,
                                              bases[0].__name__,
                                              slots)
        logger.info(clp)
        clips.Build(clp)
        cls._v_clips_class = clips.FindClass(classname)
        for mod,modclass in cls.mods.items():
            if isinstance(modclass, type):
                cls.mods[mod] = modclass.__name__
        for kls in bases:
            if getattr(kls, 'mods', _m):
                cls.mods.update(kls.mods)
        register(classname, cls)


class State(Namable):
    """
    """
    __metaclass__ = Verb

    subject = Thing
    mods = {}

    def __init__(self, *args, **kwargs):
        self.value = args and args[0] or ''
        for mod,cls in self.mods.items():
            if kwargs.get(mod, _m) is not _m:
                if isinstance(kwargs[mod], subclasses[cls]):
                    setattr(self, mod, kwargs[mod])
                else:
                    setattr(self, mod, subclasses[cls](kwargs[mod]))
            #else:
            #    raise NlError("wrong modifier for verb")

    def __str__(self):
        if self.value:
            return self.value
        mods = []
        for mod,cls in self.mods.items():
            if getattr(self, mod, _m) is not _m:
              mods.append('%s %s' % (mod, str(getattr(self, mod))))
        return '%s %s' % (self.__class__.__name__.lower(),
                          ' '.join(mods))

    @classmethod
    def from_clips(cls, instance):
        if not isinstance(instance, clips._clips_wrap.Instance):
            instance = clips.FindInstance(instance)
        cls = subclasses[str(instance.Class.Name)]
        kwargs = {}
        for mod,mcls in cls.mods.items():
            cmod = instance.GetSlot(mod)
            if cmod is not None:
                kwargs[mod] = subclasses[mcls].from_clips(cmod)
        return cls(**kwargs)

    def get_slot_constraint(self, vrs):
        """
        build rule CE constraint for clips
        for a slot constraint for a prop in a rule
        """
        newvar = _newvar()
        if varpat.match(self.value):
            return self.get_var_slot_constraint(vrs, newvar)
        constraint = [class_constraint % {'val': newvar,
                                         'cls': self.__class__.__name__}]
        for mod,cls in self.mods.items():
            mod_o =  getattr(self, mod, _m)
            if mod_o is not _m:
                constraint.append(mod_o.get_constraint(vrs, newvar, (mod,)))
        return ''.join(constraint)

    def get_constraint(self, vrs, ancestor, mod_path):
        ci = clips_instance(ancestor, mod_path)
        constraint = []
        if self.value:
            if self.value in vrs:
                if vrs[self.value]:
                    v_ci = clips_instance(*(vrs[self.value]))
                    constraint.append('&:(eq %s %s)' % (v_ci, ci))
                else:
                    constraint.append('&:(eq %s ?%s)' % (ci, self.value))
            else:
                vrs[self.value] = (ancestor, mod_path)
        else:
            constraint.append('&:(or (eq (class %(val)s) %(cls)s) (subclassp (class %(val)s) %(cls)s))' % {'val': ci, 'cls': self.__class__.__name__})
            for mod,cls in self.mods.items():
                mod_o =  getattr(self, mod, _m)
                if mod_o is not _m:
                    constraint.append(mod_o.get_constraint(vrs,
                                                       ancestor,
                                                       mod_path + (mod,)))
        return ''.join(constraint)

    def put(self, vrs, name=None):
        """
        put pred in clips as a make-instance action.
        """
        if self.value and varpat.match(self.value):
            return self.put_var(vrs)
        slots = []
        for mod in self.mods:
            mod_o = getattr(self, mod, _m)
            if mod_o is not _m:
                slots += [mod, mod_o.put(vrs)]
        slots = ' '.join(slots)
        return '(add-pred %s %s)' % (self.__class__.__name__, slots)


    def get_isc(self, templs, queries, vrs):
        """
        get instance-set condition;
        return (instance-set templates, instance-set queries)
        """
        newvar = _newvar()
        if self.value:
            if self.value in vrs:
                if vrs[self.value]:
                    queries.append('(eq ?%s %s)' % (newvar,
                                     clips_instance(*(vrs[self.value]))))
                else:
                    newvar = self.value
            else:
                vrs[self.value] = ()
                newvar = self.value
        templs.append((newvar, self.__class__.__name__))
        for mod,mcls in self.mods.items():
            mod_o = getattr(self, mod, _m)
            if mod_o is not _m and not (varpat.match(mod_o.value) and mod_o.value not in vrs):
                queries.append('(eq ?%s:%s %s)' % (newvar, mod,
                                               mod_o.get_isc(templs, queries, vrs)))
        return '?%s' % newvar


register('State', State)

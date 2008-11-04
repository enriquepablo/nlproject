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
# from nl.exceptions import NlError
from log import logger
from nl.registry import register, subclasses, clips
from nl.arith import Number
from nl.thing import varpat, class_constraint


# marker object
_m = []

_vn = 0

def _newvar():
    global _vn
    _vn += 1
    return 'Y%d' % _vn

class Verb(object):
    """
    """
    clp = '(defclass Verb (is-a USER))'
    logger.info(clp)
    clips.Build(clp)
    clips_class = clips.FindClass('Verb')


class MetaState(type):
    """
    When State is extended, this registers the class in _subclasses
    """
    def __init__(cls, classname, bases, newdict):
        super(MetaState, cls).__init__(classname, bases, newdict)
        #slots = ['(is-a %s)' % bases[0].__name__]
        slots = ['(slot %s (type %s))' % (mod,
            issubclass(modclass, Number) and '?VARIABLE' or 'INSTANCE')
                  for mod,modclass in cls.mods.items()]
        slots = ' '.join(slots)
        clp = '(defclass %s (is-a %s) %s)' % (classname,
                                              bases[0].__name__,
                                              slots)
        logger.info(clp)
        clips.Build(clp)
        cls.clips_class = clips.FindClass(classname)
        for kls in bases:
            if getattr(kls, 'mods', _m):
                cls.mods.update(kls.mods)
        register(classname, cls)


class State(Verb):
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
            #else:
            #    raise NlError("wrong modifier for verb")

    def __str__(self):
        return ''

    @classmethod
    def from_clips(cls, instance):
        inst = clips.FindInstance(instance)
        cls = subclasses[str(inst.Class.Name)]
        kwargs = {}
        for mod,mcls in cls.mods.items():
            cmod = inst.GetSlot(mod)
            if cmod is not None:
                kwargs[mod] = mcls.from_clips(cmod)
        return cls(**kwargs)

    def get_slot_constraint(self, vrs):
        """
        build rule CE constraint for clips
        for a slot constraint for a prop in a rule
        """
        newvar = _newvar()
        constraint = class_constraint % {'val': newvar,
                                         'cls': self.__class__.__name__}
        for mod,cls in self.mods.items():
            mod_o =  getattr(self, mod, _m)
            if mod_o is not _m:
                var = getattr(self, mod).value
                if varpat.match(var):
                    if var in vrs:
                        if vrs[var]:
                            constraint += '&:(eq (send ?%s get-%s) (send ?%s get-%s))' % (newvar, mod, vrs[var][0], vrs[var][1])
                        else:
                            constraint += '&:(eq (send ?%s get-%s) ?%s)' % (newvar,
                                                                   mod, var)
                    else:
                        vrs[var] = (newvar, mod)
                else:
                    if isinstance(mod_o, Number):
                        constraint += '&:(eq (send ?%s get-%s) %s)' % (newvar,
                                       mod, mod_o.get_slot_constraint(vrs))
                    else:
                        constraint += '&:(eq (send ?%s get-%s) [%s])' % (newvar,
                                                                   mod, var)
        return constraint

    def put(self, vrs):
        """
        put pred in clips as a make-instance action.
        """
        slots = []
        for mod in self.mods:
            mod_o = getattr(self, mod, _m)
            if mod_o is not _m:
                slots.append('(%s %s)' % (mod, mod_o.put(vrs)))
        slots = ' '.join(slots)
        return '(make-instance of %s %s)' % (self.__class__.__name__, slots)


    def get_isc(self, templs, queries):
        """
        get instance-set condition;
        return (instance-set templates, instance-set queries)
        """
        newvar = _newvar()
        templs.append('(?%s %s)' % (newvar, self.__class__.__name__))
        for mod,mcls in self.mods.items():
            mod_o = getattr(self, mod, _m)
            if mod_o is not _m and not varpat.match(mod_o.value):
                queries.append('(eq ?%s:%s %s)' % (newvar, mod,
                                               mod_o.get_isc(templs, queries)))
        return '?' + newvar


register('State', State)


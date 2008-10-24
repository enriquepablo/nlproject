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
from nl.registry import register, subclasses, clips
from nl.arith import Number
from nl.thing import varpat


# marker object
_m = []
# symbol for empty args
empty = 'null_'

class _Verb(object):
    """
    """
    clips_class = clips.USER_CLASS.BuildSubclass('Verb')


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
        cls.clips_class = bases[0].clips_class.BuildSubclass(classname, slots)
        for kls in bases:
            if getattr(kls, 'mods', _m):
                cls.mods.update(kls.mods)
        register(classname, cls)


class State(_Verb):
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

    _vn = 0

    @classmethod
    def _newvar(cls):
        cls._vn += 1
        return 'Y%d' % cls._vn

    @classmethod
    def from_clips(cls, instance):
        inst = clips.FindInstance(instance)
        cls = subclasses[str(inst.Class.Name)]
        kwargs = {}
        for mod,mcls in cls.mods.items():
                kwargs[mod] = mcls.from_clips(inst.GetSlot(mod))
        return cls(**kwargs)

    def get_slot_constraint(self, ces):
        """
        build rule CE constraint for clips
        for a slot constraint for a prop in a rule
        """
        newvar = self._newvar()
        ce = ['(is-a %s)' % self.__class__.__name__,
              '(name ?%s)' % newvar]
        for mod,cls in self.mods.items():
            mod_o =  getattr(self, mod, _m)
            if mod_o is not _m:
                var = getattr(self, mod).value
                if varpat.match(var):
                    ce.append('(%s ?%s)' % (mod, var))
                else:
                    var = mod_o.get_slot_constraint(ces)
                    ce.append('(%s %s)' % (mod, var))
        ces.append('(logical (object %s))' % ' '.join(ce))
        return newvar

    def put(self):
        """
        put pred in clips as a make-instance action.
        """
        slots = []
        for mod in self.mods:
            mod_o = getattr(self, mod, _m)
            if mod_o is not _m:
                slots.append('(%s %s)' % (mod, mod_o.put()))
        slots = ' '.join(slots)
        return '(make-instance of %s %s)' % (self.__class__.__name__, slots)


    def get_isc(self, templs, queries):
        """
        get instance-set condition;
        return (instance-set templates, instance-set queries)
        """
        newvar = self._newvar()
        templs.append('(?%s %s)' % (newvar, self.__class__.__name__))
        for mod,mcls in self.mods.items():
            mod_o = getattr(self, mod, _m)
            if mod_o is not _m:
                queries.append('(eq ?%s:%s %s)' % (newvar, mod,
                                               mod_o.get_isc(templs, queries)))
        return '?' + newvar


register('State', State)


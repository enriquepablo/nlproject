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
from nl.exceptions import NlError
from nl.registry import register, subclasses


# marker object
_m = []


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
        cls = subclasses[clsname]
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


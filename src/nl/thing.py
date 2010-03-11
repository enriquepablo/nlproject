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
from nl.metanl import Noun, Namable

class Thing(Namable):
    """
    The most general noun, the ancestor of all nouns.
    Is instantiated with a string,
    that can be a variable if within a rule,
    or any other string that is a valid python name,
    that will act as a proper name.

    The instances can be used as first argument to Fact,
    and also as named arguments in the construction of
    Exists instances.

    The class, and its subclasses, can be used as first argument
    to Fact, and as named arguments in Exists instances.
    """
    __metaclass__ = Noun

    def __init__(self, value, _clsvar=''):
        if value.startswith('['):
            value = value[1:-1]
        self.value = value
        self.clsvar = _clsvar

    def __str__(self):
        return self.value

    def __repr__(self):
        return '%s is a %s' % (self.value, self.__class__.__name__)

    @classmethod
    def from_clips(cls, instance):
        if isinstance(instance, clips._clips_wrap.InstanceName):
            return cls(str(instance))
        if not isinstance(instance, clips._clips_wrap.Instance):
            instance = clips.FindInstance(instance)
        clsname = str(instance.Class.Name)
        cls = utils.get_class(clsname)
        return cls(str(instance))

    def get_ce(self, vrs):
        """
        build CE for clips
        """
        if utils.varpat.match(self.value):
            vrs[self.value] = ()
            ce = '(logical (object (is-a %s) (name ?%s)))'
            return ce % (self.__class__.__name__, self.value)
        return ''

    def get_slot_constraint(self, vrs):
        """
        build rule CE constraint for clips
        for a slot constraint for a pred in a prop in a rule
        """
        if utils.varpat.match(self.value):
            return self.get_var_slot_constraint(vrs, self.value)
        return '[%s]' % self.value

    def get_constraint(self, vrs, ancestor, mod_path):
        ci = utils.clips_instance(ancestor, mod_path)
        if utils.varpat.match(self.value):
            constraint = self.get_var_constraint(vrs, ancestor, mod_path, ci)
        else:
            constraint = '&:(eq %s [%s])' % (ci, self.value)
        return constraint

    def get_isc(self, templs, queries, vrs):
        """
        get instance-set condition;
        return (instance-set templates, instance-set queries)
        """
        if utils.varpat.match(self.value):
            if self.value in vrs:
                if vrs[self.value]:
                    queries.append('(eq ?%s %s)' % (self.value,
                                     utils.clips_instance(*(vrs[self.value]))))
            else:
                templs.append((self.value, self.__class__.__name__))
                vrs[self.value] = ()
            return '?%s' % self.value
        else:
            return '[%s]' % self.value

    def get_ism(self,  templs, queries, vrs, newvar='sen'):
        """
        get instance-set method;
        return (instance-set templates, instance-set queries)
        """
        if utils.varpat.match(self.value):
            templs.append((self.value, self.__class__.__name__))
            vrs[self.value] = ()
        else:
            templs.append((newvar, self.__class__.__name__))
            queries.append('(eq ?%s [%s])' % (newvar, self.value))

    def put(self, vrs):
        if utils.varpat.match(self.value):
            return self.put_var(vrs)
        else:
            return '[%s]' % self.value

    def put_action(self, vrs=None):
        """
        put name in clips as a make-instance action.
        """
        if vrs is None:
            vrs = {}
        val = self.put(vrs)
        return '(reduce-class %s %s)' % (val, self.__class__.__name__)

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

import re
from persistent import Persistent

from log import logger
from nl.registry import register, subclasses, clips

# vars are always XNUM
varpat = re.compile(r'^X\d+$')

class_constraint = '?%(val)s&:(eq (class ?%(val)s) %(cls)s)|:(subclassp (class ?%(val)s) %(cls)s)'
sec_var_constraint = '?%(val)s&:(eq ?%(val)s (send ?%(var)s get-%(mod)s))'

clp = '(defclass Name (is-a USER))'
logger.info(clp)
clips.Build(clp)

class Name(Persistent):
    """
    """
    clips_class = clips.FindClass('Name')

class MetaThing(type):
    """
    When Name is extended, this adds 1 defclass to clips
    creating a subclass of Name.
    And registers the class in subclasses
    """
    def __init__(cls, classname, bases, newdict):
        super(MetaThing, cls).__init__(classname, bases, newdict)
        clp = '(defclass %s (is-a %s))' % (classname, bases[0].__name__)
        logger.info(clp)
        clips.Build(clp)
        cls.clips_class = clips.FindClass(classname)
        register(classname, cls)

# XXX ponerle adjetivos a thing?
class Thing(Name):
    """
    """
    __metaclass__ = MetaThing

    def __init__(self, value):
        self.value = value

    @classmethod
    def from_clips(cls, instance):
        inst = clips.FindInstance(instance)
        cls = subclasses[str(inst.Class.Name)]
        return cls(str(inst))

    def __str__(self):
        return '%s is a %s' % (self.value, self.__class__.__name__)

    def get_ce(self, vrs):
        """
        build CE for clips
        """
        if varpat.match(self.value):
            vrs[self.value] = ()
            ce = '(logical (object (is-a %s) (name ?%s)))'
            return ce % (self.__class__.__name__, self.value)
        return ''

    def get_slot_constraint(self, vrs):
        """
        build rule CE constraint for clips
        for a slot constraint for a pred in a prop in a rule
        """
        if varpat.match(self.value):
            if self.value in vrs:
                if vrs[self.value]:
                    return sec_var_constraint % {'val': self.value,
                                                 'var': vrs[self.value][0],
                                                 'mod': vrs[self.value][1]}
                else:
                    return '?%s' % self.value
            else:
                vrs[self.value]= ()
                return class_constraint % {'val': self.value,
                                           'cls': self.__class__.__name__}
        return '[%s]' % self.value

    def put_action(self, vrs):
        """
        put name in clips as a make-instance action.
        """
        val = self.put(vrs)
        return '(make-instance %s of %s)' % (val, self.__class__.__name__)

    def put(self, vrs):
        if varpat.match(self.value):
            if self.value in vrs and vrs[self.value]:
                return '(send ?%s get-%s)' % (vrs[self.value][0],
                                              vrs[self.value][1])
            else:
                return '?%s' % self.value
        else:
            return '[%s]' % self.value

    def get_isc(self, templs, queries):
        """
        get instance-set condition;
        return (instance-set templates, instance-set queries)
        """
        if varpat.match(self.value):
            templs.append('(?%s %s)' % (self.value,
                                        self.__class__.__name__))
            return '?%s' % self.value
        else:
            return '[%s]' % self.value

    def get_ism(self,  templs, queries, newvar='sen'):
        """
        get instance-set method;
        return (instance-set templates, instance-set queries)
        """
        if varpat.match(self.value):
            templs.append('(?%s %s)' % (self.value,
                                        self.__class__.__name__))
        else:
            templs.append('(?%s %s)' % (newvar, self.__class__.__name__))
            queries.append('(eq ?%s [%s])' % (newvar, self.value))

register('Thing', Thing)


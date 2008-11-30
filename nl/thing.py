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

from nl.log import logger
from nl.utils import register, clips, varpat, class_constraint, Name, clips_instance

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
        cls._v_clips_class = clips.FindClass(classname)
        register(classname, cls)

# XXX ponerle adjetivos a thing?
class Thing(Name):
    """
    """
    __metaclass__ = MetaThing

    def __init__(self, value):
        if value.startswith('['):
            value = value[1:-1]
        self.value = value

    def __str__(self):
        return self.value

    def __repr__(self):
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
                    return '?%(val)s&:(eq ?%(val)s %(var)s)' % {'val': self.value,
                                           'var': clips_instance(*(vrs[self.value]))}
                else:
                    return '?%s' % self.value
            else:
                vrs[self.value]= ()
                return class_constraint % {'val': self.value,
                                           'cls': self.__class__.__name__}
        return '[%s]' % self.value

    def get_constraint(self, vrs, ancestor, mod_path):
        ci = clips_instance(ancestor, mod_path)
        constraint = ''
        if varpat.match(self.value):
            if self.value in vrs:
                if vrs[self.value]:
                    v_ci = clips_instance(*(vrs[self.value]))
                    constraint = '&:(eq %s %s)' % (v_ci, ci)
                else:
                    constraint = '&:(eq %s ?%s)' % (ci, self.value)
            else:
                vrs[self.value] = (ancestor, mod_path)
        else:
            constraint = '&:(eq %s [%s])' % (ci, self.value)
        return constraint

    def put_action(self, vrs):
        """
        put name in clips as a make-instance action.
        """
        val = self.put(vrs)
        return '(reduce-class %s %s)' % (val, self.__class__.__name__)

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


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
import re
import clips

from nl.registry import register, subclasses

# vars are always XNUM
varpat = re.compile(r'^X(\d+)$')

class _Name(object):
    """
    """
    clips_class = clips.USER_CLASS.BuildSubclass('Name')

    def __init__(self, value):
        self.value = value


class MetaThing(type):
    """
    When Name is extended, this adds 1 defclass to clips
    creating a subclass of Name.
    And registers the class in subclasses
    """
    def __init__(cls, classname, bases, newdict):
        super(MetaThing, cls).__init__(classname, bases, newdict)
        cls.clips_class = bases[0].clips_class.BuildSubclass(classname)
        register(classname, cls)

# XXX ponerle adjetivos a thing
class Thing(_Name):
    """
    """
    __metaclass__ = MetaThing

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
            vrs.append(self.value)
            return '(logical (object (is-a %s)(name ?%s)))' % (self.__class__.__name__,
                                     self.value)
        return '(logical (object (is-a %s)(name %s)))' % (self.__class__.__name__,
                                    self.value)

    def get_slot_constraint(self, vrs):
        """
        build rule CE constraint for clips
        for a slot constraint for a prop in a rule
        """
        if varpat.match(self.value):
            if self.value in vrs:
                return '?%s' % self.value
            else:
                vrs.append(self.value)
                return '?%(val)s&:(superclassp %(cls)s (class ?%(val)s))|:(eq %(cls)s (class ?%(val)s))' % {'val': self.value, 'cls': self.__class__.__name__}
        else:
            return '[%s]' % self.value

    def put_action(self):
        """
        put name in clips as a make-instance action.
        """
        val = varpat.match(self.value) and '?%s' % self.value or '[%s]' % self.value
# XXX chequear que no existe y no pertenece ya a una clase más específica. Entonces se podrán contruir props con nombres nuevos, y unificar los put
        return '(make-instance %s of %s)' % (val, self.__class__.__name__)

    def put(self):
        return self.get_slot_constraint([self.value])

    def get_isc(self, templs, queries):
        """
        get instance-set condition;
        return (instance-set templates, instance-set queries)
        """
        if varpat.match(self.value):
            templs.append('(?%s %s)' % (self.value, self.__class__.__name__))
            return '?%s' % self.value
        else:
            return '[%s]' % self.value

    def get_ism(self,  templs, queries, newvar='sen'):
        """
        get instance-set method;
        return (instance-set templates, instance-set queries)
        """
        if varpat.match(self.value):
            templs.append('(?%s %s)' % (self.value, self.__class__.__name__))
        else:
            templs.append('(?%s %s)' % (newvar, self.__class__.__name__))
            queries.append('(eq ?%s [%s])' % (newvar, self.value))

register('Thing', Thing)


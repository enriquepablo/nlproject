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
# utils module

import re
from persistent import Persistent

import clips
from nl.log import logger

clips.DebugConfig.ExternalTraceback = True

# vars are always XNUM
varpat = re.compile(r'^[A-Z]\w*\d+$')

class_constraint = '?%(val)s&:(or (eq (class ?%(val)s) %(cls)s) (subclassp (class ?%(val)s) %(cls)s))'
_name_def = '(defclass Name (is-a USER))'
_reduce_class = '''
(deffunction reduce-class (?instance ?class)
    (if (eq (length$
                (find-all-instances ((?a ?class))(eq (instance-name ?a) ?instance)))
             0)
    then (make-instance ?instance of ?class)
    else (return TRUE)))'''

clips.Build(_name_def)
clips.Build(_reduce_class)
#clips.Build(_del_daemon)
logger.info(_name_def)
logger.info(_reduce_class)
#logger.info(_del_daemon)


subclasses = {}
def register(clsname, cls):
    subclasses[clsname] = cls


class Name(Persistent):
    """
    """
    _v_clips_class = clips.FindClass('Name')

    @classmethod
    def from_clips(cls, instance):
        if not isinstance(instance, clips._clips_wrap.Instance):
            instance = clips.FindInstance(instance)
        cls = subclasses[str(instance.Class.Name)]
        return cls(str(instance))

    def get_var_constraint(self, vrs, ancestor, mod_path, ci):
        constraint = ''
        if self.value in vrs:
            if vrs[self.value]:
                v_ci = clips_instance(*(vrs[self.value]))
                constraint = '&:(eq %s %s)' % (v_ci, ci)
            else:
                constraint = '&:(eq %s ?%s)' % (ci, self.value)
        else:
            constraint = '&:(or (eq (class %(val)s) %(cls)s) (subclassp (class %(val)s) %(cls)s))' % {'val': ci, 'cls': self.__class__.__name__}
            vrs[self.value] = (ancestor, mod_path)
        return constraint

    def get_var_slot_constraint(self, vrs, val):
        if self.value in vrs:
            if vrs[self.value]:
                return '?%(val)s&:(eq ?%(val)s %(var)s)' % {'val': val,
                                       'var': clips_instance(*(vrs[self.value]))}
            else:
                return '?%s' % self.value
        else:
            vrs[self.value]= ()
            return class_constraint % {'val': self.value,
                                           'cls': self.__class__.__name__}

    def put_var(self, vrs):
        if self.value in vrs and vrs[self.value]:
            return clips_instance(*(vrs[self.value]))
        return '?%s' % self.value

register('Name', Name)


def clips_instance(ancestor, mod_path):
    send_str = '(send ?%s get-%s)'
    for mod in mod_path:
        ancestor = send_str % (ancestor, mod)
        send_str = '(send %s get-%s)'
    return ancestor

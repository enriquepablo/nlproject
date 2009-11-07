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

import clips
from nl.log import logger
from nl.clps import class_constraint

# vars are always XNUM
varpat = re.compile(r'^[A-Z]\w*\d+$')

_vn = 0

def _newvar():
    global _vn
    _vn += 1
    return 'Y%d' % _vn


subclasses = {}
def register(clsname, cls):
    subclasses[clsname] = cls

def get_class(cls):
    return isinstance(cls, str) and subclasses[cls] or cls


class Name(object):
    """
    """
    _v_clips_class = clips.FindClass('Name')

    @classmethod
    def from_clips(cls, instance):
        if not isinstance(instance, clips._clips_wrap.Instance):
            instance = clips.FindInstance(instance)
        clsname = str(instance.Class.Name)
        cls = subclasses[clsname]
        if clsname == 'Name':
            return cls(str(instance))
        else:
            return cls.from_clips(instance)

    def get_var_constraint(self, vrs, ancestor, mod_path, ci):
        from nl.arith import Number
        constraint = ''
        if self.value in vrs:
            if vrs[self.value]:
                v_ci = clips_instance(*(vrs[self.value]))
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


# XXX not thread safe
_now = '1'

def change_now(i=0):
    global _now
    _now = i and str(float(i)) or \
            str(float(_now) + 1)




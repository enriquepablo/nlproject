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

# vars are always XNUM
varpat = re.compile(r'^X\d+$')

class_constraint = '?%(val)s&:(eq (class ?%(val)s) %(cls)s)|:(subclassp (class ?%(val)s) %(cls)s)'
sec_var_constraint = '?%(val)s&:(eq ?%(val)s (send ?%(var)s get-%(mod)s))'
_name_def = '(defclass Name (is-a USER))'
a = '(defclass Thing (is-a Name))'
b = '(defclass Cuerpo (is-a Thing))'
c = '(make-instance [a] of Thing)'
_reduce_class = '(deffunction reduce-class (?instance ?class) (if (or (eq (length$ (find-instance ((?a ?class)) (eq (instance-name ?a) ?instance))) 0) (subclassp ?class (class ?instance))) then (make-instance ?instance of ?class)))'
_init_daemon = '(defmessage-handler Name init after () (python-call tonl (class ?self) ?self))'
_del_daemon = '(defmessage-handler Name delete before () (python-call rmnl ?self))'

clips.Build(_name_def)
clips.Build(_reduce_class)
clips.Build(_init_daemon)
clips.Build(_del_daemon)
logger.info(_name_def)
logger.info(_reduce_class)
logger.info(_init_daemon)
logger.info(_del_daemon)


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

register('Name', Name)

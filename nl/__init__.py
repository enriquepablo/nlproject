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

import os
import md5
import transaction

import kb
from arith import Number, Arith, Time
from thing import Thing
from state import State
from prop import Proposition, Prop
from rule import Rule
from log import logger
from registry import clips, subclasses, root
#from exceptions import Paradox, NlError

_reduce_class = '(deffunction reduce-class (?instance ?class) (if (subclassp ?class (class ?instance)) then (make-instance ?instance of ?class)))'
clips.Build(_reduce_class)
logger.info(_reduce_class)

def tonl(classname, name):
    cls = subclasses[classname]
    sen = cls.from_clips(name)
    m = md5.new()
    m.update(str(sen))
    md5sum = m.digest()
    r = root()
    if md5sum in r['sentences']:
        return False
    else:
        r['sentences'][md5sum] = sen
        transaction.commit()
        return True

clips.RegisterPythonFunction(tonl)

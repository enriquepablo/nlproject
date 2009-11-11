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
from nl.arith import Number

# marker object
_m = []

class Noun(type):
    """
    When Namable is extended, this adds 1 defclass to clips
    creating a subclass of Namable.
    And registers the class in subclasses
    """
    def __init__(cls, classname, bases, newdict):
        super(Noun, cls).__init__(classname, bases, newdict)
        clp = '(defclass %s (is-a %s))' % (classname, bases[0].__name__)
        logger.info(clp)
        clips.Build(clp)
        cls._v_clips_class = clips.FindClass(classname)
        utils.register(classname, cls)


class Verb(type):
    """
    When State is extended, this registers the class in _subclasses
    """
    def __init__(cls, classname, bases, newdict):
        super(Verb, cls).__init__(classname, bases, newdict)
        if classname == 'State':
            return
        slots = ['(slot %s (type %s) (visibility public) (pattern-match reactive))' % (mod,
            issubclass(utils.get_class(modclass), Number) and \
                                    '?VARIABLE' or 'INSTANCE')
                  for mod,modclass in cls.mods.items()]
        slots = ' '.join(slots)
        clp = '(defclass %s (is-a %s) %s)' % (classname,
                                              bases[0].__name__,
                                              slots)
        logger.info(clp)
        clips.Build(clp)
        cls._v_clips_class = clips.FindClass(classname)
        for mod,modclass in cls.mods.items():
            if isinstance(modclass, type):
                cls.mods[mod] = modclass.__name__
        for kls in bases:
            if getattr(kls, 'mods', _m):
                cls.mods.update(kls.mods)
        utils.register(classname, cls)


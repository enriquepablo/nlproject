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

#import uuid
from persistent.dict import PersistentDict

from log import logger
from nl.utils import register, subclasses, clips, Name, varpat, class_constraint, sec_var_constraint
from nl.arith import Number
from nl.thing import Thing


# marker object
_m = []

_vn = 0

def _newvar():
    global _vn
    _vn += 1
    return 'Y%d' % _vn


clp = '(defclass Verb (is-a USER))'
logger.info(clp)
clips.Build(clp)

class Verb(Name):
    """
    """
    _v_clips_class = clips.FindClass('Verb')


class MetaState(type):
    """
    When State is extended, this registers the class in _subclasses
    """
    def __init__(cls, classname, bases, newdict):
        super(MetaState, cls).__init__(classname, bases, newdict)
        #slots = ['(is-a %s)' % bases[0].__name__]
        slots = ['(slot %s (type %s) (visibility public))' % (mod,
            issubclass(modclass, Number) and '?VARIABLE' or 'INSTANCE')
                  for mod,modclass in cls.mods.items()]
        slots = ' '.join(slots)
        clp = '(defclass %s (is-a %s) %s)' % (classname,
                                              bases[0].__name__,
                                              slots)
        clp_r = '(defrule (prop(s, p)) => s is-a S)' # XXX follow this? too many rules?
        logger.info(clp)
        clips.Build(clp)
        cls._v_clips_class = clips.FindClass(classname)
        for mod,modclass in cls.mods.items():
            if isinstance(modclass, type):
                cls.mods[mod] = modclass.__name__
        for kls in bases:
            if getattr(kls, 'mods', _m):
                cls.mods.update(kls.mods)
        register(classname, cls)


class State(Verb):
    """
    """
    __metaclass__ = MetaState

    value = ''
    subject = Thing
    mods = PersistentDict()

    def __init__(self, *args, **kwargs):
        if args:
            self.value = args[0]
        for mod,cls in self.mods.items():
            if kwargs.get(mod, _m) is not _m:
                if isinstance(kwargs[mod], subclasses[cls]):
                    setattr(self, mod, kwargs[mod])
                else:
                    setattr(self, mod, subclasses[cls](kwargs[mod]))
            #else:
            #    raise NlError("wrong modifier for verb")

    def __str__(self):
        if self.value:
            return self.value
        mods = []
        for mod,cls in self.mods.items():
            if getattr(self, mod, _m) is not _m:
              mods.append('%s %s' % (mod, str(getattr(self, mod))))
        return '%s %s' % (self.__class__.__name__.lower(),
                          ' '.join(mods))

    @classmethod
    def from_clips(cls, instance):
        if not isinstance(instance, clips._clips_wrap.Instance):
            instance = clips.FindInstance(instance)
        cls = subclasses[str(instance.Class.Name)]
        kwargs = {}
        for mod,mcls in cls.mods.items():
            cmod = instance.GetSlot(mod)
            if cmod is not None:
                kwargs[mod] = subclasses[mcls].from_clips(cmod)
        return cls(**kwargs)

    def get_slot_constraint(self, vrs, mod=''):
        """
        build rule CE constraint for clips
        for a slot constraint for a prop in a rule
        """
        newvar = _newvar()
        if self.value in vrs:
            return sec_var_constraint % {'val': newvar,
                                         'var': vrs[self.value][0],
                                         'mod': vrs[self.value][1]}
        elif self.value:
            return '?%s' % self.value
        newvar = _newvar()
        constraint = class_constraint % {'val': newvar,
                                         'cls': self.__class__.__name__}
        for mod,cls in self.mods.items():
            mod_o =  getattr(self, mod, _m)
            if mod_o is not _m:
                var = str(getattr(self, mod))
                if varpat.match(var):
                    if var in vrs:
                        if vrs[var]:
                            constraint += '&:(eq (send ?%s get-%s) (send ?%s get-%s))' % (newvar, mod, vrs[var][0], vrs[var][1])
                        else:
                            constraint += '&:(eq (send ?%s get-%s) ?%s)' % (newvar,
                                                                   mod, var)
                    else:
                        vrs[var] = (newvar, mod)
                else:
                    if isinstance(mod_o, Number):
                        constraint += '&:(eq (send ?%s get-%s) %s)' % (newvar,
                                       mod, mod_o.get_slot_constraint(vrs))
                    else:
                        constraint += '&:(eq (send ?%s get-%s) [%s])' % (newvar,
                                                                   mod, var)
        return constraint

    def put(self, vrs, name=None):
        """
        put pred in clips as a make-instance action.
        """
        slots = []
        for mod in self.mods:
            mod_o = getattr(self, mod, _m)
            if mod_o is not _m:
                slots += [mod, mod_o.put(vrs)]
        slots = ' '.join(slots)
        return '(add-pred %s %s)' % (self.__class__.__name__, slots)


    def get_isc(self, templs, queries):
        """
        get instance-set condition;
        return (instance-set templates, instance-set queries)
        """
        newvar = _newvar()
        templs.append('(?%s %s)' % (newvar, self.__class__.__name__))
        for mod,mcls in self.mods.items():
            mod_o = getattr(self, mod, _m)
            if mod_o is not _m and not varpat.match(mod_o.value):
                queries.append('(eq ?%s:%s %s)' % (newvar, mod,
                                               mod_o.get_isc(templs, queries)))
        return '?' + newvar


register('State', State)

#_init_daemon = '(defmessage-handler State init after () (python-call pred_tonl ?self))'
_set_tal = '(set-sequence-operator-recognition TRUE)'

_set_slots = """(defmessage-handler State set-slots primary ($?slots)
        (while (> (length$ ?slots) 0) do
            (bind ?slot (first$ ?slots))
            (bind ?vslots (rest$ ?slots))
            (bind ?value (first$ ?vslots))
            (bind ?slots (rest$ ?vslots))
            (dynamic-put $?slot $?value))
        (return (instance-name ?self)))
"""

_add_pred ="""
(deffunction add-pred (?class $?slots)
        (bind ?key (str-cat ?class $?slots))
        (bind ?pos (str-index "." ?key))
        (while ?pos do 
            (bind ?key (str-cat (sub-string 1 (- ?pos 1) ?key)
                                "_"
                                (sub-string (+ ?pos 1) (str-length ?key) ?key)))
            (bind ?pos (str-index "." ?key)))
        (bind ?key (sym-cat ?key))
        (if (instance-existp ?key) then
            (return (instance-name ?key))
         else
            (make-instance ?key of ?class)
            (send (instance-name ?key) set-slots $?slots)))
"""


clips.Eval(_set_tal)
clips.Build(_set_slots)
clips.Build(_add_pred)
logger.info(_set_tal)
logger.info(_set_slots)
logger.info(_add_pred)


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
import clips
from log import logger
from nl.clps import class_constraint
from nl import utils
from nl.metanl import Word, Verb, Namable
from nl.metanl import ClassVar, ClassVarVar
from nl.thing import Thing


# marker object
_m = []


class Exists(Namable):
    """
    The most general verb, the ancestor of all verbs.
    Can be instantiated, either with a string with a variable,
    or with a series of named arguments, corresponding
    to the items in the mods dict with which the class
    was declared.

    The instances can be used as the second argument to Fact,
    and also as named arguments in the construction of other
    Exists instances.

    The class, and its subclasses, can be used as first argument
    to Fact, and as named arguments in Exists instances.
    """
    __metaclass__ = Verb

    subject = Thing
    instantaneous = True
    mods = {}

    def __init__(self, *args, **kwargs):
        #if args and kwargs:
        #    raise ValueError('You cannot instantiate a verb (%s) with both '
        #                     'unnamed and named arguments' %
        #                           self.__class__.__name__)
        if len(args) > 1:
            raise ValueError('You cannot instantiate a verb (%s) with more '
                             'than one unnamed argument' %
                                   self.__class__.__name__)
        if args and args[0] and not utils.varpat.match(args[0]):
            raise ValueError('You cannot instantiate a verb (%s) with an '
                             'unnamed argument that does not correspond '
                             'to a variable' %
                                   self.__class__.__name__)
        for kw in kwargs:
            if kw not in self.mods and kw != '_clsvar':
                raise KeyError('Unknown kwarg for %s: %s' %
                               (self.__class__.__name__, kw))
        self.value = args and args[0] or ''
        self.clsvar = kwargs.get('_clsvar', '')
        for mod,cls in self.mods.items():
            if kwargs.get(mod, _m) is not _m:
                if isinstance(kwargs[mod], basestring) or \
                   isinstance(kwargs[mod], int) or \
                   isinstance(kwargs[mod], float):
                    setattr(self, mod, utils.get_class(cls)(kwargs[mod]))
                else:
                    if not isinstance(kwargs[mod], utils.get_class(cls)) and \
                       not (isinstance(utils.get_class(cls), Word) and
                           (isinstance(kwargs[mod], ClassVarVar))) and \
                       not (issubclass(utils.get_class(cls), Word) and
                            isinstance(kwargs[mod], ClassVar)):
                        raise ValueError('The %s arg to %s must be of '
                                         'type %s' % (mod,
                                            self.__class__.__name__, cls))
                    setattr(self, mod, kwargs[mod])

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
        cls = utils.get_class(str(instance.Class.Name))
        kwargs = {}
        for mod,mcls in cls.mods.items():
            cmod = instance.GetSlot(mod)
            if cmod and str(cmod) != 'nil':
                mcls = utils.get_class(mcls)
                kwargs[mod] = mcls.from_clips(cmod)
        return cls(**kwargs)

    def get_slot_constraint(self, vrs):
        """
        build rule CE constraint for clips
        for a slot constraint for a prop in a rule
        """
        if utils.varpat.match(self.value):
            if self.clsvar and self.clsvar not in vrs:
                if vrs[self.value]:
                    vrs[self.clsvar] = (
                               utils.clips_instance(*(vrs[self.value])),
                               [], ['class'])
                else:
                    vrs[self.clsvar] = (self.value, [], ['class'])
            return self.get_var_slot_constraint(vrs, self.value)
        newvar = utils._newvar()
        if self.clsvar:
            if self.clsvar in vrs:
                if vrs[self.clsvar]:
                    constraint = [
                     '?%(newvar)s&:(eq (class ?%(newvar)s) %(var)s)' % {
                         'newvar': newvar,
                         'var': utils.clips_instance(*(vrs[self.clsvar]))}]
                else:
                    constraint = [
                     '?%(newvar)s&:(eq (class ?%(newvar)s) %(var)s)' % {
                         'newvar': newvar,
                         'var': self.clsvar}]
            else:
                vrs[self.clsvar] = (newvar, [], ('class',))
                constraint = [class_constraint % {'val': newvar,
                                         'cls': self.__class__.__name__}]
        else:
            constraint = [class_constraint % {'val': newvar,
                                         'cls': self.__class__.__name__}]
        for mod,cls in self.mods.items():
            mod_o =  getattr(self, mod, _m)
            if mod_o is not _m:
                constraint_meth = getattr(mod_o, 'get_constraint_cls',
                                          mod_o.get_constraint)
                constraint.append(constraint_meth(vrs, newvar, (mod,)))
        return ''.join(constraint)

    def get_constraint(self, vrs, ancestor, mod_path):
        constraint = self.get_query(vrs, ancestor, mod_path)
        if not constraint:
            return ''
        return '&:%s' % '&:'.join(constraint)

    def get_query(self, vrs, ancestor, mod_path):
        ci = utils.clips_instance(ancestor, mod_path)
        constraint = []
        if self.value:
            if self.value in vrs:
                if vrs[self.value]:
                    if self.clsvar and self.clsvar not in vrs:
                        vrs[self.clsvar] = (
                               utils.clips_instance(*(vrs[self.value])),
                               [], ['class'])
                    v_ci = utils.clips_instance(*(vrs[self.value]))
                    constraint.append('(eq %s %s)' % (v_ci, ci))
                else:
                    if self.clsvar and self.clsvar not in vrs:
                        vrs[self.clsvar] = (ancestor, mod_path, ['class'])
                    constraint.append('(eq %s ?%s)' % (ci, self.value))
            else:
                vrs[self.value] = (ancestor, mod_path)
        else:
            if self.clsvar:
                if self.clsvar in vrs:
                    if vrs[self.clsvar]:
                        constraint.append(
                         '(eq (class %s) %s)' % (ci,
                             utils.clips_instance(*(vrs[self.clsvar]))))
                    else:
                        constraint.append(
                         '(eq (class %s) %s)' % (ci, self.clsvar))
                else:
                    vrs[self.clsvar] = (ancestor, mod_path, ('class',))
                    constraint.append(
                        '''(or (eq (class %(val)s) %(cls)s)
                                 (subclassp (class %(val)s) %(cls)s))''' % {
                                           'val': ci,
                                           'cls': self.__class__.__name__})
            else:
                constraint.append(
                        '''(or (eq (class %(val)s) %(cls)s)
                                 (subclassp (class %(val)s) %(cls)s))''' % {
                                           'val': ci,
                                           'cls': self.__class__.__name__})
            for mod,cls in self.mods.items():
                mod_o =  getattr(self, mod, _m)
                if mod_o is not _m:
                    constraint_meth = getattr(mod_o, 'get_query_cls',
                                              mod_o.get_query)
                    constraint += constraint_meth(vrs,
                                                   ancestor,
                                                 mod_path + (mod,))
        return constraint

    def put(self, vrs, name=None):
        """
        put pred in clips as a make-instance action.
        """
        if self.value and utils.varpat.match(self.value):
            return self.put_var(vrs)
        slots = []
        for mod in self.mods:
            mod_o = getattr(self, mod, _m)
            if mod_o is not _m:
                put_meth = getattr(mod_o, 'clsput', mod_o.put)
                slots += [mod, put_meth(vrs)]
        slots = ' '.join(slots)
        if self.clsvar:
            if vrs[self.clsvar]:
                return '(add-pred %s %s)' % (
                    utils.clips_instance(*(vrs[self.clsvar])), slots)
            else:
                return '(add-pred %s %s)' % (self.clsvar, slots)
        return '(add-pred %s %s)' % (self.__class__.__name__, slots)


    def get_isc(self, queries, vrs, ancestor, mod_path):
        """
        get instance-set condition;
        return (instance-set templates, instance-set queries)
        """
        constraint = self.get_query(vrs, ancestor, mod_path)
        if len(constraint) == 1:
            queries += constraint
        elif constraint:
            queries.append('(and %s)' % ' '.join(constraint))

    def in_fact(self, fact):
        '''
        To be overriden by subclasses.
        hook called from clips when a fact that has a predicate
        of this class is added to clips.

        TODO: implement as an after message-handler for creation of
        instances of Exists, so that not all facts make the call.
        '''
        logger.info('FROM STATE! %s' % str(fact))

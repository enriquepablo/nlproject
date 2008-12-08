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
from nl.utils import register, varpat, Name, clips_instance

# marker object
_m = []


def parens(expr):
    """
    quick and dirty parens parser
    """
    if expr[0] != '(':
        return expr
    depth = 0
    term = ''
    terms = []
    for c in expr:
        if depth == 1 and c == ' ':
            terms.append(term)
            term = ''
        elif c == '(':
            depth += 1
            if depth > 1:
                term += c
        elif c == ')':
            depth -= 1
            if depth > 0:
                term += c
        else:
            term += c
    terms.append(term)
    return terms

class Number(Name):
    """

    """
    def __init__(self, value, arg1='', arg2=''):
        self.arg1 = arg1
        self.arg2 = arg2
        try:
            self.value = str(float(value))
        except ValueError:
            if value[0] == '(':
                args = parens(value)
                self.value = args[0]
                self.arg1 = Number(args[1])
                self.arg2 = Number(args[2])
            else:
                self.value = value
                if arg1 != '':
                    self.arg1 = isinstance(arg1,
                                           Number) and arg1 or Number(arg1)
                if arg2 != '':
                    self.arg2 = isinstance(arg2,
                                           Number) and arg2 or Number(arg2)

    @classmethod
    def from_clips(cls, instance):
        return Number(instance)

    def get_slot_constraint(self, vrs):
        """
        in a make-instance of a proposition
        """
        if varpat.match(self.value):
            if self.value in vrs and vrs[self.value]:
                return clips_instance(*(vrs[self.value]))
            else:
                return '?%s' % self.value
        try:
            return str(float(self.value))
        except ValueError:
            if self.value == 'now':
                return 'now'
            arg1 = self.arg1 != '' and self.arg1.get_slot_constraint(vrs)
            arg2 =  self.arg1 != '' and self.arg2.get_slot_constraint(vrs)
            return '(%s %s %s)' % (self.value, arg1, arg2)

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
            constraint = '&:(eq %s %s)' % (ci, self.get_slot_constraint(vrs))
        return constraint

    def put(self, vrs):
        return self.get_slot_constraint(vrs)

    def get_isc(self, templs, queries, vrs):
        """
        get instance-set condition;
        return (instance-set templates, instance-set queries)
        """
        return self.put(vrs)

    def __str__(self):
        return self.put({})

register('Number', Number)


class Arith(Number):
    """
    Arithmetic predicate
    """
    def __init__(self, value, arg1='', arg2=''):
        if value[0] == '(':
            args = parens(value)
            self.value = args[0]
            self.arg1 = Number(args[1])
            self.arg2 = Number(args[2])
        else:
            self.value = value
            if arg1 != '':
                self.arg1 = isinstance(arg1, Number) and arg1 or Number(arg1)
            if arg2 != '':
                self.arg2 = isinstance(arg2, Number) and arg2 or Number(arg2)

    def get_ce(self, vrs=None):
        arg1 = self.arg1.put(vrs)
        arg2 = self.arg2.put(vrs)
        return '(test (%s %s %s))' % (self.value, arg1, arg2)

    def get_isc(self, templs, queries):
        arg1 =  self.arg1.put({})
        arg2 =  self.arg2.put({})
        queries.append('(%s %s %s)' % (self.value, arg1, arg2))

    def __str__(self):
        queries = []
        self.get_isc([], queries)
        return queries.pop()

register('Arith', Arith)


class Time(Number):
    """
    """

    @classmethod
    def from_clips(cls, instance):
        return Time(instance)


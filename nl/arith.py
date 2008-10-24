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
from nl.registry import register
from nl.thing import varpat

# marker object
_m = []


def parens(expr):
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

class Number(object):
    """
    A number can be of 4 classes: number, operation, var, & op with vars.
    A number can be in three places: in a bare prop, in a prem, in a con.
    in a prop it can be a number (and possibly an op).
    in a prem it can be in 2 places: in a prop, or in an arith condition.
    in a prem prop, it is a single number.
    in an arith prem, it is anything at all.
    in a cond, it is anything at all.
    so we need:
    put gives us prop and con; with a vrs arg that tell where to get the vals.
    get_constraint gives a var or a number, and if var and in vrs, a simple
    comparison with the referenced mod of the vrs.
    and we need an ArithPred that gives us CEs. with vrs.
    (double check the newvar passing).
    """
    def __init__(self, value, arg1='', arg2=''):
        try:
            self.value = str(int(value))
        except ValueError:
            if value[0] == '(':
                args = parens(value)
                self.value = args[0]
                self.arg1 = Number(args[1])
                self.arg2 = Number(args[2])
            else:
                self.value = value
                self.arg1 = arg1
                self.arg2 = arg2

    @classmethod
    def from_clips(cls, instance):
        return Number(instance)

    def get_slot_constraint(self, ces):
        """
        in a make-instance of a proposition
        """
        if varpat.match(self.value):
            return '?%s' % self.value
        try:
            return str(int(self.value))
        except ValueError:
            if self.value == 'now':
                return 'now'
            arg1 = self.arg1.get_slot_constraint(ces)
            arg2 =  self.arg2.get_slot_constraint(ces)
            return '(%s %s %s)' % (self.value, arg1, arg2)

    def put(self):
        return self.get_slot_constraint([])

    def get_isc(self, templs, queries):
        """
        get instance-set condition;
        return (instance-set templates, instance-set queries)
        """
        return self.put()

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
            self.arg1 = arg1
            self.arg2 = arg2

    def get_ce(self, ces=None):
        arg1 = self.arg1.put()
        arg2 = self.arg2.put()
        return '(test (%s %s %s))' % (self.value, arg1, arg2)

    def get_isc(self, templs, queries):
        arg1 =  self.arg1.put()
        arg2 =  self.arg2.put()
        queries.append('(%s %s %s)' % (self.value, arg1, arg2))

register('Arith', Arith)


class Time(Number):
    """
    """

    @classmethod
    def from_clips(cls, instance):
        return Time(instance)


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

from nl.utils import register
from nl.arith import Number
from nl.utils import Name


class Equals(object):
    """
    Equals predicate
    """
    def __init__(self, truth, arg1, arg2):
        self.truth = truth
        self.arg1 = isinstance(arg1, Name) and arg1 or Number(arg1)
        self.arg2 = isinstance(arg2, Name) and arg2 or Number(arg2)

    def get_ce(self, vrs=None):
        arg1 = self.arg1.put(vrs)
        arg2 = self.arg2.put(vrs)
        truth = self.truth and 'eq' or 'neq'
        return '(test (%s %s %s))' % (truth, arg1, arg2)

    def get_isc(self, templs, queries):
        arg1 =  self.arg1.put({})
        arg2 =  self.arg2.put({})
        truth = self.truth and 'eq' or 'neq'
        queries.append('(%s %s %s)' % (truth, arg1, arg2))

    def __str__(self):
        queries = []
        self.get_isc([], queries)
        return queries.pop()

register('Equals', Equals)


class Remove(object):
    '''
    remove prop from db
    '''
    def __init__(self, sen):
        self.sen = sen

    def __str__(self):
        return 'remove ' + str(self.sen)

    def put_action(self, vrs=None):
        return self.sen.remove_action(vrs)

register('Remove', Remove)

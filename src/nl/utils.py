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

from nl.log import logger

# vars are always XNUM
varpat = re.compile(r'^[A-Z]\w*\d+$')

# XXX not thread safe
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


def clips_instance(ancestor, mod_path):
    send_str = '(send ?%s get-%s)'
    for mod in mod_path:
        ancestor = send_str % (ancestor, mod)
        send_str = '(send %s get-%s)'
    return ancestor


_now = '1'

def change_now(i=0):
    global _now
    _now = i and str(float(i)) or \
            str(float(_now) + 1)

def parens(expr):
    """
    >>> from nl.arith import parens
    >>> parens('uno')
    'uno'
    >>> parens('(uno (dos tres) cuatro)')
    ['uno', '(dos tres)', 'cuatro']
    >>> parens('(uno (dos tres) (ho ho (he (ha ha))) cuatro)')
    ['uno', '(dos tres)', '(ho ho (he (ha ha)))', 'cuatro']
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


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

import time
import re
import os
import sys
import pkg_resources  # setuptools specific

from nl.log import logger

# vars are always XNUM
varpat = re.compile(r'^[A-Z]\w*\d+$')

plugins = []


def load_plugins():
    '''setuptools based plugin loader'''
    entrypoint = 'nl.new_fact'
    for entrypoint in pkg_resources.iter_entry_points(entrypoint):
        plugin = entrypoint.load()
        plugins.append(plugin)

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
    return isinstance(cls, basestring) and subclasses[cls] or cls


def clips_instance(ancestor, mod_path, meths=None):
    send_str = '(send ?%s get-%s)'
    meth_str = '(%s ?%s)'
    for mod in mod_path:
        if ancestor.startswith('('):
            send_str = '(send %s get-%s)'
        ancestor = send_str % (ancestor, mod)
        send_str = '(send %s get-%s)'
        meth_str = '(%s %s)'
    if meths:
        for meth in meths:
            ancestor = meth_str % (meth, ancestor)
            meth_str = '(%s %s)'
    return possible_var(ancestor)

def possible_var(var):
    if varpat.match(var):
        return '?%s' % var
    return var

_now = 0.0
_time_granularity = 1.0
_time_start_delta = 0

def change_now(i=0):
    'deprecated'
    global _now
    delta = float(int(time.time())) - _now
    logger.debug('OOOOOOOOOOOOOO %f' %  delta)
    _now = i and float(i) or \
            float(_now) + 1000.0

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
    if depth != 0:
        raise ValueError('wrong arithmetic expression')
    return terms

def get_subclasses(cls):
    return [subclass[0] for subclass in subclasses.items() \
            if issubclass(subclass[1], cls)]

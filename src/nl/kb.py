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
import itertools
#import re

from nl.exceptions import Paradox
from nl.log import here, logger
from nl.utils import clips, subclasses, Name, varpat
from nl.thing import Thing
from nl.state import State
from nl.time import Time
from nl.prop import Fact
from nl.rule import Rule


def tell(*args):
    for sentence in args:
        s = sentence.put_action({})
        if isinstance(sentence, Rule):
            logger.info(s)
            clips.Build(s)
        else:
            logger.info(s)
            clips.Eval(s)

def get_instancesn(*sentences):
    templs = []
    queries = []
    vrs = {}
    for sentence in sentences:
        sentence.get_ism(templs, queries, vrs)
    if len(queries) > 1:
        q = '(find-all-instances (%s) (and %s))' % \
            (' '.join(['(?%s %s)' % templ for templ in templs]),
                               ' '.join(queries))
    else:
        q = '(find-all-instances (%s) %s)' % \
                (' '.join(['(?%s %s)' % templ for templ in templs]),
                               queries and queries[0] or 'TRUE')
    return q, templs

def get_instances(*sentences):
    q, templs = get_instancesn(*sentences)
    logger.info('query: %s\ntempls: %s' % (q, str(templs)))
    return clips.Eval(q), templs

def retract(sentence):
    for ins in get_instances(sentence):
        clips.FindInstance(ins).Remove()

def ask(*sentences):
    clps, templs = get_instances(*sentences)
    resp = []
    if clps:
        names = [Name.from_clips(ins) for ins in clps]
        while names:
            first = names[:len(templs)]
            names = names[len(templs):]
            rsp = {}
            for templ in templs:
                if varpat.match(templ[0]) and not templ[0].startswith('Y'):
                    rsp[templ[0]] = str(first[templs.index(templ)])
            if rsp:
                resp.append(rsp)
        if not resp:
            resp = 'yes'
    else:
        resp = 'no'
    logger.info('RESP ' + str(resp))
    return resp

def ask_objs(*sentences):
    clps = get_instances(*sentences)
    sens = []
    if clps:
        for ins in clps:
            if isinstance(sentence, Thing):
                sens.append(Thing.from_clips(ins))
            elif isinstance(sentence, Fact):
                i = clips.FindInstance(ins)
                if issubclass(subclasses[str(i.Class.Name)], Fact):
                    sens.append(Fact.from_clips(ins))
    return sens


def ask_old(*sentences):
    sens = ask_objs(*sentences)
    if not sens:
        resp = 'no'
    else:
        resp = str(sens[0])
    logger.info(str(len(sens))+'\n\n\n'+resp)
    return resp

def extend():
    logger.info('----------running---------------------')
    acts = clips.Run()
    logger.info('----------runned: %d---------------------' % acts)
    return acts

# _pred_pat = re.compile('[()\s\[\]]')
# def make_pred(classname, slots):
#     key = classname + re.sub(_pred_pat, '', slots)
#     clp_pred = clips.FindInstance(key)
#     if not clp_pred:
#         clp = 'make-instance [%s] of %s %s' % (key, classname, slots)
#         clp_pred = clips.Eval(clp)
#     return clp_pred
# 
# clips.RegisterPythonFunction(make_pred)


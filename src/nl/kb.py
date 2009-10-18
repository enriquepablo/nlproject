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
#import re

from nl.exceptions import Paradox
from nl.log import here, logger
from nl.utils import clips, subclasses
from nl.thing import Thing
from nl.state import State
from nl.time import Time
from nl.prop import Proposition
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

def get_instancesn(sentence):
    templs = []
    queries = []
    sentence.get_ism(templs, queries, {})
    if len(queries) > 1:
        q = '(find-all-instances (%s) (and %s))' % (' '.join(templs),
                                                    ' '.join(queries))
    else:
        q = '(find-all-instances (%s) %s)' % (' '.join(templs),
                                            queries and queries[0] or 'TRUE')
    return q

def get_instances(sentence):
    q = get_instancesn(sentence)
    logger.info(q)
    return clips.Eval(q)

def retract(sentence):
    for ins in get_instances(sentence):
        clips.FindInstance(ins).Remove()


def ask_objs(sentence):
    clps = get_instances(sentence)
    sens = []
    if clps:
        for ins in clps:
            if isinstance(sentence, Thing):
                sens.append(Thing.from_clips(ins))
            elif isinstance(sentence, Proposition):
                i = clips.FindInstance(ins)
                if issubclass(subclasses[str(i.Class.Name)], Proposition):
                    sens.append(Proposition.from_clips(ins))
    return sens


def ask(sentence):
    sens = ask_objs(sentence)
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


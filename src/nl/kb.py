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
from nl.metanl import Namable
from nl.thing import Thing
from nl.prop import Fact
from nl.rule import Rule


def tell(*args):
    for sentence in args:
        s = sentence.put_action()
        logger.info(s)
        if isinstance(sentence, Rule):
            clips.Build(s)
        else:
            clips.Eval(s)

def get_instancesn(*sentences):
    templs = []
    queries = []
    vrs = {}
    for n, sentence in enumerate(sentences):
        sentence.get_ism(templs, queries, vrs, newvar='q%d' % n)
    templs = list(set(templs))
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
    logger.info(q)
    return clips.Eval(q), templs

def retract(sentence):
    for ins in get_instances(sentence):
        clips.FindInstance(ins).Remove()

def ask(*sentences):
    clps, templs = get_instances(*sentences)
    resp = []
    if clps:
        names = [Namable.from_clips(ins) for ins in clps]
        while names:
            first = names[:len(templs)]
            names = names[len(templs):]
            rsp = {}
            for templ in templs:
                if utils.varpat.match(templ[0]) and not templ[0].startswith('Y'):
                    rsp[templ[0]] = str(first[templs.index(templ)])
            if rsp:
                resp.append(rsp)
        if not resp:
            resp = True
    else:
        resp = False
    return resp

def ask_obj(sentence):
    clps, templs = get_instances(sentence)
    sens = []
    if clps:
        for ins in clps:
            if isinstance(sentence, Thing):
                sens.append(Namable.from_clips(ins))
            elif isinstance(sentence, Fact):
                i = clips.FindInstance(ins)
                if issubclass(utils.get_class(str(i.Class.Name)), Fact):
                    sens.append(Fact.from_clips(ins))
    return sens

def extend():
    acts = clips.Run()
    return acts


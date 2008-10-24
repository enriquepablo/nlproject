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
from nl.registry import clips
from nl.thing import Thing
from nl.prop import Proposition
from nl.rule import Rule


def tell(sentence):
    s = sentence.put_action()
    f = open('clips', 'a')
    f.write(s+'\n')
    f.close()
    if isinstance(sentence, Rule):
        try:
            clips.Build(s)
        except:
            import pdb;pdb.set_trace()
    else:
        clips.Eval(s)

def get_instancesn(sentence):
    templs = []
    queries = []
    sentence.get_ism(templs, queries)
    if len(queries) > 1:
        q = '(find-all-instances (%s) (and %s))' % (' '.join(templs), ' '.join(queries))
    else:
        q = '(find-all-instances (%s) %s)' % (' '.join(templs), queries and queries[0] or 'TRUE')
    return q

def get_instances(sentence):
    templs = []
    queries = []
    sentence.get_ism(templs, queries)
    if len(queries) > 1:
        q = '(find-all-instances (%s) (and %s))' % (' '.join(templs), ' '.join(queries))
    else:
        q = '(find-all-instances (%s) %s)' % (' '.join(templs), queries and queries[0] or 'TRUE')
    return clips.Eval(q)

def retract(sentence):
    for ins in get_instances(sentence):
        clips.FindInstance(ins).Remove()


def ask(sentence):
    clps = get_instances(sentence)
    if clps:
        sens = []
        for ins in clps:
            if isinstance(sentence, Thing):
                sens.append(str(Thing.from_clips(ins)))
            else:
                sens.append(str(Proposition.from_clips(ins)))
        return "\n".join(sens)
    else:
        return 'no'


def ask_objs(sentence):
    clps = get_instances(sentence)
    if clps:
        sens = []
        for ins in clps:
            if isinstance(sentence, Thing):
                sens.append(Thing.from_clips(ins))
            else:
                sens.append(Proposition.from_clips(ins))
        return sens
    else:
        return 'no'

def extend():
    return clips.Run()

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
import transaction
from ZODB.FileStorage import FileStorage
from ZODB.DB import DB
from BTrees.OOBTree import OOBTree


from nl.log import here, logger
from nl.utils import clips, subclasses
from nl.thing import Thing
from nl.state import State
from nl.arith import Time
from nl.prop import Proposition
from nl.rule import Rule


app = None
_initializing = False
_extending = False

def open(name='data'):
    global app, _initializing
    fs = os.path.join(here, 'var/%s.fs' % name)
    base = FileStorage(fs)
    db = DB(base)
    app = db.open()
    root = app.root()
    if not root.has_key('props'):
        root['rules'] = OOBTree()
        root['props'] = OOBTree()
        root['things'] = OOBTree()
        transaction.commit()
    else:
        _initializing = True
        for t in ('things', 'rules', 'props'):
            for sen in app.root()[t].itervalues():
                tell(sen)
        while True:
            n = clips._clips.getNextActivation()
            logger.info(n)
            if n is None:
                break
            clips._clips.deleteActivation(n)
        _initializing = False

def close():
    global app
    app.close()
    app.db().close()
    app = None
    logger.info('---------------------------------')
    return 'DB closed'


def tell(*args):
    for sentence in args:
        s = sentence.put_action({})
        if isinstance(sentence, Rule):
            if not app.root()['rules'].has_key(sentence.name):
                logger.info(s)
                app.root()['rules'][sentence.name] = sentence
                clips.Build(s)
                transaction.commit()
            elif _initializing:
                logger.info(s)
                clips.Build(s)
        elif isinstance(sentence, Proposition) and \
               _initializing or \
               not app.root()['props'].has_key(str(sentence)):
            logger.info(s)
            clips.Eval(s)
        elif isinstance(sentence, Thing) and \
               _initializing or \
               not app.root()['things'].has_key(str(sentence)):
            logger.info(s)
            clips.Eval(s)

def get_instancesn(sentence):
    templs = []
    queries = []
    sentence.get_ism(templs, queries)
    if len(queries) > 1:
        q = '(find-all-instances (%s) (and %s))' % (' '.join(templs),
                                                    ' '.join(queries))
    else:
        q = '(find-all-instances (%s) %s)' % (' '.join(templs),
                                            queries and queries[0] or 'TRUE')
    return q

def get_instances(sentence):
    templs = []
    queries = []
    sentence.get_ism(templs, queries)
    if len(queries) > 1:
        q = '(find-all-instances (%s) (and %s))' % (' '.join(templs),
                                                    ' '.join(queries))
    else:
        q = '(find-all-instances (%s) %s)' % (' '.join(templs),
                                            queries and queries[0] or 'TRUE')
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
        return 'no'
    else:
        return '\n'.join(map(str, sens))

def extend():
    global _extending
    _extending = True
    acts = clips.Run()
    _extending =False
    transaction.commit()
    return acts


def rmnl(classname, name):
    return True

clips.RegisterPythonFunction(rmnl)

def tonl(classname, name):
    cls = subclasses[str(classname)]
    sen = cls.from_clips(name)
    key = str(sen)
    logger.info(key)
    if not app.root()['things'].has_key(key): # XXX index
        app.root()['things'][key] = sen
        if not _extending:
            transaction.commit()
    return True

clips.RegisterPythonFunction(tonl)

def ptonl(subj, pred, time, truth):
    s = Thing.from_clips(subj)
    p = State.from_clips(pred)
    t = Time.from_clips(time)
    sen = Proposition(s, p, t, truth=truth)
    key = str(sen)
    if not app.root()['props'].has_key(key): # XXX index
        app.root()['props'][key] = sen
        if not _extending:
            transaction.commit()
    elif not _initializing:
        return clips.Symbol('FALSE')
    logger.info(key)
    return True

clips.RegisterPythonFunction(ptonl)

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

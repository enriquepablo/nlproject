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
import transaction
from ZODB.FileStorage import FileStorage
from ZODB.DB import DB
from BTrees.OOBTree import OOBTree


from nl.log import here, logger
from nl.utils import clips, subclasses
from nl.thing import Thing
from nl.prop import Proposition
from nl.rule import Rule


app = None

def open(name='data'):
    global app
    fs = os.path.join(here, 'var/%s.fs' % name)
    base = FileStorage(fs)
    db = DB(base)
    app = db.open()
    root = app.root()
    if not root.has_key('sentences'):
        root['sentences'] = OOBTree()
        transaction.commit()


def close():
    global app
    app.close()
    app = None
    return 'DB closed'


def tell(*args):
    for sentence in args:
        s = sentence.put_action({})
        logger.info(s)
        if isinstance(sentence, Rule):
            clips.Build(s)
        else:
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
    return clips.Run()


def rmnl(classname, name):
    pass

clips.RegisterPythonFunction(rmnl)

def tonl(classname, name):
    global mivar
    cls = subclasses[str(classname)]
    sen = cls.from_clips(name)
    key = str(sen)
    if app.root()['sentences'].has_key(key): # XXX index
        return False
    else:
        app.root()['sentences'][key] = sen
        transaction.commit()
        return True

clips.RegisterPythonFunction(tonl)

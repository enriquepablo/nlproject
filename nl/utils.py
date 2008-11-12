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
import os
import md5
import transaction
from persistent import Persistent
from ZODB.FileStorage import FileStorage
from ZODB.DB import DB
from BTrees.OOBTree import OOBTree

import clips
from nl.log import here, logger

# vars are always XNUM
varpat = re.compile(r'^X\d+$')

class_constraint = '?%(val)s&:(eq (class ?%(val)s) %(cls)s)|:(subclassp (class ?%(val)s) %(cls)s)'
sec_var_constraint = '?%(val)s&:(eq ?%(val)s (send ?%(var)s get-%(mod)s))'
_name_def = '(defclass Name (is-a USER))'
_reduce_class = '(deffunction reduce-class (?instance ?class) (if (or (eq (length$ (find-instance ((?a ?class)) (eq (instance-name ?a) ?instance))) 0) (subclassp ?class (class ?instance))) then (make-instance ?instance of ?class)))'
_init_daemon = '(defmessage-handler Name init after () (python-call tonl (class ?self) ?self))'
_del_daemon = '(defmessage-handler Name delete before () (python-call rmnl ?self))'

clips.Build(_name_def)
clips.Build(_reduce_class)
#clips.Build(_init_daemon)
#clips.Build(_del_daemon)
logger.info(_name_def)
logger.info(_reduce_class)
logger.info(_init_daemon)
logger.info(_del_daemon)



def rmnl(classname, name):
    pass

clips.RegisterPythonFunction(rmnl)

def tonl(classname, name):
    cls = subclasses[classname]
    sen = cls.from_clips(name)
    m = md5.new()
    m.update(str(sen))
    md5sum = m.digest()
    r = root()
    if md5sum in r['sentences']:
        return False
    else:
        r['sentences'][md5sum] = sen
        transaction.commit()
        return True

clips.RegisterPythonFunction(tonl)


class Name(Persistent):
    """
    """
    clips_class = clips.FindClass('Name')

subclasses = {}
def register(clsname, cls):
    subclasses[clsname] = cls

fs = os.path.join(here, 'var/data.fs')
if not os.path.exists(fs):
    base = FileStorage(fs)
    db = DB(base)
    conn = db.open()
    conn.root()['sentences'] = OOBTree()
    transaction.commit()
    db.close()


def root():
    base = FileStorage(fs)
    db = DB(base)
    conn = db.open()
    try:
        while True:
            yield conn.root()
    finally:
        db.close()

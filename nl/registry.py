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
# registry of subclasses

import os
from ZODB.FileStorage import FileStorage
from ZODB.DB import DB
from BTrees.OOBTree import OOBTree
from persistent.mapping import PersistentMapping
import transaction

import clips
from nl.log import here

fs = os.path.exists(os.path.join(here, 'var/data.fs'))
if not fs:
    base = FileStorage(fs)
    db = DB(base)
    conn = db.open()
    conn.root()['sentences'] = OOBTree()
    transaction.commit()
    db.close()

subclasses = {}
def register(clsname, cls):
    subclasses[clsname] = cls


def root():
    base = FileStorage(fs)
    db = DB(base)
    conn = db.open()
    try:
        while True:
            yield conn.root()
    finally:
        db.close()

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

from nl.log import logger
import uuid
from nl.metanl import Namable, Subword

class Rule(Namable):
    """
    """
    def __init__(self, prems, cons):
        self.name = uuid.uuid4().get_hex()
        self.prems = prems
        self.cons = cons
        sane = False
        for x in xrange(len(self.prems)):
            if sane:
                break
            if isinstance(self.prems[0], Subword):
                self.prems = self.prems[1:] + [self.prems[0]]
            else:
                sane = True
        if not sane:
            raise SyntaxError('cannot have only subwords as prems')

    def put_action(self, vrs=None):
        """
        put rule in clips
        """
        if vrs is None:
            vrs = {}
        cprems = filter(None, map(lambda x:x.get_ce(vrs), self.prems))
        ccons = [con.put_action(vrs) for con in self.cons]
        return '(defrule %s %s => %s)' % (self.name,
                                          ' '.join(cprems),
                                          ' '.join(ccons))


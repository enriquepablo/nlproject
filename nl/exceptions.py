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

class Paradox(Exception):
    """
    """
    def __init__(self, expr1, expr2, messg):
        self.expr1 = expr1
        self.expr2 = expr2
        self.message = messg

    def __str__(self):
        return "Contradiction between \n%s\nand\n%s\n%s" % (self.expr1,
                                                            self.expr2,
                                                            self.message)


class NameNotFound(Exception):
    """
    """
    def __init__(self, name):
        self.name = name

    def __str__(self):
        return "I don't know this name: " + self.name



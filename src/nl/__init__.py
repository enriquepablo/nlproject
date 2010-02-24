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

import kb
from metanl import Number, Arith, Noun, Word, Verb, Subword, Namable
from thing import Thing
from state import Exists
from prop import Fact
from nltime import (Time, Instant, Duration, Finish, During,
                    Coincide, MinComStart, MaxComEnd,
                    Intersection, now)
from rule import Rule
from utils import change_now

from log import logger
import utils

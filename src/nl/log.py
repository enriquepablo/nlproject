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

# this snippet comes from
# http://www.mechanicalcat.net/richard/log/Python/Simple_usage_of_Python_s_logging_module

import os
import logging

logger = logging.getLogger('nl')
here = os.path.join(os.path.dirname(__file__))
log_dir = os.path.join(here, 'clips')
log_file = os.path.join(log_dir, 'log.clp')
if not os.path.isfile(log_file):
    if not os.path.isdir(log_dir):
        os.mkdir(log_dir)
    f = open(log_file, 'w')
    f.write('log file for nl\n\n')
    f.close()
hdlr = logging.FileHandler(log_file)
formatter = logging.Formatter('%(message)s')
# formatter = logging.Formatter('%(asctime)s %(levelname)s %(message)s')
hdlr.setFormatter(formatter)
logger.addHandler(hdlr)
logger.setLevel(logging.FATAL)

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
from datetime import datetime
import clips
from nl.clps import class_constraint
from nl import utils
from nl.arith import Number

_m = []

class Time(Number):
    """
    abstract ancestor for Instant & Duration
    """

    @classmethod
    def from_clips(cls, instance):
        if not isinstance(instance, clips._clips_wrap.Instance):
            try:
                return Instant(str(float(instance)))
            except ValueError:
                instance = clips.FindInstance(instance)
        if str(instance.Class.Name) == 'Duration':
            return Duration.from_clips(instance)
        return Instant(instance)

utils.register('Time', Time)

class Instant(Time):
    '''
    An instant in time.
    Instantiated with a positive integer or the string 'now'

    >>> i23 = Instant(23)
    >>> now = Instant('now')

    Can be used as the time attribute in a Fact

    >>> Fact(Thing('X1'), State('X2'), Instant('now'))

    or as start or end attributes in a Duration.

    >>> Duration(start=Instant(1), end=Instant(3))
    '''

    def __init__(self, *args, **kwargs):
        if args and args[0] == 'now':
            self.value = utils._now
        else:
            super(Instant, self).__init__(*args, **kwargs)

    def get_isc(self, vrs, templs, queries, parent=None):
        """
        """
        return super(Instant, self).get_isc(templs, queries, vrs)
#        if varpat.match(self.value):
#            if self.value in vrs and vrs[self.value]:
#                return clips_instance(*(vrs[self.value]))
#            return '?%s' % self.value
#        try:
#            return str(float(self.value))
#        except ValueError:
#            arg1 = self.arg1 != '' and self.arg1._get_number(vrs) or ''
#            arg2 = self.arg2 != '' and self.arg2._get_number(vrs) or ''
#            val '(%s %s %s)' % (self.value, arg1, arg2)
#
#'(or (and (eq (class %(ci)s) Duration) (<= (send %(ci)s get-start) %(val)s)) ())'

utils.register('Instant', Instant)

class Duration(Time):

    def __init__(self, var='', start=-1, end=-1):
        self.value = var
        if isinstance(start, MinComStart):
            self.pstart = start
        else:
            self.start = isinstance(start, Instant) and start or \
                                                 Instant(start)
        if isinstance(end, MaxComEnd):
            self.pend = end
        else:
            self.end = isinstance(end, Instant) and end or \
                                                  Instant(end)
            if float(self.end.value) == float(utils._now):
                self.end = Instant('-1.0')

    def __str__(self):
        if utils.varpat.match(self.value):
            return self.put_var({})
        else:
            return 'from %s till %s' % (self.start.put({}), self.end.put({}))

    @classmethod
    def from_clips(cls, instance):
        '''
        '''
        if not isinstance(instance, clips._clips_wrap.Instance):
            instance = clips.FindInstance(instance)
        start = Instant(instance.GetSlot('start'))
        if start.value in ('-1', '-1.0', 'now'):
            start = Instant(utils._now)
        end = Instant(instance.GetSlot('end'))
        if end.value == 'now':
            start = Instant('-1.0')
        return Duration(start=start, end=end)

    def get_constraint(self, vrs, ancestor, mod_path):
        '''
        build rule CE constraint for clips
        as a mod in a predicate in a prem in a rule
        '''
        ci = utils.clips_instance(ancestor, mod_path)
        if utils.varpat.match(self.value):
            return self.get_var_constraint(vrs, ancestor, mod_path, ci)
        else:
            core = '(= (send %s get-start) %s)' % (ci, self.start.get_constraint(vrs))
            return '&:(and %s (= (send %s get-end) %s))' % (core, ci, self.end.get_constraint(vrs))

# XXX falta resolver el caso en ambos get constraint de que no haya end.

    def get_slot_constraint(self, vrs):
        """
        build rule CE slot constraint for clips
        as time in a prem
        """
        newvar = utils._newvar()
        if utils.varpat.match(self.value):
            return self.get_var_slot_constraint(vrs, self.value)
        core = '(eq (send %s get-start) %s)' % (newvar, self.start.get_slot_constraint(vrs))
        return '?%(var)s&:(and %(core)s (eq (send %(var)s get-end) %(end)s))' % {'core': core, 'var': newvar, 'end': self.end.get_slot_constraint(vrs)}

    def put(self, vrs):
        if utils.varpat.match(self.value):
            return self.put_var(vrs)
        else:
            return '(make-instance of Duration (start %s) (end %s))' % \
                (getattr(self, 'pstart', False) and self.pstart.put(vrs) or \
                                           self.start.get_slot_constraint(vrs),
                 getattr(self, 'pend', False) and self.pend.put(vrs) or \
                                           self.end.get_slot_constraint(vrs))

    def get_isc(self, templs, queries, vrs, parent=None):
        """
        get instance-set condition;
        modify (instance-set templates, instance-set queries)
        """
        newvar = utils._newvar()
        if utils.varpat.match(self.value):
            if self.value in vrs:
                if vrs[self.value]:
                    queries.append('(eq ?%s %s)' % (newvar,
                                     utils.clips_instance(*(vrs[self.value]))))
                else:
                    newvar = self.value
            else:
                vrs[self.value] = ()
                newvar = self.value
            templs.append((newvar, 'Duration'))
            return '?%s' % newvar
        start = getattr(self, 'start', _m)
        if parent:
            newvar = '%s:time' % parent
            core_start = '(send ?%s:time get-start)' % parent
            core_end = '(send ?%s:time get-end)' % parent
        else:
            templs.append((newvar, 'Duration'))
            core_start = '?%s:start' % newvar
            core_end = '?%s:end' % newvar
        if start is not _m and \
           not (utils.varpat.match(start.value) and \
           start.value not in vrs):
            queries.append('(= %s %s)' % (core_start,
                                  start.get_isc(templs, queries, vrs)))
        end = getattr(self, 'end', _m)
        if end is not _m and \
           not (utils.varpat.match(end.value) and \
           end.value not in vrs):
            queries.append('(= %s %s)' % (core_end,
                                  end.get_isc(templs, queries, vrs)))
        return '?%s' % newvar

utils.register('Duration', Duration)

class Finish(utils.Name):
    def __init__(self, duration, instant):
        self.duration = isinstance(duration, Duration) and \
                                duration or Duration(duration)
        self.instant = isinstance(instant, Instant) and \
                                instant or Instant(instant)

    def put_action(self, vrs):
        return '(modify-instance %s (end %s))' % (self.duration.put(vrs), self.instant.put(vrs))

utils.register('Finish', Finish)


class During(utils.Name):
    '''
    given an instant and a duration, build a condition for a rule
    that tests whether the instant is within the duration
    '''
    def __init__(self, instant, *durations):
        self.instant = isinstance(instant, Instant) and \
                                   instant or Instant(instant)
        self.durations = [isinstance(duration, str) and \
                                   Duration(duration) or duration
                                         for duration in durations]

    def get_ce(self, vrs):
        durs = []
        for duration in self.durations:
            durs.append('(test (and (<= (send %(dur)s get-start) %(ins)s) (or (= (send %(dur)s get-end) -1) (>= (send %(dur)s get-end) %(ins)s))))' % {'dur': duration.put(vrs), 'ins': self.instant.put(vrs)})
        return ' '.join(durs)

utils.register('During', During)


class DurationOpMixin(utils.Name):
    '''
    Abstract ancestor of classes constructed with a sequence of durations
    '''
    def __init__(self, *args):
        self.durations = \
          [isinstance(dur, Duration) and dur or Duration(dur) for dur in args]


utils.register('DurationOpMixin', DurationOpMixin)


class Coincide(DurationOpMixin):
    '''
    given a set of durations, build a condition for a rule
    that tests whether there is an intersection between them
    '''

    def get_ce(self, vrs):
        return """
                (test (or (and (> (mincomstart %(durs)s) -1)
                               (<= (mincomstart %(durs)s) (maxcomend %(durs)s)))
                          (= (maxcomend %(durs)s) -1))
                )
                """ % {'durs': ' '.join([dur.put(vrs) for dur in self.durations])}


utils.register('Coincide', Coincide)

class Intersection(DurationOpMixin):
    '''
    given a set of durations,
    put a duration that is the intersection of them all
    assume that the intersection exists
    '''

    def put(self, vrs):
        return """
                (make-instance of Duration (start (mincomstart %(durs)s))
                                           (end (maxcomend %(durs)s)))
                """ % {'durs': ' '.join([dur.put(vrs) for dur in self.durations])}

utils.register('Intersection', Intersection)

class MinComStart(DurationOpMixin):
    """
    given a set of durations, find out the minimum common instant
    """

    def put(self, vrs):
        instants = [dur.put(vrs) for dur in self.durations]
        return '(mincomstart %s)' % ' '.join(instants)

utils.register('MinComStart', MinComStart)

class MaxComEnd(DurationOpMixin):
    """
    given a set of durations, find out the maximum common instant
    """

    def put(self, vrs):
        instants = [dur.put(vrs) for dur in self.durations]
        return '(maxcomend %s)' % ' '.join(instants)

utils.register('MaxComEnd', MaxComEnd)

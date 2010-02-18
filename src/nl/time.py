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
from nl.log import logger
from nl import utils
from nl.metanl import Namable, Number

_m = []

class Time(Namable):
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
        return Duration.from_clips(instance)


class Instant(Time, Number):
    '''
    An instant in time.
    Instantiated with a positive integer or the string 'now'

    >>> i23 = Instant(23)
    >>> now = Instant('now')

    Can be used as the time attribute in a Fact

    >>> from nl import Fact, Thing, Exists
    >>> f = Fact(Thing('X1'), Exists('X2'), Instant('now'))

    or as start or end attributes in a Duration.

    >>> d = Duration(start=Instant(1), end=Instant(3))
    '''

    def __init__(self, *args, **kwargs):
        if args and args[0] == 'now':
            self.value = utils._now
        else:
            super(Instant, self).__init__(*args, **kwargs)

    def get_isc(self, templs, queries, vrs, parent=None):
        """
        """
        num = self._get_number(vrs)
        if parent and (not utils.varpat.match(num[1:]) or \
                       vrs.has_key(num[1:])):
            if vrs.has_key(num[1:]) and vrs[num[1:]]:
                num = utils.clips_instance(*(vrs[num[1:]]))
            queries.append( '''
               (or (and (eq (class ?%(parent)s:time) Duration)
                        (<= (send ?%(parent)s:time get-start) %(self)s)
                        (or (= (send ?%(parent)s:time get-end) -1)
                            (>= (send ?%(parent)s:time get-end) %(self)s)))
                   (eq ?%(parent)s:time %(self)s))
            ''' % {'parent': parent, 'self': num} )
        return num


class Duration(Time):
    '''
    A duration in time.
    Istantiated either with a string containing a variable name,
    or with two named args, start and end, that can either be
    instances of Instant or strings to be converted in such.
    Can be used as time argument for instances of Fact.
    '''

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
        if utils.varpat.match(self.value):
            return self.get_var_slot_constraint(vrs, self.value)
        core = '(eq (send %s get-start) %s)' % (self.value, self.start.get_slot_constraint(vrs))
        return '?%(var)s&:(and %(core)s (eq (send %(var)s get-end) %(end)s))' % {'core': core, 'var': self.value, 'end': self.end.get_slot_constraint(vrs)}

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
        if utils.varpat.match(self.value):
            if self.value in vrs:
                if vrs[self.value]:
                    queries.append('(eq ?%s %s)' % (self.value,
                                     utils.clips_instance(*(vrs[self.value]))))
            vrs[self.value] = ()
            templs.append((self.value, 'Duration'))
            return '?%s' % self.value
        start = getattr(self, 'start', _m)
        if parent:
            core_start = '(send ?%s:time get-start)' % parent
            core_end = '(send ?%s:time get-end)' % parent
        else:
            templs.append((self.value, 'Duration'))
            core_start = '?%s:start' % self.value
            core_end = '?%s:end' % self.value
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
        return '?%s' % self.value

    def get_ism(self,  templs, queries, vrs, newvar='time'):
        """
        get instance-set method;
        return (instance-set templates, instance-set queries)
        """
        if utils.varpat.match(self.value):
            templs.append((self.value, self.__class__.__name__))
            vrs[self.value] = ()
        else:
            templs.append((newvar, self.__class__.__name__))
            queries.append('(eq ?%s %s)' % (newvar, self.put(vrs)))


class Finish(Namable):
    '''
    Instantiated with an Instance of duration as only arg.

    to be used as a consecuence in a rule
    '''
    def __init__(self, duration, instant):
        self.duration = isinstance(duration, Duration) and \
                                duration or Duration(duration)
        self.instant = isinstance(instant, Instant) and \
                                instant or Instant(instant)

    def put_action(self, vrs):
        return '(modify-instance %s (end %s))' % (self.duration.put(vrs), self.instant.put(vrs))


class During(Namable):
    '''
    given an instant and a list ofdurations,
    build a condition for a rule
    that tests whether the instant
    is within the all durations.
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


class DurationOpMixin(Namable):
    '''
    Abstract ancestor of classes constructed with a sequence of durations
    '''
    def __init__(self, *args):
        self.durations = \
          [isinstance(dur, Duration) and dur or Duration(dur) for dur in args]


class Coincide(DurationOpMixin):
    '''
    given a set of durations, build a condition for a rule
    that tests whether there is an intersection between them
    '''

    def get_ce(self, vrs):
        return """
(test
  (or
    (<= (max-start %(durs)s) (min-end %(durs)s))
    (and
      (<= (max-start %(durs)s) (python-call ptime))
      (= (min-end %(durs)s) -1)))
)
        """ % {'durs': ' '.join([dur.put(vrs) for dur in self.durations])}



class Intersection(DurationOpMixin):
    '''
    given a set of durations,
    put a duration that is the intersection of them all
    assume that the intersection exists.

    To be used wherever a Duration would.
    '''

    def put(self, vrs):
        return """
(make-instance of Duration (start (max-start %(durs)s))
                           (end (min-end %(durs)s)))
                """ % {'durs': ' '.join([dur.put(vrs) for dur in self.durations])}


class MinComStart(DurationOpMixin):
    """
    given a set of durations, find out the minimum common instant

    To be used wherever an instant would, except as the end of a duration
    """

    def put(self, vrs):
        instants = [dur.put(vrs) for dur in self.durations]
        return '(max-start %s)' % ' '.join(instants)


class MaxComEnd(DurationOpMixin):
    """
    given a set of durations, find out the maximum common instant

    To be used as the end of another duration
    """

    def put(self, vrs):
        instants = [dur.put(vrs) for dur in self.durations]
        return '(min-end %s)' % ' '.join(instants)


class InstantOpMixin(Namable):
    '''
    Abstract ancestor of classes constructed with an instant
    '''
    def __init__(self, instant):
        self.instant = instant


class Past(InstantOpMixin):
    """
    init'd with an instant, returns true if
    the instant is in the past
    """
    def get_ce(vrs):
        i = self.instant.put(vrs)
        return '''(test (and (neq %s -1)
                             (< %s (python-call ptime))))''' % (i, i)

class Present(InstantOpMixin):
    """
    init'd with an instant, returns true if
    the instant is the present
    """
    def get_ce(vrs):
        i = self.instant.put(vrs)
        return '''(test (or (eq %s -1)
                            (eq %s (python-call ptime))))''' % (i, i)

class Future(InstantOpMixin):
    """
    init'd with an instant, returns true if
    the instant is in the future
    """
    def get_ce(vrs):
        i = self.instant.put(vrs)
        return '''(test (> %s (python-call ptime)))''' % (i, i)


from nl.utils import _now
import time
def now():
    global _now
    _now = int(time.time())
    return _now

now()

def ptime():
    return clips.Float(float(_now))

clips.RegisterPythonFunction(ptime)

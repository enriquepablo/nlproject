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

import time
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

    def get_isc(self, queries, vrs, ancestor, mod_path):
        """
        """
        num = self._get_number(vrs)
        if not utils.varpat.match(num[1:]) or \
                       vrs.has_key(num[1:]):
            if vrs.has_key(num[1:]) and vrs[num[1:]]:
                num = utils.clips_instance(*(vrs[num[1:]]))
            queries.append( '''
               (or (and (eq (class ?%(parent)s:time) Duration)
                        (<= (send ?%(parent)s:time get-start) %(self)s)
                        (or (and (= (send ?%(parent)s:time get-end) -1.0)
                                 (>= (python-call ptime) %(self)s))
                            (>= (send ?%(parent)s:time get-end) %(self)s)))
                   (eq ?%(parent)s:time %(self)s))
            ''' % {'parent': ancestor, 'self': num} )
        return num


class Duration(Time):
    '''
    A duration in time.
    Istantiated either with a string containing a variable name,
    or with two named args, start and end, that can either be
    instances of Instant or strings to be converted in such.
    Can be used as time argument for instances of Fact.
    '''

    def __init__(self, var='', start=-1.0, end=-1.0):
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
            if not utils.varpat.match(str(self.end.value)) and \
                   float(self.end.value) == utils._now:
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
        end = Instant(instance.GetSlot('end'))
        return Duration(start=start, end=end)

#    def get_constraint(self, vrs, ancestor, mod_path):
#        '''
#        build rule CE constraint for clips
#        as a mod in a predicate in a prem in a rule
#        '''
#        ci = utils.clips_instance(ancestor, mod_path)
#        if utils.varpat.match(self.value):
#            return self.get_var_constraint(vrs, ancestor, mod_path, ci)
#        else:
#            core = '(= (send %s get-start) %s)' % (ci, self.start.get_constraint(vrs))
#            return '&:(and %s (= (send %s get-end) %s))' % (core, ci, self.end.get_constraint(vrs))

# XXX falta resolver el caso en ambos get constraint de que no haya end.

    def get_slot_constraint(self, vrs):
        """
        build rule CE slot constraint for clips
        as time in a prem
        """
        if utils.varpat.match(self.value):
            return self.get_var_slot_constraint(vrs, self.value)
        newvar = utils._newvar()
        constraint = '?'+newvar
        for x in ('start', 'end'):
            x_constraint = getattr(self, x).get_constraint(vrs, newvar, [x])
            if x_constraint:
                constraint += '&:(eq (send ?%s get-%s) %s)' % (newvar, x, x_constraint)
        return constraint

    def put(self, vrs):
        if utils.varpat.match(self.value):
            return self.put_var(vrs)
        else:
            return '(make-instance of Duration (start %s) (end %s))' % \
                (getattr(self, 'pstart', False) and self.pstart.put(vrs) or \
                                           self.start.get_slot_constraint(vrs),
                 getattr(self, 'pend', False) and self.pend.put(vrs) or \
                                           self.end.get_slot_constraint(vrs))

    def get_isc(self, queries, vrs, ancestor, mod_path):
        """
        get instance-set condition;
        modify (instance-set templates, instance-set queries)
        """
        ci = utils.clips_instance(ancestor, mod_path)
        if utils.varpat.match(self.value):
            if self.value in vrs:
                if vrs[self.value]:
                    queries.append('(eq %s %s)' % (ci,
                                     utils.clips_instance(*(vrs[self.value]))))
                else:
                    queries.append('(eq %s ?%s)' % (ci, self.value))
            else:
                vrs[self.value] = (ancestor, mod_path)
                queries.append('(eq (class %s) Duration)' % ci)
            return
        start = getattr(self, 'start', _m)
        if utils.varpat.match(str(start.value)) and not vrs.has_key(start.value):
            vrs[start.value] = (ancestor, mod_path+('start',))
        nstart = start.get_isc([], vrs, ancestor, mod_path)
        end = getattr(self, 'end', _m)
        if utils.varpat.match(str(end.value)) and not vrs.has_key(end.value):
            vrs[end.value] = (ancestor, mod_path+('end',))
        nend = end.get_isc([], vrs, ancestor, mod_path)
        queries.append( '''
                   (and (eq (class (send ?%(parent)s get-time)) Duration)
                        (<= (send (send ?%(parent)s get-time) get-start) %(start)s)
                        (or (= (send (send ?%(parent)s get-time) get-end) %(end)s)
                            (and (= (send (send ?%(parent)s get-time) get-end) -1.0)
                                 (>= (python-call ptime) %(end)s))
                            (and (= %(end)s -1.0)
                                 (<= (python-call ptime) (send (send ?%(parent)s get-time) get-end)))
                            (and (<> %(end)s -1.0)
                                 (>= (send (send ?%(parent)s get-time) get-end) %(end)s))))
            ''' % {'parent': ancestor, 'start': nstart, 'end': nend} )

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


#(find-all-instances ((?q0 Fact))
#
#    (and (eq (send ?q0 get-subject) [jane])
#         (and (or
#              (eq (class (send ?q0 get-predicate)) Can)
#              (subclassp (class (send ?q0 get-predicate)) Can)))
#         (and (eq (class (send ?q0 get-time)) Duration)
#              (<= (send (send ?q0 get-time) get-start) 1270070257.0)
#              (or (and (= (send (send ?q0 get-time) get-end) -1.0)
#                       (>= (python-call ptime) -1.0))
#                  (and (= -1.0 -1)
#                       (<= (python-call ptime) (send (send ?q0 get-time) get-end)))
#                  (>= (send (send ?q0 get-time) get-end) -1.0)))
#         (eq ?q0:truth 1)))




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
        return """
(test
  (and
   (<= (max-start %(durs)s) %(ins)s)
   (or
     (and (= (min-end %(durs)s) -1.0)
          (>= (python-call ptime) %(ins)s))
     (>= (min-end %(durs)s) %(ins)s))))
       """ % {'durs': ' '.join([dur.put(vrs) for dur in self.durations]),
              'ins': self.instant.put(vrs)}


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
      (= (min-end %(durs)s) -1.0)))
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
        return '''(test (and (neq %s -1.0)
                             (< %s (python-call ptime))))''' % (i, i)

class Present(InstantOpMixin):
    """
    init'd with an instant, returns true if
    the instant is the present
    """
    def get_ce(vrs):
        i = self.instant.put(vrs)
        return '''(test (or (eq %s -1.0)
                            (eq %s (python-call ptime))))''' % (i, i)

class Future(InstantOpMixin):
    """
    init'd with an instant, returns true if
    the instant is in the future
    """
    def get_ce(vrs):
        i = self.instant.put(vrs)
        return '''(test (> %s (python-call ptime)))''' % (i, i)


def now(new=0):
    if new:
        utils._now = float(new)
    else:
        t = float(int(time.time() * utils._time_granularity))
        delta = t - utils._now
        delta = delta < 1 and 1.0 or delta
        utils._now = utils._now + utils._time_start_delta + delta
    return utils._now

def time_granularity(gr):
    utils._time_granularity = gr

def start_of_time(start_delta):
    utils._time_start_delta = start_delta
    now()

def ptime():
    return clips.Float(utils._now)

clips.RegisterPythonFunction(ptime)

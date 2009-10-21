# -*- coding: utf-8 -*-
'''
Instant
    * en una frase -- un num
    * en una condición -- num / var / arith / now
    * en una conclusión -- num / var / arith



# if someone wants to do something, and can do it, she does it
r1 = Rule([
        Prop(Person('X1'), Wants(to=State('X4')), Time('X2')),
        Prop(Person('X1'), Can(what=State('X4')), Time('X3')),
        During(Time('X2'), Time('X3'))
        ],[
        Prop(Person('X1'), State('X4'), Time('X2'))])

def During(inst i, dur d)
    i > d.start and
    (not d.end or i < dend)

hay instante y duration. duration tiene instantes start y end.
una duration siempre tiene start. si no tiene end, se entiende que es futuro.
la definición de durante es la de arriba.
se abre y cierra un instante now durante un cierto periodo de interacción con el sistema.
existe un time 'now' que entra en clips como el now de la linea superior.
El put action no hace un make instance en una conclusión; de modo que al cerrar una duración, se cierren en todas sus consecuencias.

'''
from datetime import datetime
from log import logger

from nl.utils import register, subclasses, clips, Name, varpat, class_constraint, clips_instance
from nl.arith import Number
from nl.thing import _newvar

_m = []


# XXX not thread safe
_now = str(datetime.now().toordinal())

def start_instant():
    global _now
    _now = str(datetime.now().toordinal())

def end_instant():
    global _now
    _now = None


class Time(Number):
    """
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

register('Time', Time)

class Instant(Time):

    def __init__(self, *args, **kwargs):
        if args and args[0] == 'now':
            self.value = _now
        else:
            super(Instant, self).__init__(*args, **kwargs)

#    def get_isc(self, vrs):
#        """
#        """
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

register('Instant', Instant)

now = Instant('now')

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
            if float(self.end.value) == float(_now):
                self.end = Instant('-1.0')

    def __str__(self):
        if varpat.match(self.value):
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
            start = Instant(_now)
        end = Instant(instance.GetSlot('end'))
        return Duration(start=start, end=end)

    def get_constraint(self, vrs, ancestor, mod_path):
        '''
        build rule CE constraint for clips
        as a mod in a predicate in a prem in a rule
        '''
        ci = clips_instance(ancestor, mod_path)
        if varpat.match(self.value):
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
        newvar = _newvar()
        if varpat.match(self.value):
            return self.get_var_slot_constraint(vrs, self.value)
        core = '(eq (send %s get-start) %s)' % (newvar, self.start.get_slot_constraint(vrs))
        return '?%(var)s&:(and %(core)s (eq (send %(var)s get-end) %(end)s))' % {'core': core, 'var': newvar, 'end': self.end.get_slot_constraint(vrs)}

    def put(self, vrs):
        if varpat.match(self.value):
            return self.put_var(vrs)
        else:
            if getattr(self, 'pstart', False):
                return '(make-instance of Duration (start %s) (end %s))' % (self.pstart.put(vrs), self.pend.put(vrs))
            return '(make-instance of Duration (start %s) (end %s))' % (self.start.get_slot_constraint(vrs), self.end.get_slot_constraint(vrs))

    def get_isc(self, templs, queries, vrs):
        """
        get instance-set condition;
        modify (instance-set templates, instance-set queries)
        """
        newvar = _newvar()
        if varpat.match(self.value):
            if self.value in vrs:
                if vrs[self.value]:
                    queries.append('(eq ?%s %s)' % (newvar,
                                     clips_instance(*(vrs[self.value]))))
                else:
                    newvar = self.value
            else:
                newvar = self.value
        templs.append('(?%s Duration)' % newvar)
        start = getattr(self, 'start', _m)
        if start is not _m and not (varpat.match(start.value) and start.value not in vrs):
            queries.append('(= ?%s:start %s)' % (newvar,
                                           start.get_isc(templs, queries, vrs)))
        end = getattr(self, 'end', _m)
        if end is not _m and not (varpat.match(end.value) and end.value not in vrs):
            queries.append('(= ?%s:end %s)' % (newvar,
                                           end.get_isc(templs, queries, vrs)))
        return '?%s' % newvar

register('Duration', Duration)

class Finish(Name):
    def __init__(self, duration):
        self.duration = duration

    def put_action(self, vrs):
        return '(send %s put-end %s)' % (self.duration.put(vrs), _now)

register('Finish', Finish)


class During(Name):
    '''
    given an instant and a duration, build a condition for a rule
    that tests whether the instant is within the duration
    '''
    def __init__(self, instant, duration):
        self.instant = instant
        self.duration = duration

    def get_ce(self, vrs):
        return '(test (and (<= (send %(dur)s get-start) %(ins)s) (or (= (send %(dur)s get-end) -1) (>= (send %(dur)s get-end) %(ins)s))))' % {'dur': self.duration.put(vrs), 'ins': self.instant.put(vrs)}

register('During', During)


class Coincide(Name):
    '''
    given a set of durations, build a condition for a rule
    that tests whether there is an intersection between them
    '''
    def __init__(self, *args):
        self.durations = \
          [isinstance(dur, Duration) and dur or Duration(dur) for dur in args]

    def get_ce(self, vrs):
        return """
                (test (or (and (> (mincomstart %(durs)s) -1)
                               (<= (mincomstart %(durs)s) (maxcomend %(durs)s)))
                          (= (maxcomend %(durs)s) -1))
                )
                """ % {'durs': ' '.join([dur.put(vrs) for dur in self.durations])}


register('Coincide', Coincide)


duration_clps = '(defclass Duration (is-a Name) (slot start (type NUMBER) (pattern-match reactive)) (slot end (type NUMBER) (pattern-match reactive)))'
logger.info(duration_clps)
clips.Build(duration_clps)

extract_clp = '''
(deffunction extract-now ($?instants)
    (if (= (length$ ?instants) 0) then
        (return -1)
    else
        (bind ?count 0)
        (bind ?not-now (create$))
        (while (> (length$ ?instants) 0) do
            (bind ?instant (nth$ 1 ?instants))
            (bind ?instants (rest$ ?instants))
            (if (= ?instant -1)
            then (bind ?count (+ ?count 1))
            else (bind ?not-now (insert$ ?not-now 1 ?instant)))
            )
        (return (insert$ ?not-now 1 ?count))
    )
)
'''

maxcomend_clp = '''
(deffunction maxcomend ($?durations)
    (bind ?ends (create$))
    (while (> (length$ ?durations) 0)
        (bind ?ends (insert$ ?ends 1 (send (nth$ 1 ?durations) get-end)))
        (bind ?durations (rest$ ?durations))
        )
    (bind ?ret (extract-now ?ends))
    (bind ?pasts (rest$ ?ret))
    (if (> (length$ ?pasts) 0)
    then (return (min (expand$ ?pasts)))
    else (return -1)
    )
)
'''

mincomstart_clp = '''
(deffunction mincomstart ($?durations)
    (bind ?starts (create$))
    (while (> (length$ ?durations) 0)
        (bind ?starts (insert$ ?starts 1 (send (nth$ 1 ?durations) get-start)))
        (bind ?durations (rest$ ?durations))
        )
    (bind ?ret (extract-now ?starts))
    (bind ?nows (nth$ 1 ?ret))
    (if (> ?nows 0)
    then (return -1)
    else (bind ?ret (rest$ ?ret))
         (return (max (expand$ ?ret)))
    )
)
'''

logger.info(extract_clp)
clips.Build(extract_clp)
logger.info(maxcomend_clp)
clips.Build(maxcomend_clp)
logger.info(mincomstart_clp)
clips.Build(mincomstart_clp)

class MinComStart(Name):
    """
    given a set of durations, find out the minimum common instant
    """
    def __init__(self, *args):
        self.durations = \
          [isinstance(dur, Duration) and dur or Duration(dur) for dur in args]

    def put(self, vrs):
        instants = [dur.put(vrs) for dur in self.durations]
        return '(mincomstart %s)' % ' '.join(instants)

register('MinComStart', MinComStart)

class MaxComEnd(Name):
    """
    given a set of durations, find out the maximum common instant
    """
    def __init__(self, *args):
        self.durations = \
          [isinstance(dur, Duration) and dur or Duration(dur) for dur in args]

    def put(self, vrs):
        instants = [dur.put(vrs) for dur in self.durations]
        return '(maxcomend %s)' % ' '.join(instants)

register('MaxComEnd', MaxComEnd)

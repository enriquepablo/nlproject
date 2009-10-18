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
            core = '(= (send %s get-start) %s)' % (ci, self.start.get_slot_constraint(vrs))
            return '&:(and %s (= (send %s get-end) %s))' % (core, ci, self.end.get_slot_constraint(vrs))

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
    '''
    def __init__(self, instant, duration):
        self.instant = instant
        self.duration = duration

    def get_ce(self, vrs):
        return '(test (and (<= (send %(dur)s get-start) %(ins)s) (or (= (send %(dur)s get-end) -1) (>= (send %(dur)s get-end) %(ins)s))))' % {'dur': self.duration.put(vrs), 'ins': self.instant.put(vrs)}

register('During', During)


class Coincide(Name):
    '''
    '''
    def __init__(self, dur1, dur2):
        self.dur1 = isinstance(dur1, Duration) and dur1 or Duration(dur1)
        self.dur2 = isinstance(dur2, Duration) and dur1 or Duration(dur2)

    def get_ce(self, vrs):
        return '(test (or (and (>= (send %(dur1)s get-start) (send %(dur2)s get-start)) (or (<= (send %(dur1)s get-start) (send %(dur2)s get-end)) (= (send %(dur2)s get-end) -1))) (and (>= (send %(dur2)s get-start) (send %(dur1)s get-start)) (or (<= (send %(dur2)s get-start) (send %(dur1)s get-end)) (= (send %(dur2)s get-end) -1)))))' % {'dur1': self.dur1.put(vrs), 'dur2': self.dur2.put(vrs)}

register('Coincide', Coincide)


duration_clps = '(defclass Duration (is-a Name) (slot start (type NUMBER) (pattern-match reactive)) (slot end (type NUMBER) (pattern-match reactive)))'
logger.info(duration_clps)
clips.Build(duration_clps) # XXX esto no debbería ir aquí, sino en un método de inicialización.

mincomstart_clps = '''

(deffunction mincomstart (?dur1 ?dur2)
    (return (max (send ?dur1 get-start) (send ?dur2 get-start)))
)
'''

clips.Build(mincomstart_clps)
logger.info(mincomstart_clps)

class MinComStart(Name):
    def __init__(self, dur1, dur2):
        self.dur1 = isinstance(dur1, Duration) and dur1 or Duration(dur1)
        self.dur2 = isinstance(dur2, Duration) and dur1 or Duration(dur2)

    def put(self, vrs):
        return '(mincomstart %s %s)' % (self.dur1.put(vrs), self.dur2.put(vrs))

register('MinComStart', MinComStart)

maxcomend_clps = '''

(deffunction maxcomend (?dur1 ?dur2)
    (bind ?e1 (send ?dur1 get-end))
    (bind ?e2 (send ?dur2 get-end))
    (if (= ?e1 ?e2) then (return ?e1))
    (if (= ?e2 -1) then
        (return ?e1)
    )
    (if (= ?e1 -1) then
        (return ?e2)
    )
    (return (min ?e1 ?e2))
)
'''


logger.info(maxcomend_clps)
clips.Build(maxcomend_clps)

class MaxComEnd(Name):
    def __init__(self, dur1, dur2):
        self.dur1 = isinstance(dur1, Duration) and dur1 or Duration(dur1)
        self.dur2 = isinstance(dur2, Duration) and dur1 or Duration(dur2)

    def put(self, vrs):
        return '(maxcomend %s %s)' % (self.dur1.put(vrs), self.dur2.put(vrs))

register('MaxComEnd', MaxComEnd)

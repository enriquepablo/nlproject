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

import uuid
import clips
from nl.log import logger
from nl import utils
from nl.clps import class_constraint


# marker object
_m = []

class Word(type):
    """
    When Namable is extended,
    __init__ adds 1 defclass to clips
    creating a subclass of Namable.
    And registers the class in utils.subclasses.
    If utils.varpat matches the classname,
    however, __new__ builds a ClassVar intance
    and returns it, thus bypassing __init__.
    In that case, bases can be a subclass of
    Exists or Thing.
    """
    def __new__(cls, classname, bases=None, newdict=None):

        if utils.varpat.match(classname):
            if bases:
                return ClassVar(classname, bases)
            elif getattr(cls, 'cls', False):
                return ClassVar(classname, cls.cls)
            return ClassVar(classname, utils.get_class('Namable'))
        cls.value = ''
        return super(Word, cls).__new__(cls, classname, bases, newdict)

    def __init__(cls, classname, bases, newdict, clp=''):
        super(Word, cls).__init__(classname, bases, newdict)
        if clp:
            logger.info(clp)
            clips.Build(clp)
        utils.register(classname, cls)

    def clsput(self, vrs, ancestor=None, mod_path=None):
        '''
        return clips snippet to be used
        when the class is the subject in a fact
        or a mod in a predicate
        and the sentence is being asserted
        or is in the head of a rule.
        Also when the class is used as subject
        of a fact in the tail of a rule.
        '''
        return self.__name__

    def get_constraint_cls(self, vrs, ancestor, mod_path):
        '''
        return clips snippet to be used
        when the class is a mod in a predicate
        and the sentence is in the tail of a rule.
        '''
        return '&:%s' % self.get_query_cls(vrs, ancestor, mod_path)[0]

    def get_query_cls(self, vrs, ancestor, mod_path):
        ci = utils.clips_instance(ancestor, mod_path)
        return ['(eq %s %s)' % (self.__name__, ci)]

    def get_isc_cls(self, queries, vrs, ancestor, mod_path):
        """
        build clips snippet
        to be used when the class is the subject in a fact
        or a mod in a predicate
        and the sentence is in a query.
        return (instance-set templates, instance-set queries)
        """
        q = self.get_query_cls(vrs, ancestor, mod_path)
        queries += q

    @classmethod
    def from_clips(cls, instance):
        '''
        build nl instance starting from a clips instance
        or instance name
        '''
        if not isinstance(instance, clips._clips_wrap.Class):
            instance = clips.FindClass(instance)
        clsname = str(instance.Name)
        return utils.get_class(clsname)

utils.register('Word', Word)

class ClassVar(object):
    '''
    Used in rules, in the head or tail,
    as subject or mod in a predicate,
    when Word is called with a string
    that matches varpat.
    Intances can be called, and give back
    an instance of ClassVarVar,
    that can handle clips for
    the case in which we have both
    the class and the name or mods
    as variables.

    This is not to be part of the public api
    '''
    def __init__(self, var, cls=None):
        self.value = var
        self.cls = cls and cls or utils.get_class('Namable')
        self.ob = self.cls(self.value)

    def __call__(self, var='', **kwargs):
        if utils.varpat.match(var):
            return ClassVarVar(self.value, self.cls, var, **kwargs)
        if issubclass(self.cls, utils.get_class('Exists')):
            return self.cls(var, _clsvar=self.value, **kwargs)
        return self.cls(var, **kwargs)

    def get_constraint(self, vrs, ancestor, mod_path):
        '''
        return clips snippet to be used
        when the class var is a mod in a predicate
        and the sentence is in the tail of a rule.
        '''
        constraint = self.get_query(vrs, ancestor, mod_path)
        if not constraint:
            return ''
        return '&:%s' % '&:'.join(constraint)

    def get_query(self, vrs, ancestor, mod_path):
        ci = utils.clips_instance(ancestor, mod_path)
        if self.value in vrs:
            return self.ob.get_query(vrs, ancestor, mod_path)
        else:
            vrs[self.value] = (ancestor, mod_path)
            return ['''(or (eq %(ci)s %(cls)s)
                            (subclassp %(ci)s %(cls)s))''' % {
                                    'ci': ci,
                                    'cls': self.cls.__name__}]

    def get_slot_constraint(self, vrs):
        """
        return clips snippet to be used
        when the class var is predicate or subject
        and the sentence is in the tail of a rule.
        """
        if self.value in vrs:
            return self.ob.get_var_slot_constraint(vrs, self.value)
        else:
            vrs[self.value] = ()
            return '''?%(cvar)s&:(or (eq ?%(cvar)s %(cls)s)
                                  subclassp ?%(cvar)s %(cls)s)''' % {
                                    'cvar': self.value,
                                    'cls': self.cls.__name__}

    def get_isc(self, queries, vrs, ancestor, mod_path):
        """
        build clips snippet
        to be used when the class var is the subject in a fact
        or a mod in a predicate
        and the sentence is in a query.
        return (instance-set templates, instance-set queries)
        """
        q = self.get_query(vrs, ancestor, mod_path)
        queries += q

    def put(self, vrs):
        # a hack
        return self.cls(self.value).put(vrs)


class ClassVarVar(object):
    '''
    Intances of ClasVar can be called, and give back
    an instance of ClassVarVar,
    that can handle clips for
    the case in which we have both
    the class and the name or mods
    as variables.

    This is not to be part of the public api
    '''
    def __init__(self, clsvar, cls, var, **kwargs):
        self.value = var
        self.clsvar = clsvar
        self.cls = cls
        self.ob = cls(var, _clsvar=clsvar, **kwargs)

    def get_constraint(self, vrs, ancestor, mod_path):
        '''
        return clips snippet to be used
        when the variable is a mod in a predicate
        and the sentence is in the tail of a rule.
        '''
        constraint = self.get_query(vrs, ancestor, mod_path)
        if not constraint:
            return ''
        return '&:%s' % '&:'.join(constraint)

    def get_query(self, vrs, ancestor, mod_path):
        ci = utils.clips_instance(ancestor, mod_path)
        constraint = []
        if not self.value or self.value in vrs:
            if self.clsvar and self.clsvar not in vrs:
                if vrs[self.value]:
                    vrs[self.clsvar] = (
                               utils.clips_instance(*(vrs[self.value])),
                               [], ['class'])
                else:
                    vrs[self.clsvar] = (ancestor, mod_path, ['class'])
            return self.ob.get_query(vrs, ancestor, mod_path)
        else:
            vrs[self.value] = (ancestor, mod_path)
            if self.clsvar in vrs:
                if vrs[self.clsvar]:
                    return ['(eq (class %(var)s) %(cls)s)' % {
                                    'var': ci,
                         'cls': utils.clips_instance(*(vrs[self.clsvar]))}]
                else:
                    return ['(eq (class %(var)s) %(cls)s)' % {
                                    'var': ci,
                                    'cls': self.clsvar}]
            vrs[self.clsvar] = (ancestor, mod_path, ['class'])
            return ['''(or
                          (eq (class %(ci)s) %(cls)s)
                          (subclassp (class %(ci)s) %(cls)s))''' % {
                                    'ci': ci,
                                    'cls': self.cls.__name__}]

    def get_slot_constraint(self, vrs):
        """
        return clips snippet to be used
        when the variable is predicate or subject
        and the sentence is in the tail of a rule.

        In case we are a Noun:

        In case we are a Verb:
        * in case the clsvar is vrs but not the var:
          * in case


          XXX dos cosas: aquí:

          (logical (object (is-a Fact) (subject ?P1&:(or (eq (class ?P1) Person) (subclassp (class ?P1) Person))) (predicate ?Y10&:(or (eq (class ?Y10) WfAction) (subclassp (class ?Y10) WfAction))&:(eq (send ?Y10 get-what) ?C1)) (time ?I1) (truth 1))) (logical (object (is-a Fact) (subject ?C1&:(eq ?C1 (send ?Y10 get-what)))
         se repite
        (eq ?C1 (send ?Y10 get-what))

        otra cosa: tanto en ClassVar como en ClassVarVar, al hacer get_slot_constraint (NO he mirado get_constraint, pero será igual) no se toma en cuanta cuando clsvar es var y está en vrs.

        ClassVar lo tiene que hacer es, en __call__, si no se le llama con una variable sino con argumantos, y es un verbo (en el caso de una cosa da igual, está unívocamente determinada por el nombre propio), tiene que pasar el clsvar para que verb lo tenga en cuenta cuando haga get_slot constraint (y posiblemente los demás)

        ClassVarVar lo tiene que tener en cuenta en get_slot_constraint cuando self value está en vrs, entonces tiene que pasarlo a self ob para que lo tenga en cuenta en get_var_slot_constraint


        HAY QUE PONERSELO TB A THING, TODO LO DEL _CLSVAR
        """
        if self.value in vrs:
            if self.clsvar and self.clsvar not in vrs:
                if vrs[self.value]:
                    vrs[self.clsvar] = (
                               utils.clips_instance(*(vrs[self.value])),
                               [], ['class'])
                else:
                    vrs[self.clsvar] = (self.value, [], ['class'])
            return self.ob.get_var_slot_constraint(vrs, self.value)
        else:
            vrs[self.value] = ()
            if self.clsvar in vrs:
                if vrs[self.clsvar]:
                    return '?%(var)s&:(eq (class ?%(var)s) %(cls)s)' % {
                                    'var': self.value,
                         'cls': utils.clips_instance(*(vrs[self.clsvar]))}
                else:
                    return '?%(var)s&:(eq (class ?%(var)s) %(cls)s)' % {
                                    'var': self.value,
                                    'cls': self.clsvar}
            vrs[self.clsvar] = (self.value, [], ['class'])
            return '''?%(var)s&:(or
                                 (eq (class ?%(var)s) %(cls)s)
                                 (subclassp (class ?%(var)s) %(cls)s))''' % {
                                    'var': self.value,
                                    'cls': self.cls.__name__}

    def put(self, vrs):
        return self.ob.put(vrs)

    def get_isc(self, queries, vrs, ancestor, mod_path):
        """
        get instance-set condition;
        return (instance-set templates, instance-set queries)
        """
        q = self.get_query(vrs, ancestor, mod_path)
        queries += q


class Subword(object):
    '''
    Instantiated with two subclasses of Namable,
    Or of ClassVar created with Word(var),
    it can be used as a premise in a rule.
    '''
    def __init__(self, sub, sup):
        self.sub = sub
        self.sup = sup

    def get_ce(self, vrs):
        '''
        '''
        sub = getattr(self.sub, 'clsput', self.sub.put)(vrs)
        sup = getattr(self.sup, 'clsput', self.sup.put)(vrs)
        return ''' (test (or (eq %(sub)s %(sup)s)
                             (subclassp %(sub)s %(sup)s)))
               ''' % {'sub': sub,
                      'sup': sup}


class Noun(Word):
    """
    When Thing is extended, this adds 1 defclass to clips
    creating a subclass of Namable.
    And registers the class in utils.subclasses.

    If utils.varpat matches classname,
    it sets cls to Thing so that Word.__new__
    can return the right ClassVar,
    unless bases is a Thing subclass.
    """
    def __new__(cls, classname, bases=None, newdict=None):
        if utils.varpat.match(classname) and not bases:
            cls.cls = utils.get_class('Thing')
        return Word.__new__(cls, classname, bases, newdict)

    def __init__(cls, classname, bases, newdict):
        superclassname = bases[0].__name__
        clp = '(defclass %s (is-a %s))' % (classname, bases[0].__name__)
        super(Noun, cls).__init__(classname, bases, newdict, clp=clp)

utils.register('Noun', Noun)



class Verb(Word):
    """
    When Exists is extended, this adds 1 defclass to clips
    creating a subclass of Namable.
    And registers the class in utils.subclasses.

    The new class mods dictionary is also
    initialized with the given mods
    and the mods of all its bases.

    If utils.varpat matches classname,
    it sets cls to Exists so that Word.__new__
    can return the right ClassVar,
    unless bases is a Exists subclass.
    """
    def __new__(cls, classname, bases=None, newdict=None):
        if utils.varpat.match(classname) and not bases:
            cls.cls = utils.get_class('Exists')
        return Word.__new__(cls, classname, bases, newdict)

    def __init__(cls, classname, bases, newdict):
        if classname == 'Exists':
            # due to the ordering of things in clps.py
            utils.register(classname, cls)
            return
        slots = ['(slot %s (type %s) (visibility public) (pattern-match reactive))' % (mod,
            issubclass(utils.get_class(modclass), Number) and \
                                    '?VARIABLE' or 'INSTANCE')
                  for mod,modclass in cls.mods.items()]
        slots = ' '.join(slots)
        clp = '(defclass %s (is-a %s) %s)' % (classname,
                                              bases[0].__name__,
                                              slots)
        for mod,modclass in cls.mods.items():
            if isinstance(modclass, type):
                cls.mods[mod] = modclass.__name__
        for kls in bases:
            if getattr(kls, 'mods', _m):
                cls.mods.update(kls.mods)
        super(Verb, cls).__init__(classname, bases, newdict, clp=clp)

utils.register('Verb', Verb)



class Namable(object):
    """
    abstract class ancestor of all excluding
    the metawords.
    """
    __metaclass__ = Word

    @classmethod
    def from_clips(cls, instance):
        if not isinstance(instance, clips._clips_wrap.Instance):
            instance = clips.FindInstance(instance)
        clsname = str(instance.Class.Name)
        cls = utils.get_class(clsname)
        if clsname == 'Namable':
            return cls(str(instance))
        else:
            return cls.from_clips(instance)

    def get_var_constraint(self, vrs, ancestor, mod_path, ci):
        return '&:%s' % self.get_var_query(vrs, ancestor, mod_path, ci)

    def get_var_query(self, vrs, ancestor, mod_path, ci):
        constraint = ''
        if self.value in vrs:
            if vrs[self.value]:
                v_ci = utils.clips_instance(*(vrs[self.value]))
                constraint = '(eq %s %s)' % (v_ci, ci)
            else:
                constraint = '(eq %s ?%s)' % (ci, self.value)
        elif not isinstance(self, Number):
            constraint = '(or (eq (class %(ci)s) %(cls)s) (subclassp (class %(ci)s) %(cls)s))' % {'ci': ci, 'cls': self.__class__.__name__}
        vrs[self.value] = (ancestor, mod_path)
        return constraint

    def get_var_slot_constraint(self, vrs, val):
        if self.value in vrs:
            if vrs[self.value]:
                return '?%(val)s&:(eq ?%(val)s %(var)s)' % {'val': val,
                                       'var': utils.clips_instance(*(vrs[self.value]))}
            else:
                return '?%s' % self.value
        else:
            vrs[self.value]= ()
            return class_constraint % {'val': self.value,
                                           'cls': self.__class__.__name__}

    def put_var(self, vrs):
        if self.value in vrs and vrs[self.value]:
            return utils.clips_instance(*(vrs[self.value]))
        return '?%s' % self.value


class ArithSafeTime(Namable):
    pass

class Number(Namable):
    """
    A number or an arithmetic operation.
    As value it can take a number, a str(number),
    a string containing a single arithmetic operator,
    within ['+', '-', '*', '/', '**']
    or a string with an arithmetic operation
    of the form '(<op> <arg1> <arg2>)'
    where op is one of the operators
    and arg can be a number or another operation.
    If the value is an operator,
    the args are either instances of Number,
    or strings to be converted in such instances.

    Within rules, variables can take the place of
    value, any of the args, or in the value in the position
    of numbers when it is a string representing an operation.
    """
    def __init__(self, value, arg1='', arg2=''):
        self.arg1 = str(arg1)
        self.arg2 = str(arg2)
        try:
            self.value = str(float(value))
        except ValueError:
            if value[0] == '(':
                args = utils.parens(value)
                self.value = args[0]
                self.arg1 = Number(args[1])
                self.arg2 = Number(args[2])
            else:
                self.value = value
                if self.arg1 != '':
                    self.arg1 = isinstance(arg1, Number) and arg1 or Number(arg1)
                if self.arg2 != '':
                    self.arg2 = isinstance(arg2, Number) and arg2 or Number(arg2)

    @classmethod
    def from_clips(cls, instance):
        return Number(instance)

    def _get_number(self, vrs):
        """
        """
        if utils.varpat.match(str(self.value)):
            return self.put_var(vrs)
        try:
            return str(float(self.value))
        except ValueError:
            arg1 = self.arg1 != '' and self.arg1._get_number(vrs) or ''
            arg2 = self.arg2 != '' and self.arg2._get_number(vrs) or ''
            return '(%s %s %s)' % (self.value, arg1, arg2)

    def get_slot_constraint(self, vrs):
        return self._get_number(vrs)

    def get_constraint(self, vrs, ancestor, mod_path):
        constraint = self.get_query(vrs, ancestor, mod_path)
        if not constraint:
            return ''
        return '&:%s' % '&:'.join(constraint)

    def get_query(self, vrs, ancestor, mod_path):
        ci = utils.clips_instance(ancestor, mod_path)
        if utils.varpat.match(self.value):
            constraint = self.get_var_query(vrs, ancestor, mod_path, ci)
        else:
            constraint = '(eq %s %s)' % (ci, self.get_slot_constraint(vrs))
        if constraint:
            return [constraint]
        else:
            return []

    def put(self, vrs):
        return self._get_number(vrs)

    def get_isc(self, queries, vrs, ancestor, mod_path):
        """
        get instance-set condition;
        return (instance-set templates, instance-set queries)
        """
        q = self.get_query(vrs, ancestor, mod_path)
        queries += q

    def __str__(self):
        return self._get_number({})



class Arith(Number):
    """
    Arithmetic predicate.
    It has the same basic form as number
    except that value can only be, either
    a string with an arithmetic predicate symbol
    from ['=', '<=', '>=', '!=', '<', '>'],
    or a string with a predication of the same form
    as the strings for number, but with the outermost
    operator substituted for a predicate.

    Instances can only be used as premises in rules.
    """
    def __init__(self, value, arg1='', arg2=''):
        if value[0] == '(':
            args = utils.parens(value)
            self.value = args[0]
            self.arg1 = Number(args[1])
            self.arg2 = Number(args[2])
        else:
            self.value = value
            if arg1 != '':
                self.arg1 = isinstance(arg1, Number) and arg1 or Number(arg1)
            if arg2 != '':
                self.arg2 = isinstance(arg2, Number) and arg2 or Number(arg2)

    def get_ce(self, vrs=None):
        arg1 = self.arg1.put(vrs)
        arg2 = self.arg2.put(vrs)
        return '(test (%s %s %s))' % (self.value, arg1, arg2)


class CountMixin(Namable):

    def s_get_slot_constraint(self, vrs, fun, reorder=False):
        '''
        using the function count-in-sentences defined in clips,
        find out how many uniques the instance-set method
        from self.sen returns.
        '''
        templs, queries = [], []
        for unique in self.uniques:
            vrs[unique] = ()
        for n, sentence in enumerate(self.sentences):
            sentence.get_ism(templs, queries, vrs, newvar='q%d' % n)
        if len(queries) > 1:
            q = '(and %s)' % ' '.join(queries)
        else:
            q = queries and queries[0] or 'TRUE'
        if reorder:
            rtempls = []
            for n, templ in enumerate(templs):
                if templ[0] in self.uniques:
                    rtempls = [templ] + rtempls
                else:
                    rtempls.append(templ)
            templs = [len(self.uniques)] + rtempls
        clps = '(find-all-instances (%s) %s)' % \
                (' '.join(['(?%s %s)' % templ for templ in templs]), q)
        return '(%s %s)' % (fun, clps)


class Count(CountMixin):
    """
    to be used in rules wherever a number can be used, i.e.,
    as a mod in the predicate of a fact used as condition
    or as consecuence;
    or as an argument in an arithmetic predicate.

    It is built with a sentence with any number of variables,
    and returns the number of sentences in the kb that
    match the given sentence.
    """

    def __init__(self, *sentences):
        self.sentences = sentences

    def get_slot_constraint(self, vrs):
        '''
        using the function count-sentences defined in clips,
        find out how many sentences the instance-set method
        from self.sen returns.
        '''
        return self.s_get_slot_constraint(vrs, 'count-sentences')

    def put(self, vrs):
        pass

    def get_isc(self, templs, queries, vrs):
        pass

class UniqueCount(CountMixin):
    """
    to be used in rules wherever a number can be used, i.e.,
    as a mod in the predicate of a fact used as condition
    or as consecuence;
    or as an argument in an arithmetic predicate.

    It is built with a iterable  'unique' of variable names,
    and a sentence with any number of variables,
    among which the previous ones are free,
    and returns the number of different matches
    of the iterable of variable names in sentences in the kb
    that match the given sentence.
    """

    def __init__(self, uniques, *sentences):
        self.uniques = uniques
        self.sentences = sentences

    def get_slot_constraint(self, vrs):
        '''
        using the function count-in-sentences defined in clips,
        find out how many uniques the instance-set method
        from self.sen returns.
        '''
        for unique in self.uniques:
            vrs[unique] = ()
        return self.s_get_slot_constraint(vrs, 'count-in-sentences')

    def put(self, vrs):
        pass

    def get_isc(self, templs, queries, vrs):
        pass


class MaxCount(CountMixin):
    """
    to be used in rules wherever a number can be used, i.e.,
    as a mod in the predicate of a fact used as condition
    or as consecuence;
    or as an argument in an arithmetic predicate.

    It is built with a 'unique' variable name,
    and a sentence with any number of variables,
    among which the previous one is free,
    and returns the maximum number of matches
    that a single word makes with the distinguished variable
    among the sentences that match the given sentences.

    """

    def __init__(self, uniques, *sentences):
        self.uniques = uniques
        self.sentences = sentences

    def get_slot_constraint(self, vrs):
        for unique in self.uniques:
            vrs[unique] = ()
        return self.s_get_slot_constraint(vrs, 'max-count', reorder=True)

class MinCount(CountMixin):
    """
    to be used in rules wherever a number can be used, i.e.,
    as a mod in the predicate of a fact used as condition
    or as consecuence;
    or as an argument in an arithmetic predicate.

    It is built with a 'unique' variable name,
    and a sentence with any number of variables,
    among which the previous one is free,
    and returns the minimum number of matches
    that a single word makes with the distinguished variable
    among the sentences that match the given sentences.

    """

    def __init__(self, uniques, *sentences):
        self.uniques = uniques
        self.sentences = sentences

    def get_slot_constraint(self, vrs):
        for unique in self.uniques:
            vrs[unique] = ()
        return self.s_get_slot_constraint(vrs, 'min-count', reorder=True)


class Not(Namable):
    """
    negation by failure
    """

    def __init__(self, sentence):
        self.sentence = sentence

    def get_ce(self, vrs=None):
        if vrs is None: vrs = []
        return '(not %s)' % self.sentence.get_ce(vrs)


class BiConnMixin(Namable):
    """
    Binary connective mixin
    """

    def __init__(self, *sentences):
        self.sentences = sentences

    def _get_ce(self, vrs=None, conn='and'):
        if vrs is None: vrs = []
        if len(self.sentences) > 1:
            sen = [s.get_ce(vrs) for s in self.sentences]
            return '(%s %s)' % (conn, ' '.join(sen))
        return self.sentences[0].get_ce(vrs)


class And(BiConnMixin):
    """
    Conjunction

    To be used as conditions in rules
    """

    def get_ce(self, vrs=None):
        return self._get_ce(vrs)


class Or(BiConnMixin):
    """
    Disjunction

    To be used as conditions in rules
    """

    def get_ce(self, vrs=None):
        return self._get_ce(vrs, conn='or')

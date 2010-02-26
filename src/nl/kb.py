
import clips

from metanl import Number, Arith, Noun, Word, Verb, Subword, Namable
from thing import Thing
from state import Exists
from prop import Fact
from nltime import (Time, Instant, Duration, Finish, During,
                    Coincide, MinComStart, MaxComEnd,
                    Intersection)
from rule import Rule

from log import logger
import utils



def tell(*args):
    '''
    Take any number of sentences and rules and
    add them to clips.
    '''
    for sentence in args:
        s = sentence.put_action()
        logger.info(s)
        if isinstance(sentence, Rule):
            try:
                clips.Build(s)
            except:
                logger.error(clips.ErrorStream.Read())
                raise
        else:
            clips.Eval(s)

def get_instancesn(*sentences):
    templs = []
    queries = []
    vrs = {}
    for n, sentence in enumerate(sentences):
        sentence.get_ism(templs, queries, vrs, newvar='q%d' % n)
    templs = list(set(templs))
    if len(queries) > 1:
        q = '(find-all-instances (%s) (and %s))' % \
            (' '.join(['(?%s %s)' % templ for templ in templs]),
                               ' '.join(queries))
    else:
        q = '(find-all-instances (%s) %s)' % \
                (' '.join(['(?%s %s)' % templ for templ in templs]),
                               queries and queries[0] or 'TRUE')
    return q, templs

def get_instances(*sentences):
    q, templs = get_instancesn(*sentences)
    logger.info(q)
    return clips.Eval(q), templs

def retract(sentence):
    for ins in get_instances(sentence):
        clips.FindInstance(ins).Remove()

def ask(*sentences):
    '''
    Retrieve objects from clips.
    sentences is a list of facts or things,
    and they can contain variables,
    whose scope is the set of sentences asked.
    return a list of dicts with the names of the variables
    used in the sentences asked as keys
    and the matched objects as values.
    If there are no variables in the
    question, but the asked sentences match,
    return True.
    if there is no match, return False
    '''
    clps, templs = get_instances(*sentences)
    resp = []
    if clps:
        names = [Namable.from_clips(ins) for ins in clps]
        while names:
            first = names[:len(templs)]
            names = names[len(templs):]
            rsp = {}
            for templ in templs:
                if utils.varpat.match(templ[0]) and not templ[0].startswith('Y'):
                    rsp[templ[0]] = str(first[templs.index(templ)])
            if rsp:
                resp.append(rsp)
        if not resp:
            resp = True
    else:
        resp = False
    return resp

def ask_obj(sentence):
    '''
    retrieve sentences in clips
    matching the given sentence.
    Can use variables.
    '''
    clps, templs = get_instances(sentence)
    sens = []
    if clps:
        for ins in clps:
            if isinstance(sentence, Thing):
                sens.append(Namable.from_clips(ins))
            elif isinstance(sentence, Fact):
                i = clips.FindInstance(ins)
                if issubclass(utils.get_class(str(i.Class.Name)), Fact):
                    sens.append(Fact.from_clips(ins))
    return sens

def extend():
    '''
    Run the CLIPS machine;
    build or extend the rete network.
    To be used whenever new sentences
    or rules are added to clips,
    and we want to query the system.
    '''
    acts = clips.Run()
    return acts


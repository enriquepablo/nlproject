from nl.bot.resolver import expose, subexpose, subresolve
from nl.bot import generated
import nl


@expose(r'^hi$')
def greetings():
    return 'hi'

# definitions
@expose(r'^a (?P<thing>\w+) is a (?P<superthing>\w+)$')
def subthing(thing, superthing):
    try:
        generate_thing(thing, superthing)
    except nl.exceptions.NameNotFound, e:
        return str(e)
    return 'ok, a %s is a %s' % (thing, superthing)

@expose(r'^to (?P<state>\w+) is to (?P<superstate>\w+) and can be (?P<mods>.*)$')
def substate(state, superstate, mods):
    mod_pairs = subresolve(mods)
    generate_state(state, superstate, mod_pairs)
    return 'ok, a %s is a %s' % (state, superstate)

# sentences
@expose(r'^(?P<name>\w+) is a (?P<thing>\w+)$')
def isa(name, thing):
    cls = nl.utils.subclasses[thing]
    nl.kb.tell(cls(name))
    return 'ok, %s is a %s' % (name, thing)

# questions


# subcomponents
@subexpose(r'^(mod) type (cls) (mods)$')
def get_mods(mod, cls, mods):
    pass


# generators

def generate_thing(thing, superthing):
    if not nl.utils.subclasses.has_key(superthing):
        raise nl.exceptions.NameNotFound(thing)
    cls_def = 'class %s(%s): pass\n\n' % (thing, superthing)
    generate_class(cls_def)

def generate_state(state, superstate, mods):
    pass

def generate_class(cls_def):
    from os import path
    here = path.join(path.dirname(__file__))
    f = open(here + '/generated.py', 'a+')
    f.write(cls_def)
    f.close()
    reload(generated)



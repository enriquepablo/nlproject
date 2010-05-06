from nl import *
from people import *
from modality import *
from cms3 import *


class Project(Thing):
    """
    """

class Component(Content):
    """
    A component belongs to a project,
    and can have subcomponents, features, and errors.
    It is a Content type, so it can have workflow
    """

class Milestone(Content):
    """
    A milestone has a due date,
    and can have components, features, and errors.
    It is a Content type, so it can have workflow
    """

class Open(WfAction):
    """
    A Component or Milestone can be opened to current
    """

class Close(WfAction):
    """
    A Component or Milestone can be closed to closed
    """

class Works(Exists):
    """
    A person can work on some content: Error, feature, component
    """
    subject = Person
    instantaneous = False
    mods = {'on': Content}


boss = Role('boss')
developer = Role('developer')
qaboss = Role('qaboss')
qateam = Role('qateam')

current = Status('current')
closed = Status('closed')
# duplicate = Status('duplicate')
# invalid = Status('invalid')


class Server(Thing):
    """
    A server corresponds to some service in some address,
    and has one branch installed.
    And speaks some protocol
    """

class Protocol(Thing):
    """
    Corresponds to some communication protocol
    """

class Branch(Thing):
    """
    A project can have any number of branches,
    as can be installed in one or more Servers.
    """

class Changeset(Thing):
    """
    a Changeset corresponds to a changeset :)
    """

class Merge(Exists):
    """
    a person can merge some changeset into some branch
    """
    subject = Person
    mods = {'changeset': Changeset,
            'branch': Branch}

class Error(Content):
    """
    An error can be in one or more servers,
    A changeset can affect one (or more) errors
    An error can be open or closed or accepted for merge
    when open, a developer owns it;
    when closed, a qateam;
    when accepted, the boss.
    """

class Feature(Content):
    """
    A changeset can affect some feature.
    A feature can be open or closed or accepted for merge
    """

class Affects(Exists):
    """
    something can affect something
    """
    subject = Thing
    mods = {'affected': Thing}







################################################################################
## APUNTES
##  
##  count(fact() [, distinct(*vars)]) -> number
##  
##  
##  Fact(Person('X1'), Goes( to=mecca ), Instant('I1'))
##  
##  #cuenta cuantas veces ha sido visitada la meca
##  Count(Fact(Person('X1'), Goes( to=mecca ), Instant('I1')))
##  
##  #cuenta cuantas personas han visitado la meca
##  Count(Fact(Person('X1'), Goes( to=mecca ), Instant('I1')), Distinct('X1'))
##  
##  #cuenta en cuantos momentos ha sido visitada la meca
##  Count(Fact(Person('X1'), Goes( to=mecca ), Instant('I1')), Distinct('I1'))
##  
##  #cuántas son las máximas veces que alguien ha visitado la meca
##  
##  #cuántas son las máximas personas que han visitado la meca a la vez
##  


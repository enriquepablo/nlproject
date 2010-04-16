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
    """

class Branch(Thing):
    """
    A project can have any number of branches, that represent the code,
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

class Error(Thing):
    """
    An error can be in one or more servers,
    A changeset can fix one (or more) errors
    """

class Feature(Thing):
    """
    A changeset can implement some feature.
    """

class 









################################################################################
## APUNTES
##  
##  count(fact()) -> number
##  
##  max_count(Xl, fact(X1)) -> ind
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
##  Count(Fact(Person('X1'), Goes( to=mecca ), Instant('I1')), Distinct('X1'))
##  
##  #cuántas son las máximas veces que alguien ha visitado la meca
##  
##  #cuántas son las máximas personas que han visitado la meca a la vez
##  
##  #
##  
##  
##  
##  count(fact) # número de frases con la forma fact
##  
##  max_count(x, fact) #el número de frases en las que aparece el x que más aparece en frases con la forma fact
##  
##  different_values(x, fact) # cuántos x hay que aparezcan en frases con la forma fact
##  

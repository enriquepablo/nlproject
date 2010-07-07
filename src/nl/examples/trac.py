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
    A Component can be opened to current
    """

class Close(WfAction):
    """
    A Component can be closed to closed
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
    mods = {'what': Changeset,
            'where': Branch}

class Test(Exists):
    """
    a person can merge some changeset into some branch
    """
    subject = Person
    mods = {'what': Ticket,
            'where': Server}

class Ticket(Content):
    """
    A ticket can be in one or more servers,
    A changeset can affect one (or more) tickets
    A ticket can be open or closed or accepted for merging any associated 
    It can be owned by any number of people,
    at least a developer and a qateam.
    """

class Error(Ticket):
    """
    An error can be in one or more servers,
    A changeset can affect one (or more) errors
    An error can be open or closed or accepted for merge
    It can be owned by any number of people,
    at least a developer and a qateam.
    """

class Feature(Ticket):
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


################################
# RULES
################################


# A ticket is current (in some server)
# And a server doesn't have that ticket neither current nor closed
# ->
# A qateam person with the less tickets has to review it on that server

Rule([
      Fact(Ticket('T1'), Has(what=open), Instant('I1')),
      Not(Fact(Ticket('T1'), Has(where=Server('S1')), Instant('I1'))),
      Fact(Person('P1'), Has(what=qateam), Instant('I1')),
      Arith('=', Count(Fact(Person('P1'), Owns(what=Ticket('T1')), Instant('I1')))
                 MinCount(('P2',), Fact(Person('P2'), Owns(what=Ticket('T2')), Instant('I1')))),
     ],[
      Fact(Person('P1'), Must(do=Test(what=Ticket('T1'), where=Server('S1'))), Duration(Instant('I1'), 'now')),
     ])

# A ticket is open
# and no developer owns it
# ->
# The boss has to give that ticket to some one
# (or: I ask the boss to whom do we give the ticket -> hook a notification)

Rule([
      Fact(Ticket('T1'), Has(what=current), Instant('I1')),
      Fact(Person('P3'), Has(what=boss), Instant('I1')),
      Not(And(Fact(Person('P1'), Has(what=developer), Instant('I1')),
              Fact(Person('P2'), Owns(what=Ticket('T2')), Instant('I1')))),
    ],[
      Fact(Person('P3'), Must(do=Give(what=Ticket('T1'))), Duration(Instant('I1'), 'now')),
    ])

# A server has an open ticket
# and another server has it closed
# and a changeset that affects that ticket
# has been merged to the branch in the second but not to the one in the first
# ->
# The boss has to apply it in the first
# (or: I merge the changeset an update -> hook a script to do it and notify the boss)

Rule([
      Fact(Person('P1'), Has(what=boss), Instant('I1')),
      Fact(Ticket('T1'), Has(what=open, where=Server('S1')), Instant('I1')),
      Fact(Server('S1'), Has(what=Branch('B1')), Instant('I1')),
      Fact(Ticket('T1'), Has(what=closed, where=Server('S2')), Instant('I1')),
      Fact(Server('S2'), Has(what=Branch('B2')), Instant('I1')),
      Fact(Changeset('X1'), Affects(what=Ticket('T1')), Instant('I1')),
      Fact(Person('P1'), Merge(what=Changeset('X1'), where=Branch('B2')), Instant('I2')),
      Not(Fact(Person('P1'), Merge(what=Changeset('X1'), where=Branch('B1')), Instant('I3'))),
    ],[
      Fact(Person('P1'), Must(do=Merge(what=Changeset('X1'), where=Branch('B1'))), Duration(Instant('I1'), 'now')),
    ])

# A server has a current ticket
# and another has it closed
# and the number of changesets that affect that ticket applied in each branch is the same
# ->
# a qateam person has to test that ticket in the first server

Rule([
      Fact(Ticket('T1'), Has(what=open, where=Server('S1')), Instant('I1')),
      Fact(Server('S1'), Has(what=Branch('B1')), Instant('I1')),
      Fact(Ticket('T1'), Has(what=closed, where=Server('S2')), Instant('I1')),
      Fact(Server('S2'), Has(what=Branch('B2')), Instant('I1')),
      Fact(Person('P1'), Has(what=boss), Instant('I1')),
      Arith('=', Count(Fact(Person('P1'), Merge(what=Changeset('X1'), where=Branch('B1')), Instant('I3')),
                       Fact(Changeset('X1'), Affects(what=Ticket('T1')), Instant('I3'))),
                 Count(Fact(Person('P1'), Merge(what=Changeset('X1'), where=Branch('B2')), Instant('I3')),
                       Fact(Changeset('X1'), Affects(what=Ticket('T1')), Instant('I3')))),
      Fact(Person('P2'), Has(what=qateam), Instant('I1')),
      Arith('=', Count(Fact(Person('P2'), Owns(what=Ticket('T1')), Instant('I1')))
                 MinCount(('P3'), Fact(Person('P3'), Owns(what=Ticket('T2')), Instant('I1')))),
     ],[
      Fact(Person('P2'), Must(do=Test(what=Ticket('T1'), where=Server('S1'))), Duration(Instant('I1'), 'now')),
     ])


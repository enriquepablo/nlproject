from nl import *
from people import *

class Wants(Exists):
    """
    the verb Wants has a person proper name as subject
    and can take as 'to' modifier a verb prhase
    """
    subject = Person
    mods = {'to': Exists}

class Can(Exists):
    """
    the verb Can has a person proper name as subject
    and can take as 'what' modifier a verb prhase
    """
    subject = Person
    instantaneous = False
    mods = {'what': Exists}


class Must(Exists):
    """
    the verb Can has a person proper name as subject
    and can take as 'what' modifier a verb prhase
    """
    subject = Person
    instantaneous = False
    mods = {'what': Exists}





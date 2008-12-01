from nl import Thing, State, Number


class Hombre(Thing): pass

class Ir(State): mods = {'hacia': Thing}

class Quiere(State): mods = {'que': State}

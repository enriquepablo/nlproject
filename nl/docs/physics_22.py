from nl import Thing, State, Number

class Cuerpo(Thing): pass

class TieneMasa(State):
    mods = {'kgs': Number}

class TienePosicion(State):
    mods = {'metros': Number}

class EstaADistancia(State):
    mods = {'metros': Number, 'otro': Cuerpo}

class TieneVelocidad(State):
    mods = {'mps': Number}

class Anda(TieneVelocidad):
    mods = {'longitud_zancada': Number}

class TieneAceleracion(State):
    mods = {'mps2': Number}

class SufreFuerza(State):
    mods = {'newton': Number}

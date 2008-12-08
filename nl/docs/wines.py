from nl import Thing, State, Number

class Winery(Thing): pass
class Region(Thing): pass
class ConsumableThing(Thing): pass
class PotableLiquid(ConsumableThing): pass
class EdibleThing(ConsumableThing): pass
class Wine(PotableLiquid): pass
class Pasta(EdibleThing): pass

class Grape(EdibleThing): pass
class WineGrape(Grape): pass

class madeFromGrape(State):
      subject = Wine
      mods = {'from': WineGrape}

class WineDescriptor(Thing): pass
class WineColor(WineDescriptor): pass

class HasWineDescriptor(State):
      subject = Wine
      mods = {'descriptor': WineDescriptor}

class HasWineColor(HasWineDescriptor):
      subject = Wine
      mods = {'descriptor': WineColor}

class HasMaker(State):
      subject = Wine
      mods = {'maker': Winery}

class LocatedIn(State):
      subject = Thing
      mods = {'region': Region}

class Moves(State):
      subject = Thing
      mods = {'frm': Region,
              'to': Region}

class Vintage(Thing): pass

class VintageOf(State):
      subject = Vintage
      mods = {'of': Wine}

class VintageYear(Thing): pass

class YearValue(State):
      subject = VintageYear
      mods = {'value': Number}

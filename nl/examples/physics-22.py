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

import Gnuplot
from nl import kb, State, Thing, Number, Arith, Prop, Rule


time = '10000'

# names

class Cuerpo(Thing): pass


# verbs

class TieneMasa(State):
    mods = {'kgs': Number}



class TienePosicion(State):
    mods = {'x': Number,
            'y': Number}



class EstaADistancia(State):
    mods = {'mts': Number,
            'otro': Cuerpo}



class TieneVelocidad(State):
    mods = {'x': Number,
            'y': Number}



class TieneAceleracion(State):
    mods = {'x': Number,
            'y': Number}



class SufreFuerza(State):
    mods = {'x': Number,
            'y': Number}



# rules


r1 = Rule('r1', [
           Prop(Cuerpo('X1'), TienePosicion(x='X2', y='X3'), 'X4'),
           Prop(Cuerpo('X1'), TieneVelocidad(x='X5', y='X6'), 'X4'),
           Arith('(< X4 %s)' % time)
           ], [
           Prop(Cuerpo('X1'), TienePosicion(x='(+ X2 X5)', y='(+ X3 X6)'), '(+ X4 1)')
           ])


r2 = Rule('r2', [
           Prop(Cuerpo('X1'), TieneVelocidad(x='X2', y='X3'), 'X4'),
           Prop(Cuerpo('X1'), TieneAceleracion(x='X5', y='X6'), 'X4'),
           Arith('(< X4 %s)' % time)
           ], [
           Prop(Cuerpo('X1'), TieneVelocidad(x='(+ X2 X5)', y='(+ X3 X6)'), '(+ X4 1)')
           ])


r3 = Rule('r3', [
           Prop(Cuerpo('X1'), SufreFuerza(x='X2', y='X3'), 'X4'),
           Prop(Cuerpo('X1'), TieneMasa(kgs='X5'), 'X4'),
           Arith('(< X4 %s)' % time)
           ], [
           Prop(Cuerpo('X1'), TieneAceleracion(x='(/ X2 X5)', y='(/ X3 X5)'), '(+ X4 1)')
           ])

r4 = Rule('r4', [
           Prop(Cuerpo('X1'), TienePosicion(x='X2', y='X3'), 'X4'),
           Prop(Cuerpo('X1'), TieneMasa(kgs='X5'), 'X4'),
           Prop(Cuerpo('X6'), TienePosicion(x='X7', y='X8'), 'X4'),
           Prop(Cuerpo('X6'), TieneMasa(kgs='X9'), 'X4'),
           Arith('(< X4 %s)' % time),
           Arith('(neq X1 X6)')
           ], [
           Prop(Cuerpo('X1'), SufreFuerza(
              x='(- 0 (/ (* (* X5 X9) (- X2 X7)) (** (+ (** (- X2 X7) 2) (** (- X3 X8) 2)) (/ 3 2))))',
              y='(- 0 (/ (* (* X5 X9) (- X3 X8)) (** (+ (** (- X2 X7) 2) (** (- X3 X8) 2)) (/ 3 2))))'),
              '(+ X4 1)')])

r5 = Rule('r5', [
           Prop(Cuerpo('X1'), TieneMasa(kgs='X2'), 'X3'),
           Arith('(< X3 %s)' % time)
           ],
           [
           Prop(Cuerpo('X1'), TieneMasa(kgs='X2'), '(+ X3 1)')
           ])



# things

c1 = Cuerpo('c1')

c2 = Cuerpo('c2')

# propositions

p1 = Prop(c1, TieneMasa(kgs=750), 1)

p2 = Prop(c2, TieneMasa(kgs=750), 1)

p3 = Prop(c1, TienePosicion(x=0, y=0), 1)

p4 = Prop(c2, TienePosicion(x=0, y=100), 1)

p5 = Prop(c1, TieneVelocidad(x=-2, y=0), 1)

p6 = Prop(c2, TieneVelocidad(x=2, y=0), 1)

p7 = Prop(c1, TieneAceleracion(x=0, y=0), 1)

p8 = Prop(c2, TieneAceleracion(x=0, y=0), 1)

p9 = Prop(c1, SufreFuerza(x=0, y=0), 1)

p10 = Prop(c2, SufreFuerza(x=0, y=0), 1)

for p in (c1, c2, p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, r1, r2, r3, r4, r5):
    kb.tell(p)

kb.extend()


resp1 = kb.ask_objs(Prop(c1, TienePosicion(x='X1', y='X2'), 'X3'))
resp2 = kb.ask_objs(Prop(c2, TienePosicion(x='X1', y='X2'), 'X3'))

#resp1 = kb.ask_objs(Prop(c1, SufreFuerza(newton='X1'), 'X2'))
#resp2 = kb.ask_objs(Prop(c2, SufreFuerza(newton='X1'), 'X2'))


line1 = [(float(p.predicate.x.value), float(p.predicate.y.value)) for p in resp1]
line2 = [(float(p.predicate.x.value), float(p.predicate.y.value)) for p in resp2]


gp = Gnuplot.Gnuplot(persist = 1)

gp('set data style lines')

plot1 = Gnuplot.PlotItems.Data(line1, with="dots lw 2 lc rgb 'red'",
      title='c1 con %s kgs, desde (%s, %s) a (%s, %s)' % (p1.predicate.kgs.value,
                                                     p3.predicate.x.value,
                                                     p3.predicate.y.value,
                                                     p5.predicate.x.value,
                                                     p5.predicate.y.value))
plot2 = Gnuplot.PlotItems.Data(line2, with="points pt 6 lw 1 lc rgb 'blue'",
      title='c2 con %s kgs, desde (%s, %s) a (%s, %s)' % (p2.predicate.kgs.value,
                                                     p4.predicate.x.value,
                                                     p4.predicate.y.value,
                                                     p6.predicate.x.value,
                                                     p6.predicate.y.value))

gp.plot(plot1, plot2)

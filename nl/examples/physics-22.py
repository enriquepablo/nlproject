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


time = '1000'

# names

class Body(Thing): pass


# verbs

class HasMass(State):
    mods = {'kgs': Number}



class HasPosition(State):
    mods = {'x': Number,
            'y': Number}



class EstaADistancia(State):
    mods = {'mts': Number,
            'otro': Body}



class HasSpeed(State):
    mods = {'x': Number,
            'y': Number}



class HasAcceleration(State):
    mods = {'x': Number,
            'y': Number}



class IsForced(State):
    mods = {'x': Number,
            'y': Number}



# rules


r1 = Rule('r1', [
           Prop(Body('X1'), HasPosition(x='X2', y='X3'), 'X4'),
           Prop(Body('X1'), HasSpeed(x='X5', y='X6'), 'X4'),
           Arith('(< X4 %s)' % time)
           ], [
           Prop(Body('X1'), HasPosition(x='(+ X2 X5)', y='(+ X3 X6)'), '(+ X4 1)')
           ])


r2 = Rule('r2', [
           Prop(Body('X1'), HasSpeed(x='X2', y='X3'), 'X4'),
           Prop(Body('X1'), HasAcceleration(x='X5', y='X6'), 'X4'),
           Arith('(< X4 %s)' % time)
           ], [
           Prop(Body('X1'), HasSpeed(x='(+ X2 X5)', y='(+ X3 X6)'), '(+ X4 1)')
           ])


r3 = Rule('r3', [
           Prop(Body('X1'), IsForced(x='X2', y='X3'), 'X4'),
           Prop(Body('X1'), HasMass(kgs='X5'), 'X4'),
           Arith('(< X4 %s)' % time)
           ], [
           Prop(Body('X1'), HasAcceleration(x='(/ X2 X5)', y='(/ X3 X5)'), '(+ X4 1)')
           ])

r4 = Rule('r4', [
           Prop(Body('X1'), HasPosition(x='X2', y='X3'), 'X4'),
           Prop(Body('X1'), HasMass(kgs='X5'), 'X4'),
           Prop(Body('X6'), HasPosition(x='X7', y='X8'), 'X4'),
           Prop(Body('X6'), HasMass(kgs='X9'), 'X4'),
           Arith('(< X4 %s)' % time),
           Arith('(neq X1 X6)')
           ], [
           Prop(Body('X1'), IsForced(
              x='(- 0 (/ (* (* X5 X9) (- X2 X7)) (** (+ (** (- X2 X7) 2) (** (- X3 X8) 2)) (/ 3 2))))',
              y='(- 0 (/ (* (* X5 X9) (- X3 X8)) (** (+ (** (- X2 X7) 2) (** (- X3 X8) 2)) (/ 3 2))))'),
              '(+ X4 1)')])

r5 = Rule('r5', [
           Prop(Body('X1'), HasMass(kgs='X2'), 'X3'),
           Arith('(< X3 %s)' % time)
           ],
           [
           Prop(Body('X1'), HasMass(kgs='X2'), '(+ X3 1)')
           ])



# things

c1 = Body('c1')

c2 = Body('c2')

# propositions

p1 = Prop(c1, HasMass(kgs=750), 1)

p2 = Prop(c2, HasMass(kgs=750), 1)

p3 = Prop(c1, HasPosition(x=0, y=0), 1)

p4 = Prop(c2, HasPosition(x=0, y=100), 1)

p5 = Prop(c1, HasSpeed(x=-2, y=0), 1)

p6 = Prop(c2, HasSpeed(x=2, y=0), 1)

p7 = Prop(c1, HasAcceleration(x=0, y=0), 1)

p8 = Prop(c2, HasAcceleration(x=0, y=0), 1)

p9 = Prop(c1, IsForced(x=0, y=0), 1)

p10 = Prop(c2, IsForced(x=0, y=0), 1)

kb.open()

for p in (c1, c2, p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, r1, r2, r3, r4, r5):
    kb.tell(p)

kb.extend()


resp1 = kb.ask_objs(Prop(c1, HasPosition(x='X1', y='X2'), 'X3'))
resp2 = kb.ask_objs(Prop(c2, HasPosition(x='X1', y='X2'), 'X3'))

#resp1 = kb.ask_objs(Prop(c1, IsForced(newton='X1'), 'X2'))
#resp2 = kb.ask_objs(Prop(c2, IsForced(newton='X1'), 'X2'))

kb.close()


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

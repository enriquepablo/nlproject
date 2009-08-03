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

from nl import kb, State, Thing, Prop, Rule, Remove, Equals

# BASIC STUFF

class Person(Thing): pass

class Can(State):
    subject = Thing
    mods = {'what': State}

class Wants(State):
    subject = Person
    mods = {'to': State}

r1 = Rule('r1', [
        Prop(Person('X1'), Wants(to=State('X4'))), # XXX only State can be a var
        Prop(Person('X1'), Can(what=State('X4')))
        ],[
        Prop(Person('X1'), State('X4'))])

class Has(State):
    subject = Person
    mods = {'what': Thing}

class IsNeeded(State):
    subject = Thing
    mods = {'for_action': State}

r2 = Rule('r2', [
        Prop(Thing('X2'), IsNeeded(for_action=State('X4'))),
        Prop(Thing('X1'), Has(what=Thing('X2')))
        ],[
        Prop(Thing('X1'), Can(what=State('X4')))])

class IsIn(State):
    subject = Thing
    mods = {'what': Thing}

r3 = Rule('r3', [
        Prop(Thing('X1'), IsIn(what=Thing('X2'))),
        Prop(Thing('X2'), IsIn(what=Thing('X3')))
        ],[
        Prop(Thing('X1'), IsIn(what=Thing('X3')))])

# CONTENT MANAGEMENT

class Group(Person): pass

class Permission(Thing): pass

r4 = Rule('r4', [
        Prop(Person('X1'), IsIn(group=Group('X2'))),
        Prop(Group('X2'), Has(what=Permission('X4')))
        ],[
        Prop(Person('X1'), Has(what=Permission('X4')))])

class Role(Person): pass

r5 = Rule('r5', [
        Prop(Person('X1'), Has(what=Role('X2'))),
        Prop(Role('X2'), Has(what=Permission('X4')))
        ],[
        Prop(Person('X1'), Has(what=Permission('X4')))])

admin = Person('admin')

member = Role('member')
manager = Role('manager')

r6 = Rule('r6', [
        Person('X1')
        ],[
        Prop(Person('X1'), Has(what=member))])

r9 = Rule('r9', [
        Permission('X2'),
        ],[
        Prop(manager, Has(what=Permission('X2'))),])

p1 = Prop(admin, Has(what=manager))

basic_perm = Permission('basic_perm')
manage_perm = Permission('manage_perm')

p2 = Prop(member, Has(what=basic_perm))

class Content(Thing): pass

class Create(State):
    subject = Person
    mods = {'what': Thing}

class IsOwner(State):
    subject = Person
    mods = {'of': Content}

create_perm = Permission('create_content')

r10 = Rule('r10', [
        Prop(Person('X1'), Wants(to=Create(what=Thing('X33')))),
        Prop(Person('X1'), Has(what=create_perm))
        ],[
        Prop(Person('X1'), Create(what=Thing('X33')))])

class Status(Thing): pass

private = Status('private')
public = Status('public')

r7 = Rule('r7', [
        Prop(Person('X1'), Create(what=Content('X2'))),
        ],[
        Content('X2'),
        Prop(Person('X1'), IsOwner(of=Content('X2'))),
        Prop(Content('X2'), Has(what=private))])

class View(State):
    subject = Person
    mods = {'what': Thing}

r12 = Rule('r12', [
        Prop(Content('X1'), Has(what=public))
        ],[
        Prop(basic_perm, IsNeeded(for_action=View(what=Content('X1'))))])

r13 = Rule('r13', [
        Prop(Content('X1'), Has(what=private))
        ],[
        Prop(manage_perm, IsNeeded(for_action=View(what=Content('X1'))))])

r14 = Rule('r14', [
        Prop(Content('X1'), Has(what=private)),
        Prop(Person('X2'), IsOwner(of=Content('X1')))
        ],[
        Prop(Person('X2'), Can(what=View(what=Content('X1'))))])

class Publish(State):
    subject = Person
    mods = {'what': Content,}

r15 = Rule('r15', [
        Prop(Person('X1'), Publish(what=Content('X2'))),
        Prop(Content('X2'), Has(what=Status('X4'))),
        Equals(False, Status('X4'), public)
        ],[
        Remove(Prop(Content('X2'), Has(what=Status('X4')))),
        Prop(Content('X2'), Has(what=public))])

r16 = Rule('r16', [
        Content('X1')
        ],[
        Prop(manage_perm, IsNeeded(for_action=Publish(what=Content('X1'))))])

class Hide(State):
    subject = Person
    mods = {'what': Content,}

r17 = Rule('r17', [
        Prop(Person('X1'), Hide(what=Content('X2')))
        ],[
        Remove(Prop(Content('X2'), Has(what=Status('X3')))),
        Prop(Content('X2'), Has(what=private))])

r18 = Rule('r18', [
        Prop(Person('X1'), IsOwner(of=Content('X2')))
        ],[
        Prop(Person('X1'), Can(what=Hide(what=Content('X2'))))])


kb.tell(admin, member, manager, basic_perm, manage_perm, create_perm, public, private,
       p1, p2, r10, r1, r2, r3, r4, r5, r6, r7, r9, r12, r13, r14, r15, r16, r17, r18)



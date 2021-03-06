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


from nl import kb, Exists, Thing, Fact, Rule, Instant, Duration, During, Finish, Coincide, MinComStart, MaxComEnd

# BASIC STUFF

# a person is a thing:
class Person(Thing): pass

# Can is a verb that takes a thing as a subject and a state as a modificator
class Can(Exists):
    subject = Thing
    mods = {'what': Exists}

# Wants is a verb that takes a person as a subject and a state as a modificator
class Wants(Exists):
    subject = Person
    mods = {'to': Exists}

# if someone wants to do something, and can do it, she does it
r1 = Rule([
        Fact(Person('X1'), Wants(to=Exists('X4')), Instant('X2')), # XXX only Exists can be a var
        Fact(Person('X1'), Can(what=Exists('X4')), Duration('X3')),
        During(Instant('X2'), Duration('X3'))
        ],[
        Fact(Person('X1'), 'X4', Instant('X2'))])

# Has is a verb that takes a person as a subject and a thing as a modificator
class Has(Exists):
    subject = Thing
    mods = {'what': Thing}

# IsNeeded is a verb that takes a Thing as a subject and a state as a modificator
class IsNeeded(Exists):
    subject = Thing
    mods = {'for_action': Exists}

# If something is needed for some state, and something else has it, that something else can be in that state
r2 = Rule([
        Fact(Thing('X2'), IsNeeded(for_action=Exists('X4')), Duration('X3')),
        Fact(Person('X1'), Has(what=Thing('X2')), Duration('X5')),
        Coincide(Duration('X3'), Duration('X5'))
        ],[
        Fact(Thing('X1'), Can(what=Exists('X4')), Duration(start=MinComStart('X3', 'X5'), end=MaxComEnd('X3', 'X5')))])

# IsIn is a verb that takes a Thing as a subject and a Thing as a modificator
class IsIn(Exists):
    subject = Thing
    mods = {'what': Thing}

# if a thing is in another thing, and that another thing is in yet another, the first is in the third as well
r3 = Rule([
        Fact(Thing('X1'), IsIn(what=Thing('X2')), Duration('X4')),
        Fact(Thing('X2'), IsIn(what=Thing('X3')), Duration('X5')),
        Coincide(Duration('X4'), Duration('X5'))
        ],[
        Fact(Thing('X1'), IsIn(what=Thing('X3')), Duration(start=MinComStart('X4', 'X5'), end=MaxComEnd('X4', 'X5')))])

# CONTENT MANAGEMENT

# A group is a person
class Group(Thing): pass

# a permission is a thing
class Permission(Thing): pass

# If a person is in a group, and that group has some permission, the person also has it
r4 = Rule([
        Fact(Person('X1'), IsIn(what=Group('X2')), Duration('X3')),
        Fact(Group('X2'), Has(what=Permission('X4')), Duration('X5')),
        Coincide(Duration('X3'), Duration('X5'))
        ],[
        Fact(Person('X1'), Has(what=Permission('X4')), Duration(start=MinComStart('X3', 'X5'), end=MaxComEnd('X3', 'X5')))])

# a role s a person
class Role(Thing): pass

# If a person has a role, and that role has some permission, the person also has it
r5 = Rule([
        Fact(Person('X1'), Has(what=Role('X2')), Duration('X3')),
        Fact(Role('X2'), Has(what=Permission('X4')), Duration('X5')),
        Coincide(Duration('X3'), Duration('X5'))
        ],[
        Fact(Person('X1'), Has(what=Permission('X4')), Duration(start=MinComStart('X3', 'X5'), end=MaxComEnd('X3', 'X5')))])

# admin is a person
admin = Person('admin')

# member is a role
member = Role('member')
# manager is a role
manager = Role('manager')

# everyperson has role member
r6 = Rule([
        Person('X1')
        ],[
        Fact(Person('X1'), Has(what=member), Duration(start=Instant('now')))])

# the manager role has every permission
r9 = Rule([
        Permission('X2'),
        ],[
        Fact(manager, Has(what=Permission('X2')), Duration(start=Instant('now'))),])

# admin has role manager
p1 = Fact(admin, Has(what=manager), Duration(start=Instant('now')))

# basic_perm is a permission
basic_perm = Permission('basic_perm')
# manage_perm is a permission
manage_perm = Permission('manage_perm')

# the member role has the basic_perm
p2 = Fact(member, Has(what=basic_perm), Duration(start=Instant('now')))

# a content is a thing
class Content(Thing): pass

# Create is a verb that takes a Person as subject and a thing as modificator
class Create(Exists):
    subject = Person
    mods = {'what': Thing}

# IsOwner is a verb that takes a person as subject and a content as modificator
class IsOwner(Exists):
    subject = Person
    mods = {'of': Content}

# create_perm  is a permission
create_perm = Permission('create_perm')

# if a person wants to create something, and has create_perm, he creates it
r10 = Rule([
        Fact(Person('X1'), Wants(to=Create(what=Thing('X33'))), Instant('X2')),
        Fact(Person('X1'), Has(what=create_perm), Duration('X3')),
        During(Instant('X2'), Duration('X3'))
        ],[
        Fact(Person('X1'), Create(what=Thing('X33')), Instant('X2'))])

# a status is a thing
class Status(Thing): pass

# private is a status
private = Status('private')
# public is a status
public = Status('public')

# if a person creates some content, the content is private and that person is its owner.
r7 = Rule([
        Fact(Person('X1'), Create(what=Content('X2')), Instant('X3')),
        ],[
        Content('X2'),
        Fact(Person('X1'), IsOwner(of=Content('X2')), Duration(start=Instant('X3'))),
        Fact(Content('X2'), Has(what=private), Duration(start=Instant('X3')))])

# View is a verb that takes a person as subject and a thing as modificator.
class View(Exists):
    subject = Person
    mods = {'what': Thing}

# if some content is public, the basic_perm is needed to view it
r12 = Rule([
        Fact(Content('X1'), Has(what=public), Duration('X2'))
        ],[
        Fact(basic_perm, IsNeeded(for_action=View(what=Content('X1'))), Duration('X2'))])

# if some content is private, the manage_perm is needed to view it
r13 = Rule([
        Fact(Content('X1'), Has(what=private), Duration('X2'))
        ],[
        Fact(manage_perm, IsNeeded(for_action=View(what=Content('X1'))), Duration('X2'))])

# if someone is owner of some content that is private, she can view it
r14 = Rule([
        Fact(Content('X1'), Has(what=private), Duration('X3')),
        Fact(Person('X2'), IsOwner(of=Content('X1')), Duration('X4')),
        Coincide(Duration('X3'), Duration('X4'))
        ],[
        Fact(Person('X2'), Can(what=View(what=Content('X1'))), Duration(start=MinComStart('X3', 'X4'), end=MaxComEnd('X3', 'X4')))])

# Publish is a verb that takes a person as subject and some content as modificator
class Publish(Exists):
    subject = Person
    mods = {'what': Content,}

# If someone publishes some content, it stops having any previous state and has public XXX ¿y si X5 ya termino?
r15 = Rule([
        Fact(Person('X1'), Publish(what=Content('X2')), Instant('X3')),
        Fact(Content('X2'), Has(what=private), Duration('X5')),
        During(Instant('X3'), Duration('X5'))
        ],[
        Finish('X5', 'X3'),
        Fact(Content('X2'), Has(what=public), Duration(start=Instant('X3')))])

# manage_perm is needed to publish anything
r16 = Rule([
        Fact(Content('X1'), Has(what=private), Duration('X2'))
        ],[
        Fact(manage_perm, IsNeeded(for_action=Publish(what=Content('X1'))), Duration('X2'))])

# Hide is a verb that takes a person as subject and a content as object
class Hide(Exists):
    subject = Person
    mods = {'what': Content,}

# If someone hides some content, it stops having any previous state and has private
r17 = Rule([
        Fact(Person('X1'), Hide(what=Content('X2')), Instant('X3')),
        Fact(Content('X2'), Has(what=public), Duration('X5')),
        During(Instant('X3'), Duration('X5'))
        ],[
        Finish('X5', 'X3'),
        Fact(Content('X2'), Has(what=private), Duration(start=Instant('X3')))])

# if a person is the owner of some content, she can hide it
r18 = Rule([
        Fact(Person('X1'), IsOwner(of=Content('X2')), Duration('X3'))
        ],[
        Fact(Person('X1'), Can(what=Hide(what=Content('X2'))), Duration('X3'))])

# manage_perm is needed to hide anything
r19 = Rule([
        Fact(Content('X1'), Has(what=public), Duration('X2'))
        ],[
        Fact(manage_perm, IsNeeded(for_action=Hide(what=Content('X1'))), Duration('X2'))])


# enter everything into the database
kb.tell(admin, member, manager, basic_perm, manage_perm, create_perm, public, private,
       p1, p2, r10, r1, r2, r3, r4, r5, r6, r7, r9, r12, r13, r14, r15, r16, r17, r18, r19)



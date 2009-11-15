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

from nl import (Noun, Thing, State, Fact, Rule, Duration, Instant, During, Coincide,
                Intersection, Finish, MinComStart, MaxComEnd, kb)

class Person(Thing):
    """
    the name person
    """

admin = Person('admin')
kb.tell(admin)

anonymous = Person('anonymous')
kb.tell(anonymous)

class Wants(State):
    """
    the verb Wants has a person proper name as subject
    and can take as 'to' modifier a verb prhase
    """
    subject = Person
    mods = {'to': State}

class Can(State):
    """
    the verb Can has a person proper name as subject
    and can take as 'what' modifier a verb prhase
    """
    subject = Person
    mods = {'what': State}

class Role(Thing):
    """
    The name role, for roles that can be had in contexts by people,
    and have some associated permissions
    """

member = Role('member')
kb.tell(member)

editor = Role('editor')
kb.tell(editor)

manager = Role('manager')
kb.tell(manager)

class Context(Thing):
    """
    content can be located in some context
    """

basic_context = Context('basic_context')
kb.tell(basic_context)

class Has(State):
    """
    a thing can have other things in a certain context
    """
    subject = Thing
    mods = {'what': Thing,
            'where': Context}

# admin is a manager in the basic context from now on
kb.tell( Fact(admin, Has(what=manager, where=basic_context), Duration(start='now')) )

class Permission(Thing):
    """
    a permission that can protect some action on some context
    """

view_perm = Permission('view_perm')
kb.tell(view_perm)

edit_perm = Permission('edit_perm')
kb.tell(edit_perm)

manage_perm = Permission('manage_perm')
kb.tell(manage_perm)

def p_role_has_perm(role, perm):
    """
    Role role has permission perm from now on
    """
    kb.tell( Fact(role, Has(what=perm), Duration(start='now')) )

p_role_has_perm(member, view_perm)

p_role_has_perm(editor, view_perm)

p_role_has_perm(editor, edit_perm)

p_role_has_perm(manager, view_perm)

p_role_has_perm(manager, edit_perm)

p_role_has_perm(manager, manage_perm)

class Content(Thing):
    """
    a content object
    """

class Located(State):
    """
    a thing can be located in some context
    """
    subject = Thing
    mods = {'where': Context}

class Status(Thing):
    """
    content objects have a status
    """

public = Status('public')
kb.tell(public)

private = Status('private')
kb.tell(private)

class Action(State):
    """
    an abstract action over a content
    """
    subject = Person
    mods = {'what': Content}

class View(Action):
    """
    a person can view some content
    """

class Edit(Action):
    """
    a person can edit some content
    """

class WfAction(Action):
    """
    abstract workflow action on some content
    """

class Publish(WfAction):
    """
    a person can publish some content
    """

class Hide(WfAction):
    """
    a person can hide some content
    """

class Workflow(Thing):
    """
    a content type can have a workflow
    """

def r_permission(action, status, perm):
    """
    If a person wants to perform the given action on a content object,
    and that object is in some context and has the given status,
    and the person has the given perm on that context,
    all at the same time,
    the person performs the given action
    """
    kb.tell( Rule([
        Fact(Person('P1'), Wants(to=action(what=Content('C1'))), Instant('I1')),
        Fact(Content('C1'), Has(what=status), Duration('T1')),
        Fact(Content('C1'), Located(where=Context('X1')), Duration('T2')),
        Fact(Person('P1'), Has(what=Role('R1'), where=Context('X1')), Duration('T3')),
        Fact(Role('R1'), Has(what=perm), Duration('T4')),
        During('I1', 'T1','T2','T3','T4')
    ],[
        Fact(Person('P1'), action(what=Content('C1')), Instant('I1'))]))

r_permission(View, public, view_perm)

r_permission(Edit, public, edit_perm)

r_permission(Hide, public, manage_perm)

r_permission(View, private, manage_perm)

r_permission(Edit, private, manage_perm)

r_permission(Publish, private, manage_perm)

#def r_transition(action, workflow, initial, final):
#    """
#    If a person performs a workflow action on a content object,
#    and that object has the intitial status up till that moment,
#    from now on it has status final
#    """
#    kb.tell( Rule([
#        Fact(Person('P1'), action(what=Content('C1')), Instant('I1')),
#        Fact(Content('C1'), Has(what=initial), Duration('T1')),
#        Fact(Content('C1'), Has(what=workflow), Duration('T2')),
#        During('I1', 'T1','T2')
#    ],[
#        Fact(Content('C1'), Has(what=final), Duration(start=Instant('I1'), end=MaxComEnd('T1', 'T2'))),
#        Finish('T1', 'I1')]))
#
def r_workflow_for_content(content_type, workflow, context):
    """
    assign workflow to content_type
    """
    kb.tell( Fact(workflow, AssignedTo(noun=content_type, where=context), Duration(start=Instant('now'))))



class AssignedTo(State):
    """
    an abstract action over a content
    """
    subject = Workflow
    mods = {'noun': Noun,
            'where': Context}

def r_transition(action, workflow, content_type, initial, final):
    """
    If a person performs a workflow action on a content object,
    and that object has the intitial status up till that moment,
    and that workflow is assigned to the type of the object in the context in which it is,
    from now on it has status final
    """
    kb.tell( Rule([
        Fact(Person('P1'), action(what=content_type('C1')), Instant('I1')),
        Fact(content_type('C1'), Located(where=Context('X1')), Duration('T1')),
        Fact(workflow, AssignedTo(noun=content_type, where=Context('X1')), Duration('T2')),
        Fact(content_type('C1'), Has(what=initial), Duration('T3')),
        During('I1', 'T1','T2', 'T3')
    ],[
        Fact(content_type('C1'), Has(what=final), Duration(start=Instant('I1'), end=MaxComEnd('T1', 'T2'))),
        Finish('T3', 'I1')]))


class Document(Content):
    """
    a document
    """

doc_workflow = Workflow('doc_workflow')
kb.tell(doc_workflow)

r_workflow_for_content(Document, doc_workflow, basic_context)

r_transition(Publish, doc_workflow, Document, private, public)

r_transition(Hide, doc_workflow, Document, public, private)

class Owns(State):
    """
    a person can own some content
    """
    subject = Person
    mods = {'what': Content}

def r_owner_can_action(action):
    """
    The owner of a content can perform the given action on the content
    """
    kb.tell( Rule([
        Fact(Person('P1'), Wants(to=action(what=Content('C1'))), Instant('I1')),
        Fact(Person('P1'), Owns(what=Content('C1')), Duration('T1')),
        During('I1','T1')
    ],[
        Fact(Person('P1'), action(what=Content('C1')), Instant('I1'))]))

r_owner_can_action(View)

r_owner_can_action(Edit)

r_owner_can_action(Hide)


class Give(State):
    """
    a person can give some content to someone else
    """
    subject = Person
    mods = {'what': Content,
            'whom': Person}

# if someone wants to give some content to someone else, and owns the content,
# then he gives it to her
kb.tell(Rule([
        Fact(Person('P1'), Wants(to=Give(what=Content('C1'), whom=Person('P2'))), Instant('I1')),
        Fact(Person('P1'), Owns(what=Content('C1')), Duration('T1')),
        During('I1', 'T1')
    ],[
        Fact(Person('P1'), Give(what=Content('C1'), whom=Person('P2')), Instant('I1'))]))

# If someone gives some content to someone else, and owns it,
# then the other owns it from then on
kb.tell(Rule([
        Fact(Person('P1'), Give(what=Content('C1'), whom=Person('P2')), Instant('I1')),
        Fact(Person('P3'), Owns(what=Content('C1')), Duration('T1')),
        During('I1', 'T1')
    ],[
        Fact(Person('P2'), Owns(what=Content('C1')), Duration(start=Instant('I1'))),
        Finish('T1', 'I1')]))

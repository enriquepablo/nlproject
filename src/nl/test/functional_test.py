import nl
from nl.log import logger

def reset():
    logger.info('\n\n\n---------RESET-----------\n\n\n')
    nl.clps.clips.Reset()
    nl.clps.clips.Clear()
    reload(nl.clps.clips)
    reload(nl.clps)
    reload(nl.thing)
    reload(nl.state)
    reload(nl.prop)
    reload(nl.arith)
    reload(nl.time)
    reload(nl.rule)
    reload(nl.kb)
    reload(nl)


class cms_test(object):
    def setup(self):
        from nl.examples import cms2
        self.cms = cms2

    def teardown(self):
        reset()
        del self.cms

#    def thing_test(self):
#        pooh = self.cms.Person('pooh')
#        assert repr(pooh) == 'pooh is a Person'
#        assert str(pooh) == 'pooh'


    def third_test(self):
        #from nl.examples import cms2
        #self.cms = cms2
        # john is a person
        john = self.cms.Person('john')
        # pete is a person
        pete = self.cms.Person('pete')
        # jane is a person
        jane = self.cms.Person('jane')
        # c1 is a content
        c1 = self.cms.Content('c1')
        # c2 is a content
        c2 = self.cms.Content('c2')
        # john has role manager
        p1 = nl.Fact(john, self.cms.Has(what=self.cms.manager), self.cms.Duration(start=self.cms.Instant('now')))
        # jane has create_perm
        p2 = nl.Fact(jane, self.cms.Has(what=self.cms.create_perm), self.cms.Duration(start=self.cms.Instant('now')))
        # jane wants to create c1
        p3 = nl.Fact(jane, self.cms.Wants(to=self.cms.Create(what=c1)), self.cms.Instant('now'))
        # pete wants to create c2
        p4 = nl.Fact(pete, self.cms.Wants(to=self.cms.Create(what=c2)), self.cms.Instant('now'))
        # input everything into the db
        nl.kb.tell(john, pete, jane, c1, c2, p1, p2, p3, p4)
        # to the question is jane owner of c1?, the answer is no
        assert not nl.kb.ask(nl.Fact(jane, self.cms.IsOwner(of=c1), self.cms.Duration(start=self.cms.Instant('now'))))
        # extend the db
        nl.kb.extend()
        # to the question is jane owner of c1?, the answer is yes
        assert nl.kb.ask(nl.Fact(jane, self.cms.IsOwner(of=c1), self.cms.Duration(start=self.cms.Instant('now'))))
        # to the question has c1 private state?, the answer is yes
        assert nl.kb.ask(nl.Fact(c1, self.cms.Has(what=self.cms.private), self.cms.Duration(start=self.cms.Instant('now'))))
        # to the question is pete owner of c2?, the answer is no
        assert not nl.kb.ask(nl.Fact(pete, self.cms.IsOwner(of=c2), self.cms.Duration(start=self.cms.Instant('now'))))
        # jane wants to publish c1
        nl.kb.tell(nl.Fact(jane, self.cms.Wants(to=self.cms.Publish(what=c1)), self.cms.Instant('now')))
        # pete wants to publish c2
        nl.kb.tell(nl.Fact(pete, self.cms.Wants(to=self.cms.Publish(what=c2)), self.cms.Instant('now')))
        # extend the db
        nl.kb.extend()
        # to the question is c1 public?, the answer is no
        assert not nl.kb.ask(nl.Fact(c1, self.cms.Has(what=self.cms.public), self.cms.Duration(start=self.cms.Instant('now'))))
        # to the question is c2 public?, the answer is no
        assert not nl.kb.ask(nl.Fact(c2, self.cms.Has(what=self.cms.public), self.cms.Duration(start=self.cms.Instant('now'))))
        # to the question can jane view c1?, the answer is yes
        assert nl.kb.ask(nl.Fact(jane, self.cms.Can(what=self.cms.View(what=c1)), self.cms.Duration(start=self.cms.Instant('now'))))
        # to the question can pete view c1?, the answer is no
        assert not nl.kb.ask(nl.Fact(pete, self.cms.Can(what=self.cms.View(what=c1)), self.cms.Duration(start=self.cms.Instant('now'))))
        # john wants to publish c1
        nl.kb.tell(nl.Fact(john, self.cms.Wants(to=self.cms.Publish(what=c1)), self.cms.Instant('now')))
        # extend the db
        nl.kb.extend()
        # to the question is c1 private?, the answer is no
        assert not nl.kb.ask(nl.Fact(c1, self.cms.Has(what=self.cms.private), nl.Duration(start=nl.Instant('now'))))
        # to the question is c1 public?, the answer is yes
        assert nl.kb.ask(nl.Fact(c1, self.cms.Has(what=self.cms.public), nl.Duration(start=nl.Instant('now'))))
        # to the question can pete view c1?, the answer is yes
        assert nl.kb.ask(nl.Fact(pete, self.cms.Can(what=self.cms.View(what=c1)), nl.Duration(start=nl.Instant('now'))))

        # what can pete view? -> c1
        assert nl.kb.ask(self.cms.Content('X1'), nl.Fact(pete, self.cms.Can(what=self.cms.View(what='X1')), nl.Duration(start=nl.Instant('now')))) == [{'X1': 'c1'}]

        # who can view what?
        assert nl.kb.ask(self.cms.Content('X1'), self.cms.Person('X2'), nl.Fact(self.cms.Person('X2'), self.cms.Can(what=self.cms.View(what='X1')), nl.Duration(start=nl.Instant('now')))) == [{'X2': 'admin', 'X1': 'c1'}, {'X2': 'john', 'X1': 'c1'}, {'X2': 'pete', 'X1': 'c1'}, {'X2': 'jane', 'X1': 'c1'}]

        # c2 is private
        nl.kb.tell(nl.Fact(c2, self.cms.Has(what=self.cms.private), nl.Duration(start=nl.Instant('now'))))
        nl.kb.extend()

        # who can view what?
        assert nl.kb.ask(self.cms.Content('X1'), self.cms.Person('X2'), nl.Fact(self.cms.Person('X2'), self.cms.Can(what=self.cms.View(what='X1')), nl.Duration(start=nl.Instant('now')))) == [{'X2': 'admin', 'X1': 'c1'}, {'X2': 'john', 'X1': 'c1'}, {'X2': 'pete', 'X1': 'c1'}, {'X2': 'jane', 'X1': 'c1'}, {'X2': 'admin', 'X1': 'c2'}, {'X2': 'john', 'X1': 'c2'}]

        # what permissions has admin?
        assert nl.kb.ask(self.cms.Permission('X2'), nl.Fact(self.cms.manager, self.cms.Has(what=self.cms.Permission('X2')), nl.Duration(start=nl.Instant('X3')))) == [{'X2': 'basic_perm'}, {'X2': 'manage_perm'}, {'X2': 'create_perm'}]

        # can admin view c2?
        assert nl.kb.ask(nl.Fact(self.cms.admin, self.cms.Can(what=self.cms.View(what=c2)), nl.Duration(start=nl.Instant('now'))))


class cms3_test(object):
    def setup(self):
        from nl.examples import cms3
        self.cms = cms3

    def teardown(self):
        reset()
        del self.cms

    def _add_context(self, context):
        nl.kb.tell(self.cms.Context(context))

    def _add_content(self, content, status, context):
        nl.kb.tell(self.cms.Document(content),
                   nl.Fact(self.cms.Content(content),
                           self.cms.Has(what=self.cms.Status(status)),
                           nl.Duration(start=nl.Instant('now'))),
                   nl.Fact(self.cms.Content(content),
                           self.cms.Located(where=self.cms.Context(context)),
                           nl.Duration(start=nl.Instant('now'))))

    def _add_user(self, user, role, context):
        nl.kb.tell(self.cms.Person(user),
                   nl.Fact(self.cms.Person(user),
                           self.cms.Has(what=self.cms.Role(role),
                                        where=self.cms.Context(context)),
                           nl.Duration(start=nl.Instant('now'))))

    def cms_test(self):
        contexts = ('one', 'two', 'three')
        for c in contexts:
            self._add_context(c)
        self.cms.r_workflow_for_content(self.cms.Document,
                                        self.cms.doc_workflow,
                                        self.cms.Context('one'))
        for n in xrange(0, 100, 3):
            for m, c in enumerate(contexts):
                self._add_content('cpu%d' % (n+m), 'public', c)
        for n in xrange(0, 100, 3):
            for m, c in enumerate(contexts):
                self._add_content('cpr%d' % (n+m), 'private', c)
        for n in xrange(0, 50, 3):
            for m, c in enumerate(contexts):
                self._add_user('m%d' % (n+m), 'manager', c)
        for n in xrange(0, 100, 3):
            for m, c in enumerate(contexts):
                self._add_user('e%d' % (n+m), 'editor', c)
        for n in xrange(0, 300, 3):
            for m, c in enumerate(contexts):
                self._add_user('u%d' % (n+m), 'member', c)

        nl.kb.tell(nl.Fact(self.cms.Person('m3'),
                           self.cms.Wants(
                                   to=self.cms.Publish(
                                             what=self.cms.Content('cpr3'))),
                           self.cms.Instant('now')))

        nl.kb.extend()
        assert nl.kb.ask(nl.Fact(self.cms.Person('m3'),
                                 self.cms.Publish(what=self.cms.Content('cpr3')),
                                 nl.Instant('now')))
        assert nl.kb.ask(nl.Fact(self.cms.Document('cpr3'),
                                 self.cms.Has(what=self.cms.Status('public')),
                                 nl.Duration(start=nl.Instant('now'))))

        nl.change_now()

        nl.kb.tell(nl.Fact(self.cms.Person('m3'),
                           self.cms.Wants(
                                   to=self.cms.Hide(
                                             what=self.cms.Content('cpu3'))),
                           self.cms.Instant('now')))

        nl.kb.extend()

        assert nl.kb.ask(nl.Fact(self.cms.Document('cpu3'),
                                 self.cms.Has(what=self.cms.Status('private')),
                                 nl.Duration(start=nl.Instant('now'))))

        nl.change_now()

        nl.kb.tell(nl.Fact(self.cms.Person('m6'),
                           self.cms.Wants(
                                   to=self.cms.Publish(
                                             what=self.cms.Content('cpu3'))),
                           self.cms.Instant('now')))

        nl.kb.extend()

        assert nl.kb.ask(nl.Fact(self.cms.Person('m6'),
                                 self.cms.Publish(what=self.cms.Content('cpu3')),
                                 nl.Instant('now')))

        assert nl.kb.ask(nl.Fact(self.cms.Document('cpu3'),
                                 self.cms.Has(what=self.cms.Status('public')),
                                 nl.Duration(start=nl.Instant('now'))))

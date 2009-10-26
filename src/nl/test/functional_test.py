import nl


class cms_test(object):
    def setup(self):
        from nl.examples import cms2
        self.cms = cms2

    def teardown(self):
        pass

#    def thing_test(self):
#        pooh = self.cms.Person('pooh')
#        assert repr(pooh) == 'pooh is a Person'
#        assert str(pooh) == 'pooh'

#    def second_test(self):
#        # john is a person
#        john = self.cms.Person('john')
#        # pete is a person
#        pete = self.cms.Person('pete')
#        # jane is a person
#        jane = self.cms.Person('jane')
#        # c1 is a content
#        c1 = self.cms.Content('c1')
#        # c2 is a content
#        c2 = self.cms.Content('c2')
#        # john has role manager
#        p1 = nl.Fact(john, self.cms.Has(what=self.cms.manager))
#        # jane has create_perm
#        p2 = nl.Fact(jane, self.cms.Has(what=self.cms.create_perm))
#        # jane wants to create c1
#        p3 = nl.Fact(jane, self.cms.Wants(to=self.cms.Create(what=c1)))
#        # pete wants to create c2
#        p4 = nl.Fact(pete, self.cms.Wants(to=self.cms.Create(what=c2)))
#        # input everything into the db
#        nl.kb.tell(john, pete, jane, c1, c2, p1, p2, p3, p4)
#        # to the question is jane owner of c1?, the answer is no
#        assert nl.kb.ask(nl.Fact(jane, self.cms.IsOwner(of=c1))) == 'no'
#        # extend the db
#        nl.kb.extend()
#        # to the question is jane owner of c1?, the answer is yes
#        assert nl.kb.ask(nl.Fact(jane, self.cms.IsOwner(of=c1), nl.Duration(start=nl.now))) == 'jane isowner of c1 at %s.0' % nl.time._now
#        # to the question has c1 private state?, the answer is yes
#        assert nl.kb.ask(nl.Fact(c1, self.cms.Has(what=self.cms.private))) == 'c1 has what private at %s.0' % nl.time._now
#        # to the question is pete owner of c2?, the answer is no
#        assert nl.kb.ask(nl.Fact(pete, self.cms.IsOwner(of=c2))) == 'no'
#        # jane wants to publish c1
#        nl.kb.tell(nl.Fact(jane, self.cms.Wants(to=self.cms.Publish(what=c1))))
#        # pete wants to publish c2
#        nl.kb.tell(nl.Fact(pete, self.cms.Wants(to=self.cms.Publish(what=c2))))
#        # extend the db
#        nl.kb.extend()
#        # to the question is c1 public?, the answer is no
#        assert nl.kb.ask(nl.Fact(c1, self.cms.Has(what=self.cms.public))) == 'no'
#        # to the question is c2 public?, the answer is no
#        assert nl.kb.ask(nl.Fact(c2, self.cms.Has(what=self.cms.public))) == 'no'
#        # to the question can jane view c1?, the answer is yes
#        assert nl.kb.ask(nl.Fact(jane, self.cms.Can(what=self.cms.View(what=c1)))) == 'jane can what view what c1 at %s.0' % nl.time._now
#        # to the question can pete view c1?, the answer is no
#        assert nl.kb.ask(nl.Fact(pete, self.cms.Can(what=self.cms.View(what=c1)))) == 'no'
#        # john wants to publish c1
#        nl.kb.tell(nl.Fact(john, self.cms.Wants(to=self.cms.Publish(what=c1))))
#        # extend the db
#        nl.kb.extend()
#        # to the question is c1 private?, the answer is no
#        assert nl.kb.ask(nl.Fact(c1, self.cms.Has(what=self.cms.private))) == 'no'
#        # to the question is c1 public?, the answer is yes
#        assert nl.kb.ask(nl.Fact(c1, self.cms.Has(what=self.cms.public))) == 'c1 has what public at -1.0'
#        # to the question can pete view c1?, the answer is yes
#        assert nl.kb.ask(nl.Fact(pete, self.cms.Can(what=self.cms.View(what=c1)))) == 'pete can what view what c1 at -1.0'
#
#
#
#        assert not nl.kb.app.root()['props'].has_key('c1 has what private at -1.0')
#        assert nl.kb.app.root()['props'].has_key('c1 has what public at -1.0')

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
        assert nl.kb.ask(nl.Fact(jane, self.cms.IsOwner(of=c1), self.cms.Duration(start=self.cms.Instant('now')))) == 'no'
        # extend the db
        nl.kb.extend()
        # to the question is jane owner of c1?, the answer is yes
        assert nl.kb.ask(nl.Fact(jane, self.cms.IsOwner(of=c1), self.cms.Duration(start=self.cms.Instant('now')))) == 'yes'
        # to the question has c1 private state?, the answer is yes
        assert nl.kb.ask(nl.Fact(c1, self.cms.Has(what=self.cms.private), self.cms.Duration(start=self.cms.Instant('now')))) == 'yes'
        # to the question is pete owner of c2?, the answer is no
        assert nl.kb.ask(nl.Fact(pete, self.cms.IsOwner(of=c2), self.cms.Duration(start=self.cms.Instant('now')))) == 'no'
        # jane wants to publish c1
        nl.kb.tell(nl.Fact(jane, self.cms.Wants(to=self.cms.Publish(what=c1)), self.cms.Instant('now')))
        # pete wants to publish c2
        nl.kb.tell(nl.Fact(pete, self.cms.Wants(to=self.cms.Publish(what=c2)), self.cms.Instant('now')))
        # extend the db
        nl.kb.extend()
        # to the question is c1 public?, the answer is no
        assert nl.kb.ask(nl.Fact(c1, self.cms.Has(what=self.cms.public), self.cms.Duration(start=self.cms.Instant('now')))) == 'no'
        # to the question is c2 public?, the answer is no
        assert nl.kb.ask(nl.Fact(c2, self.cms.Has(what=self.cms.public), self.cms.Duration(start=self.cms.Instant('now')))) == 'no'
        # to the question can jane view c1?, the answer is yes
        assert nl.kb.ask(nl.Fact(jane, self.cms.Can(what=self.cms.View(what=c1)), self.cms.Duration(start=self.cms.Instant('now')))) == 'yes'
        # to the question can pete view c1?, the answer is no
        assert nl.kb.ask(nl.Fact(pete, self.cms.Can(what=self.cms.View(what=c1)), self.cms.Duration(start=self.cms.Instant('now')))) == 'no'
        # john wants to publish c1
        nl.kb.tell(nl.Fact(john, self.cms.Wants(to=self.cms.Publish(what=c1)), self.cms.Instant('now')))
        # extend the db
        nl.kb.extend()
        # to the question is c1 private?, the answer is no
        assert nl.kb.ask(nl.Fact(c1, self.cms.Has(what=self.cms.private), nl.Duration(start=nl.Instant('now')))) == 'no'
        # to the question is c1 public?, the answer is yes
        assert nl.kb.ask(nl.Fact(c1, self.cms.Has(what=self.cms.public), nl.Duration(start=nl.Instant('now')))) == 'yes'
        # to the question can pete view c1?, the answer is yes
        assert nl.kb.ask(nl.Fact(pete, self.cms.Can(what=self.cms.View(what=c1)), nl.Duration(start=nl.Instant('now')))) == 'yes'

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
        assert nl.kb.ask(nl.Fact(self.cms.admin, self.cms.Can(what=self.cms.View(what=c2)), nl.Duration(start=nl.Instant('now')))) == 'yes'




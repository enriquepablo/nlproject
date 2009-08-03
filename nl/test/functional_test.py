import nl


class cms_test(object):
    def setup(self):
        nl.kb.open('test_cms')
        from nl.examples import cms
        self.cms = cms

    def teardown(self):
        import os
        from nl.log import here
        from glob import glob
        for f in glob(here + '/var/test_cms*'):
            os.remove(f)

    def thing_test(self):
        pooh = self.cms.Person('pooh')
        assert repr(pooh) == 'pooh is a Person'
        assert str(pooh) == 'pooh'

    def second_test(self):
        john = self.cms.Person('john')
        pete = self.cms.Person('pete')
        jane = self.cms.Person('jane')
        c1 = self.cms.Content('c1')
        c2 = self.cms.Content('c2')
        p1 = nl.Prop(john, self.cms.Has(what=self.cms.manager))
        p2 = nl.Prop(jane, self.cms.Has(what=self.cms.create_perm))
        p3 = nl.Prop(jane, self.cms.Wants(to=self.cms.Create(what=c1)))
        p4 = nl.Prop(pete, self.cms.Wants(to=self.cms.Create(what=c2)))
        nl.kb.tell(john, pete, jane, c1, c2, p1, p2, p3, p4)
        assert nl.kb.ask(nl.Prop(jane, self.cms.IsOwner(of=c1))) == 'no'
        nl.kb.extend()
        assert nl.kb.ask(nl.Prop(jane, self.cms.IsOwner(of=c1))) == 'jane isowner of c1 at now'
        assert nl.kb.ask(nl.Prop(c1, self.cms.Has(what=self.cms.private))) == 'c1 has what private at now'
        assert nl.kb.ask(nl.Prop(pete, self.cms.IsOwner(of=c2))) == 'no'
        nl.kb.tell(nl.Prop(jane, self.cms.Wants(to=self.cms.Publish(what=c1))))
        nl.kb.tell(nl.Prop(pete, self.cms.Wants(to=self.cms.Publish(what=c2))))
        nl.kb.extend()
        assert nl.kb.ask(nl.Prop(c1, self.cms.Has(what=self.cms.public))) == 'no'
        assert nl.kb.ask(nl.Prop(c2, self.cms.Has(what=self.cms.public))) == 'no'
        assert nl.kb.ask(nl.Prop(jane, self.cms.Can(what=self.cms.View(what=c1)))) == 'jane can what view what c1 at now'
        assert nl.kb.ask(nl.Prop(pete, self.cms.Can(what=self.cms.View(what=c1)))) == 'no'
        nl.kb.tell(nl.Prop(john, self.cms.Wants(to=self.cms.Publish(what=c1))))
        nl.kb.extend()
        assert nl.kb.ask(nl.Prop(c1, self.cms.Has(what=self.cms.private))) == 'no'
        #assert nl.kb.ask(nl.Prop(c1, self.cms.Has(what=self.cms.public))) == 'c1 has what public at now'
        #assert nl.kb.ask(nl.Prop(pete, self.cms.Can(what=self.cms.View(what=c1)))) == 'pete can what view what c1 at now'



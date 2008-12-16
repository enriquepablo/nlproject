"""
This is a generic irc bot, consisting of two modules, this and
the accompanying resolvers.py. It only depends on twisted; the
dependency on nl apparent in the fourth line of code is only there
for application initialization purposes, and can be easily removed.
And the fifth line could import resolver from wherever.
The code in this module is almost straight from
http://www.eflorenzano.com/blog/post/writing-markov-chain-irc-bot-twisted-and-python/,
and is commented in that link.
The only added code is, as I said the init code,
and a call to the resolve function (from resolver) in the
privmsg method of MacarronicBot.
For a synopsis of the use of the resolver module, see its docstring.

This is used, from the command line, like:
# python bot.py irc_channel
and it will connect to that channel on freenode.

"""

import re
from twisted.words.protocols import irc
from twisted.internet import protocol
import nl
from nl.bot.resolver import resolve, UnresolvedSentence

class MacarronicBot(irc.IRCClient):
    def _get_nickname(self):
        return self.factory.nickname
    nickname = property(_get_nickname)

    def signedOn(self):
        self.join(self.factory.channel)
        print "Signed on as %s." % (self.nickname,)

    def joined(self, channel):
        print "Joined %s." % (channel,)

    def privmsg(self, user, channel, msg):
        if not user:
            return
        if self.nickname in msg:
            msg = re.compile(self.nickname + "[:,]* ?", re.I).sub('', msg)
            prefix = "%s: " % (user.split('!', 1)[0], )
        else:
            prefix = ''
        try:
            f, args, kwargs = resolve(msg)
        except UnresolvedSentence:
            sentence = 'I do not understand'
        else:
            sentence = f(*args, **kwargs)
        self.msg(self.factory.channel, prefix + sentence)

class MacarronicBotFactory(protocol.ClientFactory):
    protocol = MacarronicBot

    def __init__(self, channel, nickname='macarronic_bot'):
        self.channel = channel
        self.nickname = nickname

    def clientConnectionLost(self, connector, reason):
        print "Lost connection (%s), reconnecting." % (reason,)
        connector.connect()

    def clientConnectionFailed(self, connector, reason):
        print "Could not connect: %s" % (reason,)


import sys
from twisted.internet import reactor

if __name__ == "__main__":
    chan = sys.argv[1]
    reactor.connectTCP('irc.freenode.net', 6667, MacarronicBotFactory('#' + chan))
    nl.kb.open()
    try:
        reactor.run()
    finally:
        nl.kb.close()

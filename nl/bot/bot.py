"""
This is a generic irc bot, consisting of two modules, this and
the accompanying resolvers.py. It only depends on twisted; the
dependency on nl apparent in line 24 is only there
for application initialization purposes (lines 76 & 80),
and can be easily removed.
And line 26 could import resolver from wherever.
The code in this module is almost straight from
http://www.eflorenzano.com/blog/post/writing-markov-chain-irc-bot-twisted-and-python/,
and is commented in that link.
The only added code is, as I said, the init code,
and a call to the resolve function (from the resolver module) in the
privmsg method of MacarronicBot.
For a synopsis of the use of the resolver module, see its docstring.

This is used, from the command line, like:
# python bot.py irc_channel
and it will connect to #irc_channel on freenode.

"""

import re
from twisted.words.protocols import irc
from twisted.internet import protocol
import nl
from nl.bot.resolver import resolve, UnresolvedSentence

partial_msg = ''
partial = False

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
        if user and self.nickname in msg:
            msg = re.compile(self.nickname + "[:,]* ?", re.I).sub('', msg)
            prefix = "%s: " % (user.split('!', 1)[0], )
            if msg == 'begin':
                msg = ''
                partial = True
            elif msg == 'end':
                msg = partial_msg
                partial = False
            if partial:
                global partial_msg
                partial_msg += ' ' + msg
                return 'yes...'
            try:
                f, args, kwargs = resolve(msg)
            except UnresolvedSentence:
                sentence = 'I do not understand'
            else:
                sentence = f(*args, **kwargs)
            self.msg(self.factory.channel, prefix + sentence)

class MacarronicBotFactory(protocol.ClientFactory):
    protocol = MacarronicBot

    def __init__(self, channel, nickname='luis_ricardo'):
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
    nick = sys.argv[1]
    chan = sys.argv[2]
    reactor.connectTCP('irc.freenode.net', 6667,
                       MacarronicBotFactory('#' + chan, nickname=nick))
    nl.kb.open(nick)
    try:
        reactor.run()
    finally:
        nl.kb.close()

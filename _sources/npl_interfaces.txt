Interfacing with npl
====================

You can use **npl** in several different ways. You can start an ircbot and
talk to
it in an irchat, in a REPL manner. Or you can start a daemon and talk to it
over HTTP. Or you can use it from python (for this, refer to the
`nl documentation <nl_intro>`_).

Ircbot
------

From the root of the nl buildout, do:

::

  $ bin/ircbot some_name
  Generating LALR tables
  Signed on as some_name_bot.
  Joined #nlpbot_some_name.

Now you can talk to the ircbot on freenode at channel #nlpbot_some_name.
You provide definitions, facts and rules ending them with a dot, and you ask
facts ending them with a question mark. Question facts can contain variables.
Whenever you tell or ask something to the bot, you have to address it
prefixing your message with its nickname: "some_name_bot: <message>" where <message>
is any definition, fact, rule, or question.

HTTP
----

You can now also start a daemon, and, as I said, talk to it over HTTP::

  $ bin/npldaemon
  $

A telnet session now with the daemon::

  $ telnet localhost 8280
  Trying ::1...
  Trying 127.0.0.1...
  Connected to localhost.
  Escape character is '^]'.
  person are thing.
  Noun Person defined.Connection closed by foreign host.

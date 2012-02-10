
The nl language
===============

The only documentation about nl is a blog entry hereXXX.

Install
-------

to install::

  $ git clone git://github.com/enriquepablo/nlp.buildouts.git
  $ cd nlp.buildouts
  $ cp buildout.cfg.in buildout.cfg
  $ vim buildout.cfg  # -> uncomment profiles/ircbot.cfg
  $ virtualenv --no-site-packages --python=python2.7 .
  $ source bin/activate
  $ python bootstrap.py
  $ bin/buildout
  ...
  $ bin/ircbot some_name
  Generating LALR tables
  Signed on as some_name_bot.
  Joined #nlpbot_some_name.

If you obtain any errors, please report at XXX.

And you can talk to the ircbot on freenode at channel #nlpbot_some_name.
You provide definitions, facts and rules ending them with a dot, and you ask
facts ending them with a question mark. Question facts can contain variables.
Whenever you tell or ask something to the bot, you have to address it
prefixing your message with "some_name_bot: <message>" where <message>
is any definition, fact, rule, or question.



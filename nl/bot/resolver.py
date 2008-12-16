
"""
A dispatcher module, extracted mainly from django
and a little from the werkzeug tutorial.

This module stores, in the patterns list, instances of RegexPattern,
a very stripped down version of django's RegexURLPattern.
The function resolve (a stripped down version of django's)
searches in the list of patterns for the one that matches the given
string, and calls the associated function.

And then, you have a decorator, expose, that you import in other
app specific modules and decorate the functions there with it.
like so:

@expose(r"^hi I'm (?P<name>.*)$")
def say_hi(name):
    return 'hi ' + name + ' I'm macarronic_bot'
"""

import re

patterns = []

class UnresolvedSentence(Exception):
    """
    """
    def __init__(self, sentence):
        self.sentence = sentence

    def __str__(self):
        return '"Unresolved sentence: "%s"' % (self.sentence)


class RegexPattern(object):
    def __init__(self, regex, callback):
        self.regex = re.compile(regex, re.UNICODE)
        self.callback = callback

    def resolve(self, sentence):
        match = self.regex.search(sentence)
        if match:
            # If there are any named groups, use those as kwargs, ignoring
            # non-named groups. Otherwise, pass all non-named arguments as
            # positional arguments.
            kwargs = match.groupdict()
            if kwargs:
                args = ()
            else:
                args = match.groups()
            return self.callback, args, kwargs

def resolve(sentence):
    for pattern in patterns:
        match = pattern.resolve(sentence)
        if match:
            return match
    raise UnresolvedSentence(sentence)

def expose(*args):
    def decorate(f):
        for regex in args:
            patterns.append(RegexPattern(regex, f))
            return f
    return decorate

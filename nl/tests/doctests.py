import glob
import testbase
import unittest
import doctest
from os import path
# from nl import kb


def suite():
    alltests = unittest.TestSuite()
    # for doctests in docstrings, import modules and put them in the list below
    #for mod in (kb,):
    #    alltests.addTest(doctest.DocTestSuite(mod)), r3
    here = path.join('/'.join(path.dirname(__file__).split('/')[:-1]))
    for filename in glob.glob(here + '/docs/*.txt'):
        alltests.addTest(doctest.DocFileSuite(filename, module_relative=False))
    return alltests


if __name__ == '__main__':
    testbase.main(suite())

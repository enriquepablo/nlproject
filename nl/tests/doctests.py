import glob
import testbase
import unittest
import doctest
import path
# from nl import kb


def suite():
    alltests = unittest.TestSuite()
    # for doctests in docstrings, import modules and put them in the list below
    #for mod in (kb,):
    #    alltests.addTest(doctest.DocTestSuite(mod)), r3
    here = path.join(path.dirname(__file__))
    for filename in glob.glob(here + '/../docs/*.txt'):
        alltests.addTest(doctest.DocFileSuite(filename))
    return alltests


if __name__ == '__main__':
    testbase.main(suite())

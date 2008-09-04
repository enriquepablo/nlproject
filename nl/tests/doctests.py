import glob
import testbase
import unittest
import doctest


def suite():
    alltests = unittest.TestSuite()
    # for doctests in docstrings, import modules and put them in the list below
    for mod in ():
        alltests.addTest(doctest.DocTestSuite(mod))
    for filename in glob.glob('../doc/*.txt'):
        alltests.addTest(doctest.DocFileSuite(filename))
    return alltests


if __name__ == '__main__':
    testbase.main(suite())

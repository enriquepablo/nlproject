import testbase
import unittest

import doctests
# import more test modules with aa suite function and add them below as suites


def suite():
    alltests = unittest.TestSuite()
    for suite in (doctests):
        alltests.addTest(suite.suite())
    return alltests


if __name__ == '__main__':
    testbase.main(suite())

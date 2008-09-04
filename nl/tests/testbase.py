import sys
import unittest


def main(suite=None):

    if not suite:
        if len(sys.argv[1:]):
            suite =unittest.TestLoader().loadTestsFromNames(sys.argv[1:], __import__('__main__'))
        else:
            suite = unittest.TestLoader().loadTestsFromModule(__import__('__main__'))

    runner = unittest.TextTestRunner(verbosity = 3)
    result = runner.run(suite)
    sys.exit(not result.wasSuccessful())
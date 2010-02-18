# -*- coding: utf-8 -*-
from setuptools import setup, find_packages

setup(
    name = "nl",
    version = "0.95",
    url = 'http://bitbucket.org/enriquepablo/nl/wiki/Home',
    license = 'GPL',
    description = "A python library that provides a production system with an API modelled on the natural language",
    author = 'Enrique Perez Arnaud',
    author_email = 'enriquepablo@gmail.com',
    packages = find_packages('src'),
    package_dir = {'': 'src'},
    classifiers = [
                   "Topic :: Scientific/Engineering :: Artificial Intelligence",
                   "Programming Language :: Python",
                   "Topic :: Software Development :: Libraries :: Python Modules"
                  ],
    zip_safe = False,
    dependency_links = [
    ],
    install_requires = ['setuptools>=0.6c11',],
    entry_points = {
        'console_scripts':
            [ 'plot_ph22 = nl.examples.physics22:plotPh22', ],
    }
)

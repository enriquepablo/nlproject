# -*- coding: utf-8 -*-
from setuptools import setup, find_packages

setup(
    name = "nl",
    version = "1.0",
    url = 'http://bitbucket.org/enriquepablo/nl/wiki/',
    license = 'GPL',
    description = "A natural language-like python API",
    author = 'Enrique Pérez Arnaud',
    packages = find_packages('nl'),
    package_dir = {'': 'nl'},
    install_requires = ['setuptools'],
)

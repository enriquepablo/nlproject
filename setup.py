# -*- coding: utf-8 -*-
from setuptools import setup, find_packages

setup(
    name = "nl",
    version = "0.90",
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
        'http://sourceforge.net/project/showfiles.php?group_id=114052',
        'http://sourceforge.net/projects/pyclips/files/',
        'http://bitbucket.org/enriquepablo/nl/downloads/',
        'http://bitbucket.org/enriquepablo/nl/downloads/pyclips-1.0.7.348_clips_6.24-py2.5-linux-i686.egg',
    ],
    install_requires = ['setuptools', 'pyclips'],
    entry_points = {
        'console_scripts':
            [ 'plot_ph22 = nl.examples.physics22:plotPh22', ],
    }
)

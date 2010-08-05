#!/bin/bash

git add docs/*.rst
git add docs/nl-static/*
git add docs/nl-templates/*
git commit -m 'updated docs'
git push
cd docs
make html
cd nl-build/html
git add *
git commit -m 'updated docs'
git push origin gh-pages

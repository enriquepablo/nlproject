#!/bin/bash

cp -R _static/* static/
find . -name '*.html' -exec sed -i 's/\<_static\>/static/g' {} \;
find . -name '*.html' -exec git add {} \;
git add static/*
git add searchindex.js
git commit -m 'updated docs'
git push origin gh-pages

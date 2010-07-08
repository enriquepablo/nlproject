#!/bin/bash

find . -name '*.html' -exec gawk '{gsub(/_static/,"static");print > "{}"}' {} \;
find . -name '*.html' -exec git add {} \;
cp -R _static/* static/
git add static/*
git commit -m 'static'
git push origin gh-pages

find . -name '*.html' -exec gawk '{gsub(/_static/,"static");print > "{}"}' {}; git add {} \;
cp -R _static/* static/
git add static/*
git commit -m 'static'
git push origin gh-pages

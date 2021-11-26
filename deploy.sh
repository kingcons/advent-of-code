#!/bin/sh
git branch -D gh-pages
git checkout -b gh-pages
sbcl --eval "(ql:quickload :advent)" --eval "(aoc::build-site)" --quit
ln -sf advent.html docs/index.html
git add -f docs/
git commit -m "Generate site."
git push -f origin gh-pages
git checkout -

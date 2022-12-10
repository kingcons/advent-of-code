#!/bin/sh
git branch -D gh-pages
git checkout -b gh-pages
sbcl --non-interactive \
     --eval "(ql:quickload :advent)" \
     --eval "(asdf:compile-system :advent :force t)" \
     --eval "(aoc::build-site)" --quit
ln -sf advent.html docs/index.html
git add -f docs/
git commit -m "Generate site."
git push -f origin gh-pages
git checkout -

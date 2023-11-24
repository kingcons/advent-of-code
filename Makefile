.PHONY: all repl site test clean

all: test site

clean:
	rm -rf bin/advent site

repl:
	rm -f .swank-port
	sbcl --eval "(ql:quickload :advent)" \
	     --eval "(aoc:repl)"

site:
	sbcl --non-interactive \
	     --eval "(ql:quickload :advent)" \
	     --eval "(funcall #'advent.docs:build-site)" \
	     --quit

test:
	sbcl --non-interactive \
	     --eval "(ql:quickload :advent/tests)" \
	     --eval "(funcall #'advent.tests:test-ci)" \
	     --quit

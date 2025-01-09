
build: manual.html

reference.md: ../src/*.lisp  ../lib/*.stencil
	./extract-reference.sh

%.html: %.md reference.md
	turnup $< > $@

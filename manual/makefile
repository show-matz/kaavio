
build: manual.html

PARTS=$(wildcard [0-9][0-9]*.md)       \
      $(wildcard img/figure.*.png)     \
      $(wildcard img/figure.*.jpg)     \
      $(wildcard img/figure.*.diagram)

reference.md: ../src/*.lisp  ../lib/*.stencil
	./extract-reference.sh

%.html: %.md reference.md $(PARTS)
	turnup $< > $@

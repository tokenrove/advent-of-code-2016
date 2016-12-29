CC ?= gcc
CFLAGS ?= -Wall -Wextra -Wconversion -g
JAVAC ?= javac
CM3 ?= /usr/local/cm3/bin/cm3

all: day1 day5 day8 day9 day10 day12 day20

clean:
	$(RM) day1 Day5.class day8 day8.o day9
	cd day9_m3/ && $(CM3) -realclean

.c.o:
	$(CC) -c $(CFLAGS) -o $@ $^

.s.o:
	$(AS) --32 -o $@ $^

Day5.class: Day5.java
	$(JAVAC) $^

day1:	day1.lisp
	buildapp --output $@ --load $^ --entry day1:main

day12:	day12.lisp
	buildapp --output $@ --load $^ --entry main
day20: day20.hs
	ghc --make day20

day5: Day5.class

day8: day8.o
	$(CC) -o $@ $^

day9: day9_m3/Day9.m3
	cd day9_m3 && $(CM3) -build
	cp day9_m3/*/day9 .	# stupid

day10: day10.ml
	ocamlc -o $@ $^

check: all
	prove -v

.PHONY: clean all check

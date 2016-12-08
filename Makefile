
CC ?= gcc
CFLAGS ?= -Wall -Wextra -Wconversion -g
JAVAC ?= javac

all: day1 day5 day8

clean:
	$(RM) day1.o day1

.c.o:
	$(CC) -c $(CFLAGS) -o $@ $^

.s.o:
	$(AS) --32 -o $@ $^

Day5.class: Day5.java
	$(JAVAC) $^

day1:	day1.o
	ld -melf_i386 -nostdlib -o $@ $^

day5: Day5.class

day8: day8.o
	$(CC) -o $@ $^

check: all
	prove -v

.PHONY: clean all check

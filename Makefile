
JAVAC ?= javac

all: day1 day5

clean:
	$(RM) day1.o day1

.s.o:
	$(AS) --32 -o $@ $^

Day5.class: Day5.java
	$(JAVAC) $^

day1:	day1.o
	ld -melf_i386 -nostdlib -o $@ $^

day5: Day5.class

check: all
	prove -v

.PHONY: clean all check


all: day1

clean:
	$(RM) day1.o day1

.s.o:
	$(AS) --32 -o $@ $^

day1:	day1.o
	ld -melf_i386 -nostdlib -o $@ $^

check: all
	./t/tests.bats

.PHONY: clean all check

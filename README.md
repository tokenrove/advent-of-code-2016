
My [Advent of Code 2016](http://adventofcode.com) solutions.

My goal was to finish each day in a different language, although I
don't know if I'll go through with that.  I did make a list of 46
languages I had at least written a trivial program in before that
would be usable for solving problems.

## Requirements

 - bats, prove for testing
 - day 1: Common Lisp: sbcl and buildapp
 - day 2: JavaScripts: node.js
 - day 3: GNU APL
 - day 4: ruby 2.x
 - day 5: Java
 - day 6: [J 8](http://jsoftware.com)
 - day 7: perl 5
 - day 8: C: any reasonable C compiler
 - day 9: Modula-3: [cm3](https://modula3.elegosoft.com/cm3/)

## Notes

I don't know why I started using `--initial` as the argument to switch
between the first and second halves of the problems.  It's dumb, I
know.

### day 1

Initially I wanted to do this in x86 assembly abusing the BCD
instructions, to demonstrate what compact code they can produce.  I
also thought that maybe keeping the coordinates in Morton Z-ordering
might be fun.

But I didn't finish this implementation, and a few days later, I went
back, and decided to implement this using complex numbers to represent
points.  Except for the text processing, this would be a nice short
one-liner in J, which has complex support built-in, but I decided to
use Common Lisp which I like almost as much as J for interactively
playing with problems, and which also has great complex support.

When I got to the second part, I was glad I hadn't gone through with
my assembly implementation.  Already, I was a little screwed as
obviously it would have been easier if I could have marked a bitmap or
something with visited points.  (At this scale, even keeping a hash
table of each point might be okay, as long as you actually store each
grid point visited, not just the end points.)  However I wanted to go
all the way and so implemented line collision detection.

I realized this might be a fun one to implement in PostScript or
LaTeX, where one could have a visual output that could be inspected to
get the answer.  (Draw lines, annotate each endpoint with
coordinates.)

### Day 2

The idea of doing this as a functional graph appealed to me, with the
edges looping back on themselves.  I thought about using Prolog, where
expressing the graph relationship as functions would have been
elegant, but decided to use JavaScript as a contrast to x86 assembly
(which was supposed to be the implementation language of day 1).

I liked the idea that with JS objects, I could easily call functions
directly from the characters in the input.  I also could have had a
clever (awful) hack where, since I or `0x20` to the input to downcase
it, I could also have had `*` in the object and had that done the work
of printing a value.  However it soon became clear that just using
integers would be less cumbersome, so I scrapped my fun hack and added
an `if`, especially as I came up with what seemed like a clever way of
generating the connections in the grid.

For the second part, it was logical to just use hex so little else in
my program would have to change.  The means of wiring up the grid was
less elegant, but what can you do.

After writing this, I decided to go back and insert my awful `*` hack,
since I could use ES6 arrow notation to make things more terse.

### Day 3

Really simple in an array-oriented language.  Even simpler if one
realizes that one can test `2*max(a b c) < sum(a b c)`.  First sketch
was in J since I hadn't yet figured out how to write scripts with GNU
APL, which allowed me to use the nice backtick reduce construct in J,
but then I went back and wrote it in APL.  One-liner modulo argument
and input handling:

```apl
in ← (⍉⍤2) in ⍴⍨ (9÷⍨⍴in),3 3 ⋄ +/ (,2×⌈/ in) < (,+/⍤1 in)
```

### Day 4

Thought about using a language where I'd be forced to do a clever
state machine implementation here, but I was trying to catch up, so I
went with Ruby-pretending-to-be-Perl, which was actually pleasant to
write.

### Day 5

One of the first ones I wrote.  I knew that there would be something
like this, like there was last year, and it would make an all-J or
all-assembly run inconvenient, so I planned to use something where MD5
was in the standard library.  I wanted to implement an in-place
counter so the program didn't generate a ton of garbage as it
generated codes; I setup the interface for this, but then just
implemented the simple thing and let it burn CPU cycles.

I tried to avoid getting bitten by Java's lack of unsigned types and
got bitten anyway.

### Day 6

This kind of problem is just designed for vector languages.  Ignoring
argument and input handling, this is just a one-liner:
```j
, (~."1 t) {"1~ |: ,: (i.<./)"1 (#/.~"1 t)
```

### Day 7

I often make fun of perl's context-sensitive irregular expressions, so
this seemed like a safe opportunity to demonstrate their abuse.  I
didn't really sink to any depths with it, though, since the problem
ended up more restrained than I expected (no deep nesting, et cetera).

### Day 8

I decided to do this in C since I figured a bitmap would be an obvious
representation for this and C would make the bit-twiddling easy.  It
ended up less terse and less interesting than I had hoped, although I
threw in a few C abuses to make it more fun.

Doing this in a vector language probably would have been more
satisfying.  And doing this in CL would have avoided a bunch of bugs
that caused pain, due to things like C's modulo semantics and negative
numbers, implicit conversions of numbers that truncate or sign extend,
et cetera.

### Day 9

I wrote a prototype in Python before deciding to use Modula-3.  This
doesn't show off anything interesting about Modula-3, unfortunately.

I realized as I was writing it that, like many of these
implementations, it's broken with respect to some valid input which
doesn't occur in the test inputs.

### Day 10

Transitive closure; could use a graph representation and propagate
recursively.

Logic languages would probably make this trivial.

## Quotes

```
   /:~&.>"1 ({.;#)/."1~ |: t

Process J segmentation fault
```

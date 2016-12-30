
My [Advent of Code 2016](http://adventofcode.com) solutions.

My goal was to finish each day in a different language, although I
don't know if I'll go through with that.  I did make a list of 46
languages I had at least written a trivial program in before that
would be usable for solving problems.

In the end it was all a horrible rush and it's mostly hacks.  Such is
life.

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
 - day 10: OCaml
!- day 11: Prolog? should use Picat
?- day 12: Tcl?  CL
?- day 13: CL? something with arrays, recursion, and popcount
 - day 14: Python
?- day 15: J
?- day 16: CL
 - day 17: Erlang
?- day 18: CL
!- day 19:
!- day 20: Haskell
!- day 21: should use icon
!- day 22: Prolog
?- day 23: CL
?- day 24: CL
?- day 25: CL

## Notes

I don't know why I started using `--initial` as the argument to switch
between the first and second halves of the problems.  It's dumb, I
know, and then I decided to stay consistent.

### Day 1

Initially I wanted to do this in x86 assembly abusing the BCD
instructions (in particular `FBSTP`), to demonstrate what compact code
they can produce.  I also thought that maybe keeping the coordinates
in Morton Z-ordering might be fun.

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

Later I revisited this and avoided generating garbage.  It probably
would have been more productive to parallelize this, however.  Even
just using a `synchronized` variable would probably work, since the
contention on it shouldn't be high.

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

Logic languages would probably make this trivial.  I used OCaml
because it looked like an interpreter pattern would be important here,
but it turned out that Prolog would have been a much better choice.

### Day 11

PiCat, Prolog, or Mercury seems like the obvious choice here.  I
started in Prolog, and parsing the input with DCGs was great, but
couldn't seem to construct a suitable backtracking solver that didn't
blow up.

### Day 12

This was such a simple interpreter pattern that I immediately dashed
it off in Common Lisp, with an array holding the instructions, and
abusing `symbol-value` slightly for the registers.

My next thought was to do it in Tcl, where I could take that kind of
eval cheating to an extreme (maybe even safely with `interp`).  While
I was determining how to store the instructions in Tcl, I realized a
fun Scheme implementation might be to use a list and make all jump
targets point into the list, since they're all static, so we can avoid
the O(n) search at interpretation time.

### Day 13

Colored by day 11, I started approaching this as a classic AI search
problem and started implementing another BFS in Prolog.

Popcount modulo 2 is the parity of a word.

Flood fill is a better solution, especially once we get to part 2.

### Day 14

It would have been nice to do this with bit shifting tricks, but most
of the languages where working with 128-bit numbers is easy don't have
built-in MD5 primitives.

Erlang came to mind as a possibility (easy to work with large numbers,
and has built-in MD5), but I ended up just hacking out a quick Python
solution.

If I had done it in Erlang, I could have parallelized it, at least
searching each five-digit number separately

### Day 15

The Chinese Remainder Theorem immediately came to mind when I saw the
puzzle, and a quick check of the input for coprime bases confirmed it,
so my first approach was just to feed the values into
[maxima](http://maxima.sourceforge.net/docs/manual/maxima_29.html#chinese);
of course the initial positions actually need to have their position
added to their value (`init+1+⍳⍴init`).  But this wasn't giving me a
reasonable answer, so I brute forced it in a line of J.  We just need
to check `0 = bases | init+y` We know by the CRT that we don't need to
check any higher than `×/bases`.

I realized after brute forcing it that in order to use existing CRT
solvers, I needed to use `(bases|bases-init+1+i.$init)` as the
remainders.  A good CRT solver (like
[this one in J](http://code.jsoftware.com/wiki/Essays/Chinese_Remainder_Theorem))
solves the problem instantly.

### Day 16

It shouldn't be hard to figure out how to compute this without
actually executing the dragon curve, but once again, the numbers are
small enough for us to take a naïve approach.

There are some excuses to use fancy bit-twiddling tricks here,
especially Morton Z-order decoding.

It is hard to resist using Common Lisp given its profusion of bit
twiddling features and the ease of working with arbitrary-length bit
vectors.

### Day 17

Breadth-first search, then depth-first search.  Still need to
parallelize this.

### Day 18

```j
3 ({.={:);._3 (|.0,|.0,('^'='...^^.'))
```

### Day 19

I knew I had seen this before but I couldn't remember where. Then I
was talking with Vincent in the kitchen and he mentioned the elves
killing themselves, which was clue enough for me to suddenly realize
this was the Josephus problem.

### Day 22

Looks like a graph problem, although we see that we actually have
another Von Neumann-neighborhood-connected array like many of the
other search problems.

### Day 23

Although it's possible to run the second case without anything fancy,
as the instructions hint, it would be nice to speed this up.  I was
immediately reminded of strength reduction although of course this
wouldn't be that; more the opposite.  Detect induction variables and
strength "increase" their operations.

### Day 24

Although an early thought from seeing the example is to turn it into a
graph, collapsing corridors into edges with a weight equal to their
length, after seeing the actual input and knowing that the number of
points to hit was much smaller than the total space, what about BFS
for all pairs to find the shortest paths between points, then choose
the route from 0 that minimizes the distance?

Again, once you've constructed one path (perhaps the obvious one),
although you have to check every permutation, you can immediately
discard those paths that would be longer than the shortest path found.
I guess BFS again is possible but it's probably faster to use some
kind of dynamic programming formulation.

### Day 25

Just walked through the assembly by hand and then, although clearly
there was a closed-form way to figure out a value a smallest value
with appropriate remainders (should have alternating bits), I just
decided to run the inner loop with my simulator, bailing out if we
didn't emit the right pattern.  This quickly found the smallest value
with alternating 1/0 bits, from which I then removed the initial
offset.

## Quotes

```
   /:~&.>"1 ({.;#)/."1~ |: t

Process J segmentation fault
```

%% -*- prolog -*-

:- use_module(library(dcg/basics)).

input_file(Width, Height) -->
    header,
    grid(Width, Height).
header -->
    string_without("\n", _), "\n",
    string_without("\n", _), "\n".
grid(0,0) --> [].
grid(Width,Height) -->
    df_line(X,Y),
    grid(W1, H1),
    {(X > W1 -> Width = X; Width = W1),
     (Y > H1 -> Height = Y; Height = H1)}.
df_line(X,Y) -->
    "/dev/grid/node-x",
    number(X),
    "-y",
    number(Y),
    whites, number(_Size), "T",
    whites, number(Used), "T",
    whites, number(Avail), "T",
    whites, number(_Perc), "%",
    {assert(node(X,Y)),
     assert(used(X,Y,Used)),
     assert(avail(X,Y,Avail))},
    "\n".

empty(X,Y) :- used(X,Y,Used), Used = 0.

viable((AX,AY), (BX,BY)) :-
    not(empty(AX,AY)),
    not((AX,AY) = (BX,BY)),
    used(AX,AY,Used), avail(BX,BY,Avail),
    Used =< Avail.

all_possible(X,Y) :-
    width(W), height(H),
    between(0,W,X), between(0,H,Y).

all_viable :-
    all_possible(AX,AY),
    all_possible(BX,BY),
    viable((AX,AY),(BX,BY)).

read_input(File) :-
    phrase_from_file(input_file(Width, Height), File),
    print(Width), nl,
    assert(width(Width)),
    assert(height(Height)).

part_a(Count) :-
    aggregate_all(count, all_viable, Count).

%% -*- prolog -*-

input_file([F1,F2,F3,F4]) -->
    line(1,F1),line(2,F2),line(3,F3),line(4,F4).

line(Floor,Ts) -->
    "The ", floor_num(Floor), " floor contains ", things(Ts), ".\n".
floor_num(1) --> "first".
floor_num(2) --> "second".
floor_num(3) --> "third".
floor_num(4) --> "fourth".

things([]) --> "nothing relevant".
things([(E,K)]) --> thing(E,K).
things(Ts) --> more_things(Ts).

more_things([(E,K)|T]) --> thing(E,K), conjunction, more_things(T).
more_things([]) --> [].

thing(E, generator) -->
    article, element(L), {atom_string(E,L)}, " generator".
thing(E, microchip) -->
    article, element(L), {atom_string(E,L)}, "-compatible microchip".

article --> ("a ";"an ").
conjunction --> (", "; ", and "; " and "; []).

%% ("thulium";"plutonium";"strontium";"promethium";"ruthenium").
element([H|T]) --> [H], {code_type(H,alpha)}, element(T).
element([]) --> [].

final((_,[F1,F2,F3,F4]), (4,[[],[],[],Everything])) :-
    union(F1, F2, U12),
    union(U12, F3, U123),
    union(U123, F4, Everything).

initial(State, [F1,F2,F3,F4]) :-
    State = (1,[F1,F2,F3,F4]).

unsafe_floor(L) :-
    member((X,microchip),L),
    member((Y,generator),L),
    not(X = Y),
    not(memberchk((X,generator),L)),
    !.

safe_floor(L) :- not(unsafe_floor(L)).

safe((_,Floors)) :- maplist(safe_floor, Floors).

from(State,Next) :-
    up(State,Next), safe(Next);
    down(State,Next), safe(Next).

up((3,[F1,F2,OldIn,OldOut]),(4,[F1,F2,NewIn,NewOut])) :- move(OldIn,OldOut,NewIn,NewOut).
up((2,[F1,OldIn,OldOut,F4]),(3,[F1,NewIn,NewOut,F4])) :- move(OldIn,OldOut,NewIn,NewOut).
up((1,[OldIn,OldOut,F3,F4]),(2,[NewIn,NewOut,F3,F4])) :- move(OldIn,OldOut,NewIn,NewOut).

down((4,[F1,F2,OldOut,OldIn]),(3,[F1,F2,NewOut,NewIn])) :- move(OldIn,OldOut,NewIn,NewOut).
down((3,[F1,OldOut,OldIn,F4]),(2,[F1,NewOut,NewIn,F4])) :- move(OldIn,OldOut,NewIn,NewOut).
down((2,[OldOut,OldIn,F3,F4]),(1,[NewOut,NewIn,F3,F4])) :- move(OldIn,OldOut,NewIn,NewOut).

safe_with_stuff((X,generator), [(X,microchip)]).
safe_with_stuff((X,microchip), [(X,generator)]).
safe_with_stuff((_,Y), [(_,Y)]).
safe_with_stuff(_, []).

take(0, _, []) :- !.
take(N, [Head|Tail], [Head|Stuff]) :-
    safe_with_stuff(Head, Stuff),
    N1 is N-1,
    take(N1, Tail, Stuff).
take(N, [_|Tail], Stuff) :-
    take(N, Tail, Stuff).

take(In, Stuff) :- take(2, In, Stuff).
take(In, Stuff) :- take(1, In, Stuff).

move(In,OldDest,NewSrc,Out) :-
    take(In, Stuff),
    subtract(In, Stuff, NewSrc),
    union(Stuff, OldDest, Out).

solve_ids_once(_, _, Goal, Goal).
solve_ids_once(N, Depth, State, Goal) :-
    N < Depth,
    from(State, Next), safe(Next),
    N1 is N+1,
    solve_ids_once(N1, Depth, Next, Goal).

solve_ids(Depth, MaxDepth, Initial, Goal) :-
    between(0, MaxDepth, Depth),
    solve_ids_once(0, Depth, Initial, Goal).

solve(File, MaxDepth, Depth) :-
    read_input(File, Fs),
    initial(State, Fs),
    final(State, Goal),
    solve_ids(Depth, MaxDepth, State, Goal).

read_input(File, Fs) :-
    phrase_from_file(input_file(Fs), File).

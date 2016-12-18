-module(day17).
-export([char_of_dir/1,
         dirs_of_code/2,
         dirs_of_code/3,
         point_by_dir/2,
         pos_of_path/2,
         dir_of_char/1,
         first/1, second/1,
         longest/5,
         bfs/2]).

-type dir() :: up|down|left|right.
-type point() :: {0..3,0..3}.

-record(state,
        {
          base = "ihgpwlah" :: string(),
          start = {0,0} :: point(),
          goal = {3,3} :: point()
        }).

-spec char_of_dir(dir()) -> char().
char_of_dir(up)    -> $U;
char_of_dir(down)  -> $D;
char_of_dir(left)  -> $L;
char_of_dir(right) -> $R.

-spec dir_of_char(char()) -> dir().
dir_of_char($U) -> up;
dir_of_char($D) -> down;
dir_of_char($L) -> left;
dir_of_char($R) -> right.

-spec dirs_of_code(string(), [dir()]) -> [dir()].
dirs_of_code(Base, Path) ->
    <<U:4,D:4,L:4,R:4,_/bitstring>> =
        crypto:md5(lists:reverse([char_of_dir(X) || X <- Path] ++ Base)),
    %% (D bshl 3) bor (R bshl 2) bor (U bshl 1) bor (L bshl 0).
    lists:filtermap(fun ({X,V}) when X > 16#a -> {true,V}; (_) -> false end,
                    [{D,down},{R,right},{U,up},{L,left}]).

dirs_of_code(Base, Path, Point) ->
    lists:filter(fun (Dir) ->
                         {X,Y} = point_by_dir(Point, Dir),
                         X >= 0 andalso Y >= 0 andalso X < 4 andalso Y < 4
                 end, dirs_of_code(Base, Path)).

-spec point_by_dir(point(), dir()) -> point().
point_by_dir({X,Y}, up)    -> {X,Y-1};
point_by_dir({X,Y}, down)  -> {X,Y+1};
point_by_dir({X,Y}, left)  -> {X-1,Y};
point_by_dir({X,Y}, right) -> {X+1,Y}.

-spec pos_of_path(point(), [dir()]) -> point().
pos_of_path(Point, [])    -> Point;
pos_of_path(Point, [H|T]) -> pos_of_path(point_by_dir(Point, H), T).

bfs(Agenda, State=#state{goal=Goal,
                         base=Base}) ->
    {Point,Path} = queue:get(Agenda),           % intentionally error if empty
    case Point == Goal of
        true -> Path;
        _ ->
            Dirs = dirs_of_code(Base, Path, Point),
            bfs(queue:join(queue:drop(Agenda),
                           queue:from_list([begin P = point_by_dir(Point, D), {P,[D|Path]} end || D <- Dirs])),
                State)
    end.


longest(Goal, _P, N, Longest, #state{goal=Goal}) when N > Longest ->
    N;
longest(Goal, _P, _N, Longest, #state{goal=Goal}) ->
    Longest;
longest(Point, Path, N, Longest, State=#state{base=Base}) ->
    lists:max([Longest|
               [longest(point_by_dir(Point, D),
                        [D|Path],
                        N+1,
                        Longest,
                        State)
                || D <- dirs_of_code(Base, Path, Point)]]).

first(Base) ->
    Start = {0,0},
    lists:map(fun char_of_dir/1,
              lists:reverse(bfs(queue:from_list([{Start,[]}]), #state{base=lists:reverse(Base), start=Start}))).

second(Base) ->
    Start = {0,0},
    longest(Start, [], 0, 0, #state{base=lists:reverse(Base), start=Start}).

-module(commands_test).
-include_lib("eunit/include/eunit.hrl").

quit_test() ->
    Res = commands:parse(<<"quit">>),
    ?assertMatch(quit, Res).

say_test() ->
    Res = commands:parse(<<"say some thing">>),
    ?assertMatch({say, _}, Res).

look_test() ->
    Res = commands:parse(<<"look">>),
    ?assertMatch(look, Res).

look_at_test() ->
    Res = commands:parse(<<"look target">>),
    ?assertMatch({look, {at, _}}, Res).

look_at2_test() ->
    Res = commands:parse(<<"look at target">>),
    ?assertMatch({look, {at, _}}, Res).

look_in_test() ->
    Res = commands:parse(<<"look in target">>),
    ?assertMatch({look, {in, _}}, Res).

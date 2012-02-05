-module(telnet_test).
-include_lib("eunit/include/eunit.hrl").

telnet_string_test() ->
    Res = telnet:decode(<<"abc">>),
    ?assertMatch({ok, {text, <<"abc">>}, <<>>}, Res).

double_iac_test() ->
    Res = telnet:decode(<<"ab\xff\xffcd">>),
    ?assertMatch({ok, {text, <<"ab\xffcd">>}, <<>>}, Res).

wont_delimiter_test() ->
    Res = telnet:decode(<<"ab\xff\xfccbc">>),
    ?assertMatch({ok, {text, <<"ab">>}, <<"\xff\xfccbc">>}, Res).

parse_wont_test() ->
    Res = telnet:decode(<<"\xff\xfccbc">>),
    ?assertMatch({ok, {wont, $c}, <<"bc">>}, Res).

command_test() ->
    Res = telnet:decode(<<"\xffabc">>),
    ?assertMatch({ok, {command, $a}, <<"bc">>}, Res).

command_delimiter_test() ->
    Res = telnet:decode(<<"abc\xffd">>),
    ?assertMatch({ok, {text, <<"abc">>}, <<"\xffd">>}, Res).

iac_not_finished_test() ->
    Res = telnet:decode(<<"abc\xff">>),
    ?assertMatch({more, <<"abc\xff">>}, Res).

sb_test() ->
    Res = telnet:decode(<<"\xff\xfaabc\xff\xf0abc">>),
    ?assertMatch({ok, {subnego, $a, <<"bc">>}, <<"abc">>}, Res).

sb_not_finished_test() ->
    Res = telnet:decode(<<"\xff\xfaabc">>),
    ?assertMatch({more, <<"\xff\xfaabc">>}, Res).

sb_not_finished2_test() ->
    Res = telnet:decode(<<"\xff\xfa">>),
    ?assertMatch({more, <<"\xff\xfa">>}, Res).

cr_test() ->
    Res = telnet:decode(<<"\x0d\x0aabc\x0d\x0adef">>),
    ?assertMatch({ok, {text, <<"abc">>}, <<"\x0adef">>}, Res).

cr2_test() ->
    Res = telnet:decode(<<"\x0dabc\x0ddef">>),
    ?assertMatch({ok, {text, <<"abc">>}, <<"def">>}, Res).

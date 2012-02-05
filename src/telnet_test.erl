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

encode_test() ->
    Res = telnet:encode([{text, <<"abc">>}, {text, <<"def">>}]),
    ?assertMatch({ok, <<"abc\r\ndef\r\n">>}, Res).

encode_wont_test() ->
    Res = telnet:encode([{text, <<"abc">>}, {wont, $c}]),
    ?assertMatch({ok, <<"abc\r\n\xff\xfcc">>}, Res).

encode_command_test() ->
    Res = telnet:encode([{text, <<"abc">>}, {command, $c}]),
    ?assertMatch({ok, <<"abc\r\n\xffc">>}, Res).

encode_text_iac_test() ->
    Res = telnet:encode([{text, <<"abc\xffdef">>}]),
    ?assertMatch({ok, <<"abc\xff\xffdef\r\n">>}, Res).

encode_sb_test() ->
    Res = telnet:encode([{subnego, $c, <<"abc">>}]),
    ?assertMatch({ok, <<"\xff\xfacabc\xff\xf0">>}, Res).

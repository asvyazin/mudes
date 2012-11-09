-module(mudes_telnet_options).
-author('Alexander Svyazin <guybrush@live.ru>').

-export([process/2]).

process({will, W}, ConnPid) ->
    mudes_connection:send(ConnPid, [{dont, W}]),
    ok;
process({do, D}, ConnPid) ->
    mudes_connection:send(ConnPid, [{wont, D}]),
    ok;
process(_, _ConnPid) ->
    skip.

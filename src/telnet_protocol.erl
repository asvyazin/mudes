-module(telnet_protocol).
-author('Alexander Svyazin <guybrush@live.ru>').
-behaviour(ranch_protocol).

-export([start_link/4]).

-include("telnet.hrl").

-spec start_link(pid(), inet:socket(), module(), any()) -> {ok, pid()}.
start_link(ListenerPid, Socket, Transport, Opts) ->
    {ok, ConnPid} = mudes_connection:start_link(ListenerPid, Socket, Transport, Opts),
    mudes_events:notify(new_connection, {new_connection, ConnPid}),
    {ok, ConnPid}.

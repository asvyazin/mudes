-module(telnet_protocol).
-author('Alexander Svyazin <guybrush@live.ru>').
-behaviour(ranch_protocol).

-export([start_link/4]).
-export([init/4]).

-include("telnet.hrl").

-spec start_link(pid(), inet:socket(), module(), any()) -> {ok, pid()}.
start_link(ListenerPid, Socket, Transport, Opts) ->
    mudes_connection:start_link(ListenerPid, Socket, Transport, Opts).

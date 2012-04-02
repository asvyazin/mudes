-module(telnet_protocol).
-behaviour(cowboy_protocol).

-export([start_link/4]). %% API
-export([init/4]).

-include("telnet.hrl").

-spec start_link(pid(), inet:socket(), module(), any()) -> {ok, pid()}.
start_link(ListenerPid, Socket, Transport, Opts) ->
    Pid = spawn_link(?MODULE, init, [ListenerPid, Socket, Transport, Opts]),
    {ok, Pid}.

%% @private
-spec init(pid(), inet:socket(), module(), any()) -> ok.
init(ListenerPid, Socket, Transport, _Opts) ->
    ok = cowboy:accept_ack(ListenerPid),
    {ok, ConnPid} = mudes_connection:start_link(Socket, Transport),
    ok = Transport:controlling_process(Socket, ConnPid),
    read_name(ConnPid).

read_name(ConnPid) ->
    mudes_connection:send_text(ConnPid, <<"What is your name?">>),
    {text, Name} = mudes_connection:next_token(ConnPid),
    io:format("name entered: ~p~n", [Name]),
    mudes_connection:send_text(ConnPid, <<"Enter password:">>),
    mudes_connection:send_will(ConnPid, ?ECHO),
    {do, ?ECHO} = mudes_connection:next_token(ConnPid),
    {text, Password} = mudes_connection:next_token(ConnPid),
    mudes_connection:send_wont(ConnPid, ?ECHO),
    {dont, ?ECHO} = mudes_connection:next_token(ConnPid),
    io:format("password entered: ~p~n", [Password]),
    mudes_connection:close(ConnPid).

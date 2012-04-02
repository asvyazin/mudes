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
    {ok, ConnPid} = mudes_connection:start_link(Socket, Transport, self()),
    ok = Transport:controlling_process(Socket, ConnPid),
    mudes_connection:listen(ConnPid),
    read_name(ConnPid).

read_name(ConnPid) ->
    mudes_connection:send_text(ConnPid, <<"What is your name?">>),
    receive
	{token, {text, Name}} ->
	    io:format("name entered: ~p~n", [Name])
    end,
    mudes_connection:send_text(ConnPid, <<"Enter password:">>),
    mudes_connection:send_will(ConnPid, ?ECHO),
    receive
	{token, {text, Password}} ->
	    io:format("password entered: ~p~n", [Password])
    end,
    mudes_connection:send_wont(ConnPid, ?ECHO),
    mudes_connection:close(ConnPid).

-module(telnet_protocol).
-behaviour(cowboy_protocol).

-export([start_link/4]). %% API
-export([init/4]).

-include("telnet.hrl").

-record(state,
	{
	  transport :: module(),
	  socket :: inet:socket(),
	  buffer = <<>> :: binary()
	}).

-spec start_link(pid(), inet:socket(), module(), any()) -> {ok, pid()}.
start_link(ListenerPid, Socket, Transport, Opts) ->
    Pid = spawn_link(?MODULE, init, [ListenerPid, Socket, Transport, Opts]),
    {ok, Pid}.

%% @private
-spec init(pid(), inet:socket(), module(), any()) -> ok.
init(ListenerPid, Socket, Transport, _Opts) ->
    ok = cowboy:accept_ack(ListenerPid),
    read_name(Socket, Transport).

read_name(Socket, Transport) ->
    ok = send_text(Socket, Transport, <<"What is your name?">>),
    {ok, {text, Name}, Buffer} = next_token(Socket, Transport, <<>>),
    io:format("name entered: ~p~n", [Name]),
    ok = send_text(Socket, Transport, <<"Enter password:">>),
    {ok, {text, Password}, _Buffer2} = next_token(Socket, Transport, Buffer),
    io:format("password entered: ~p~n", [Password]).

send_text(Socket, Transport, Text) ->
    {ok, Buffer} = telnet:encode([{text, Text}]),
    Transport:send(Socket, Buffer).

next_token(Socket, Transport, Buffer) ->
    case telnet:decode(Buffer) of
	{more, Rest} ->
	    {ok, Data} = Transport:recv(Socket, 0, infinity),
	    next_token(Socket, Transport, <<Rest/binary, Data/binary>>);
	{ok, Token, Rest} ->
	    {ok, Token, Rest}
    end.

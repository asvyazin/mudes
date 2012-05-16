-module(telnet_protocol).
-author('Alexander Svyazin <guybrush@live.ru>').
-behaviour(cowboy_protocol).

-export([start_link/4, send_text/2, close/1, send_tokens/2]). %% API
-export([init/4]).

-include("telnet.hrl").
-include("mudes.hrl").

-spec start_link(pid(), inet:socket(), module(), any()) -> {ok, pid()}.
start_link(ListenerPid, Socket, Transport, Opts) ->
    Pid = spawn_link(?MODULE, init, [ListenerPid, Socket, Transport, Opts]),
    {ok, Pid}.

send_text(Pid, Text) ->
    Data = telnet:encode([{text, Text}]),
    Pid ! {send, Data}.

close(Pid) ->
    Pid ! close.

send_tokens(Pid, Tokens) ->
    Pid ! {send, telnet:encode(Tokens)}.

-spec init(pid(), inet:socket(), module(), any()) -> ok.
init(ListenerPid, Socket, Transport, _Opts) ->
    ok = cowboy:accept_ack(ListenerPid),
    {ok, ConnSupRef} = supervisor:start_child(mudes_connections_sup, []),
    {ok, ConnPid} = mudes_connection_fsm:start(self(), ConnSupRef),
    loop(ConnPid, Transport, Socket, <<>>).

loop(ConnPid, Transport, Socket, Buffer) ->
    ok = Transport:setopts(Socket, [{active, once}]),
    receive
	{tcp, _Socket, Data} ->
	    decode_buffer(ConnPid, Transport, Socket, <<Buffer/binary, Data/binary>>);
	{send, Data} ->
	    Transport:send(Socket, Data),
	    loop(ConnPid, Transport, Socket, Buffer);
	close ->
	    Transport:close(Socket)
    end.

decode_buffer(ConnPid, Transport, Socket, Buffer) ->
    case telnet:decode(Buffer) of
	{more, Rest} ->
	    loop(ConnPid, Transport, Socket, Rest);
	{ok, Token, Rest} ->
	    mudes_connection_fsm:token(ConnPid, Token),
	    decode_buffer(ConnPid, Transport, Socket, Rest)
    end.

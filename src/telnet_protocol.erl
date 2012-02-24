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
    loop(#state{transport = Transport, socket = Socket}).

loop(State = #state{socket = Socket, transport = Transport, buffer = Buffer}) ->
    {ok, Data} = Transport:recv(Socket, 0, infinity),
    {ok, Rest} = process(Socket, Transport, <<Buffer/binary, Data/binary>>),
    loop(State#state{buffer = Rest}).

process(Socket, Transport, Buffer) ->
    Res = telnet:decode(Buffer),
    process_telnet(Socket, Transport, Res).

process_telnet(_Socket, _Transport, {more, Rest}) ->
    {ok, Rest};
process_telnet(Socket, Transport, {ok, {Cmd, C}, Rest})
  when Cmd == do; Cmd == dont ->
    io:format("received cmd ~p, ~p~n", [Cmd, C]),
    {ok, SendData} = telnet:encode([{wont, C}]),
    Transport:send(Socket, SendData),
    process(Socket, Transport, Rest);
process_telnet(Socket, Transport, {ok, {Cmd, C}, Rest})
  when Cmd == will; Cmd == wont ->
    io:format("received cmd ~p, ~p~n", [Cmd, C]),
    {ok, SendData} = telnet:encode([{dont, C}]),
    Transport:send(Socket, SendData),
    process(Socket, Transport, Rest);
process_telnet(Socket, Transport, {ok, {text, _} = Text, Rest}) ->
    io:format("received: ~p~n", [Text]),
    {ok, SendData} = telnet:encode([Text]),
    Transport:send(Socket, SendData),
    process(Socket, Transport, Rest);
process_telnet(Socket, Transport, {ok, {command, ?IP}, _Rest}) ->
    Transport:close(Socket),
    {error, connection_closed};
process_telnet(Socket, Transport, {ok, {command, C}, Rest}) ->
    io:format("command received: ~p~n", [C]),
    process(Socket, Transport, Rest).

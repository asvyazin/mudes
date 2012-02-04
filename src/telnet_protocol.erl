-module(telnet_protocol).
-behaviour(cowboy_protocol).

-export([start_link/4]). %% API

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
init(ListenerPid, Socket, Transport, Opts) ->
    ok = cowboy:accept_ack(ListenerPid),
    loop(#state{transport = Transport, socket = Socket}).

loop(State = #state{socket = Socket, transport = Transport}) ->
    {ok, Data} = Transport:recv(Socket, 0),
    Transport:send(Socket, Data).

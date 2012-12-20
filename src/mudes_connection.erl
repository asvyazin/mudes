-module(mudes_connection).
-author('Alexander Svyazin <guybrush@live.ru>').
-behaviour(gen_server).

%% API
-export([start_link/4, send/2, send_text/2, set_handler/3, quit/1]).

%% gen_server
-export([init/1, handle_info/2, handle_cast/2, terminate/2]).

-record(state, {listener_pid, socket, transport, handler, buffer}).

start_link(ListenerPid, Socket, Transport, Opts) ->
    gen_server:start_link(?MODULE, [ListenerPid, Socket, Transport, Opts], []).

send(Pid, Data) ->
    gen_server:cast(Pid, {send, Data}).

send_text(Pid, Text) ->
    send(Pid, {text, Text}).

quit(Pid) ->
    gen_server:cast(Pid, quit).

set_handler(ConnPid, Module, Args) ->
    gen_server:cast(ConnPid, {set_handler, Module, Args}).

init([ListenerPid, Socket, Transport, _Opts]) ->
    {ok, #state{listener_pid = ListenerPid, socket = Socket, transport = Transport, buffer = <<>>}}.

set_active(Socket, Transport) ->
    Transport:setopts(Socket, [{active, once}]).

handle_cast(quit, State) ->
    {stop, normal, State};
handle_cast({send, Data}, State = #state{socket = Socket, transport = Transport}) ->
    ok = Transport:send(Socket, telnet:encode(Data)),
    {noreply, State};
handle_cast({set_handler, Module, Args}, State = #state{}) ->
    {ok, HandlerPid} = gen_server:start_link(Module, Args, []),
    {noreply, State#state{handler = HandlerPid}}.

handle_info({shoot, ListenerPid}, State = #state{listener_pid = ListenerPid, socket = Socket, transport = Transport}) ->
    ok = set_active(Socket, Transport),
    {noreply, State};
handle_info({tcp, _Socket, Data}, State = #state{handler = HandlerPid, buffer = Buffer, socket = Socket, transport = Transport}) ->
    {ok, Rest} = decode_buffer(<<Buffer/binary, Data/binary>>, HandlerPid),
    ok = set_active(Socket, Transport),
    {noreply, State#state{buffer = Rest}}.
    
decode_buffer(Buffer, HandlerPid) ->
    case telnet:decode(Buffer) of
	{ok, Input, Rest} ->
	    case mudes_telnet_options:process(Input, self()) of
		skip ->
		    mudes_handler:input(HandlerPid, Input),
		    decode_buffer(Rest, HandlerPid);
		ok ->
		    decode_buffer(Rest, HandlerPid)
	    end;
	{more, Rest} ->
	    {ok, Rest}
    end.

terminate(Reason, _State) ->
    mudes_events:notify(connection_closed, {connection_closed, self(), Reason}),
    ok.

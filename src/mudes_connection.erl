-module(mudes_connection).
-author('Alexander Svyazin <guybrush@live.ru>').
-behaviour(gen_server).

%% API
-export([start_link/4, send/2, send_text/2, set_handler/2, quit/1]).

%% gen_server
-export([init/1, handle_info/2, handle_cast/2]).

-record(state, {listener_pid, socket, transport, handler, buffer}).

start_link(ListenerPid, Socket, Transport, Opts) ->
    gen_server:start_link(?MODULE, [ListenerPid, Socket, Transport, Opts], []).

send(Pid, Data) ->
    gen_server:cast(Pid, {send, Data}).

send_text(Pid, Text) ->
    send(Pid, {text, Text}).

set_handler(Pid, HandlerPid) ->
    gen_server:cast(Pid, {set_handler, HandlerPid}).

quit(Pid) ->
    gen_server:cast(Pid, quit).

init([ListenerPid, Socket, Transport, _Opts]) ->
    {ok, HandlerPid} = mudes_login_handler:start_link(self()),
    {ok, #state{listener_pid = ListenerPid, socket = Socket, transport = Transport, handler = HandlerPid, buffer = <<>>}}.

set_active(Socket, Transport) ->
    Transport:setopts(Socket, [{active, once}]).

handle_cast(quit, State) ->
    {stop, normal, State};
handle_cast({send, Data}, State = #state{socket = Socket, transport = Transport}) ->
    ok = Transport:send(Socket, telnet:encode(Data)),
    {noreply, State};
handle_cast({set_handler, HandlerPid}, State) ->
    {noreply, State#state{handler = HandlerPid}}.

handle_info({shoot, ListenerPid}, State = #state{listener_pid = ListenerPid, socket = Socket, transport = Transport}) ->
    ok = set_active(Socket, Transport),
    {noreply, State};
handle_info({tcp, _Socket, Data}, State = #state{handler = HandlerPid, buffer = Buffer}) ->
    {ok, Rest} = decode_buffer(<<Buffer/binary, Data/binary>>, HandlerPid),
    {noreply, State#state{buffer = Rest}}.
    
decode_buffer(Buffer, HandlerPid) ->
    case telnet:decode(Buffer) of
	{ok, Input, Rest} ->
	    mudes_handler:input(HandlerPid, Input),
	    decode_buffer(Rest, HandlerPid);
	{more, Rest} ->
	    {ok, Rest}
    end.

-module(mudes_connection).
-author('Alexander Svyazin <guybrush@live.ru>').
-behaviour(gen_server).

%% API
-export([send/2, set_handler/2, quit/1]).

%% gen_server
-export([init/1, handle_info/2, handle_cast/2]).

-record(state, {listener, socket, transport, handler}).

start_link(ListenerPid, Socket, Transport, Opts) ->
    gen_server:start_link(?MODULE, [ListenerPid, Socket, Transport, Opts], []).

send(Pid, Data) ->
    gen_server:cast(Pid, {send, Data}).

set_handler(Pid, HandlerPid) ->
    gen_server:cast(Pid, {set_handler, HandlerPid}).

quit(Pid) ->
    gen_server:cast(Pid, quit).

init([ListenerPid, Socket, Transport, _Opts]) ->
    ok = ranch:accept_ack(ListenerPid),
    ok = set_active(Socket, Transport),
    {ok, HandlerPid} = mudes_login_handler:start_link(self()),
    {ok, #state{listener = ListenerPid, socket = Socket, transport = Transport, handler = HandlerPid}}.

set_active(Socket, Transport) ->
    Transport:setopts(Socket, [{active, once}]).

handle_cast(quit, State) ->
    {stop, normal, State};
handle_cast({send, Data}, State = #state{socket = Socket, transport = Transport}) ->
    ok = Transport:send(Socket, telnet:encode(Data)),
    {ok, State};
handle_cast({set_handler, HandlerPid}, State) ->
    {ok, State#state{handler = HandlerPid}}.

handle_info({tcp, _Socket, Data}, State = #state{handler = HandlerPid}) ->
    case telnet:decode(Buffer) of
	{more, Rest} ->
	    loop(ConnPid, Transport, Socket, Rest);
	{ok, Token, Rest} ->
	    mudes_connection_fsm:token(ConnPid, Token),
	    decode_buffer(ConnPid, Transport, Socket, Rest)
    end.
    

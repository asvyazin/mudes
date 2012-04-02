-module(mudes_connection).
-behaviour(gen_server).

-export([start_link/2, next_token/1, send_text/2, send_dont/2, send_do/2, send_will/2, send_wont/2, close/1]).
-export([init/1, handle_call/3, handle_cast/2, terminate/2]).

-record(state,
	{
	  transport :: module(),
	  socket :: inet:socket(),
	  buffer = <<>> :: binary()
	}).

start_link(Socket, Transport) ->
    gen_server:start_link(?MODULE, [Socket, Transport], []).

init([Socket, Transport]) ->
    {ok, #state{transport = Transport, socket = Socket}}.

next_token(Pid) ->
    gen_server:call(Pid, next_token).

send_text(Pid, Text) ->
    gen_server:cast(Pid, {send_text, Text}).

send_dont(Pid, Cmd) ->
    gen_server:cast(Pid, {send_dont, Cmd}).

send_do(Pid, Cmd) ->
    gen_server:cast(Pid, {send_do, Cmd}).

send_will(Pid, Cmd) ->
    gen_server:cast(Pid, {send_will, Cmd}).

send_wont(Pid, Cmd) ->
    gen_server:cast(Pid, {send_wont, Cmd}).

close(Pid) ->
    gen_server:cast(Pid, close).

handle_call(next_token, _From, State = #state{transport = Transport,
					      socket = Socket,
					      buffer = Buffer}) ->
    {ok, Token, Rest} = next_token(Socket, Transport, Buffer),
    {reply, Token, State#state{buffer = Rest}}.

next_token(Socket, Transport, Buffer) ->
    case telnet:decode(Buffer) of
	{more, Rest} ->
	    {ok, Data} = Transport:recv(Socket, 0, infinity),
	    next_token(Socket, Transport, <<Rest/binary, Data/binary>>);
	{ok, Token, Rest} ->
	    {ok, Token, Rest}
    end.

handle_cast({send_text, Text}, State = #state{transport = Transport,
					      socket = Socket}) ->
    ok = encode_and_send(Socket, Transport, [{text, Text}]),
    {noreply, State};
handle_cast({send_dont, Cmd}, State = #state{transport = Transport,
					     socket = Socket}) ->
    ok = encode_and_send(Socket, Transport, [{dont, Cmd}]),
    {noreply, State};
handle_cast({send_do, Cmd}, State = #state{transport = Transport,
					   socket = Socket}) ->
    ok = encode_and_send(Socket, Transport, [{do, Cmd}]),
    {noreply, State};
handle_cast({send_will, Cmd}, State = #state{transport = Transport,
					     socket = Socket}) ->
    ok = encode_and_send(Socket, Transport, [{will, Cmd}]),
    {noreply, State};
handle_cast({send_wont, Cmd}, State = #state{transport = Transport,
					     socket = Socket}) ->
    ok = encode_and_send(Socket, Transport, [{wont, Cmd}]),
    {noreply, State};
handle_cast(close, State = #state{transport = Transport,
				  socket = Socket}) ->
    ok = Transport:close(Socket),
    {noreply, State}.

encode_and_send(Socket, Transport, ToEncode) ->
    {ok, Send} = telnet:encode(ToEncode),
    Transport:send(Socket, Send).

terminate(normal, _State) ->
    ok.

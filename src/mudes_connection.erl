-module(mudes_connection).
-author('Alexander Svyazin <guybrush@live.ru>').
-behaviour(gen_server).

-export([start_link/3, start/4, send_text/2, send_dont/2, send_do/2, send_will/2, send_wont/2, send_tokens/2, close/1, listen/1]).
-export([init/1, handle_cast/2, terminate/2, handle_info/2]).

-record(state,
	{
	  transport :: module(),
	  socket :: inet:socket(),
	  buffer = <<>> :: binary(),
	  parent :: pid()
	}).

start(Socket, Transport, Parent, SupRef) ->
    ChildSpec = {{mudes_connection, SupRef},
		 {mudes_connection, start_link, [Socket, Transport, Parent]},
		 temporary,
		 5000,
		 worker,
		 [mudes_connection]},
    supervisor:start_child(SupRef, ChildSpec).

start_link(Socket, Transport, Parent) ->
    gen_server:start_link(?MODULE, [Socket, Transport, Parent], []).

init([Socket, Transport, Parent]) ->
    {ok, #state{transport = Transport, socket = Socket, parent = Parent}}.

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

send_tokens(Pid, Tokens) ->
    gen_server:cast(Pid, {send_tokens, Tokens}).

close(Pid) ->
    gen_server:cast(Pid, close).

listen(Pid) ->
    gen_server:cast(Pid, listen).

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
handle_cast({send_tokens, Tokens}, State = #state{transport = Transport,
						  socket = Socket}) ->
    ok = encode_and_send(Socket, Transport, Tokens),
    {noreply, State};
handle_cast(close, State = #state{transport = Transport,
				  socket = Socket}) ->
    ok = Transport:close(Socket),
    {stop, normal, State};
handle_cast(listen, State = #state{transport = Transport,
				   socket = Socket}) ->
    ok = Transport:setopts(Socket, [{active, once}]),
    {noreply, State}.

encode_and_send(Socket, Transport, ToEncode) ->
    Send = telnet:encode(ToEncode),
    Transport:send(Socket, Send).

terminate(normal, _State) ->
    ok.

handle_info({tcp, _Socket, Data}, State = #state{buffer = Buffer}) ->
    decode_buffer(State#state{buffer = <<Buffer/binary, Data/binary>>}).

decode_buffer(State = #state{buffer = Buffer, parent = Parent,
			     transport = Transport, socket = Socket}) ->
    case telnet:decode(Buffer) of
	{more, Rest} ->
	    Transport:setopts(Socket, [{active, once}]),
	    {noreply, State#state{buffer = Rest}};
	{ok, Token, Rest} ->
	    Parent ! {token, Token},
	    decode_buffer(State#state{buffer = Rest})
    end.

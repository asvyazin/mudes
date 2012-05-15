-module(mudes_connection).
-author('Alexander Svyazin <guybrush@live.ru>').
-behaviour(gen_server).

-export([start_link/2, start/2, send_text/2, send_dont/2, send_do/2, send_will/2,
	 send_wont/2, send_tokens/2, close/1, token/2]).
-export([init/1, handle_cast/2, terminate/2]).

-record(state, {parent, fsm_pid}).

start(Parent, SupRef) ->
    ChildSpec = {{mudes_connection, SupRef},
		 {mudes_connection, start_link, [Parent, SupRef]},
		 temporary,
		 5000,
		 worker,
		 [mudes_connection]},
    supervisor:start_child(SupRef, ChildSpec).

start_link(Parent, SupRef) ->
    gen_server:start_link(?MODULE, [Parent, SupRef], []).

init([Parent, SupRef]) ->
    {ok, Fsm} = mudes_connection_fsm:start(self(), SupRef),
    {ok, #state{parent = Parent, fsm_pid = Fsm}}.

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

token(Pid, Token) ->
    gen_server:cast(Pid, {token, Token}).

handle_cast({send_text, Text}, State = #state{parent=Parent}) ->
    ok = encode_and_send(Parent, [{text, Text}]),
    {noreply, State};
handle_cast({send_dont, Cmd}, State = #state{parent=Parent}) ->
    ok = encode_and_send(Parent, [{dont, Cmd}]),
    {noreply, State};
handle_cast({send_do, Cmd}, State = #state{parent=Parent}) ->
    ok = encode_and_send(Parent, [{do, Cmd}]),
    {noreply, State};
handle_cast({send_will, Cmd}, State = #state{parent=Parent}) ->
    ok = encode_and_send(Parent, [{will, Cmd}]),
    {noreply, State};
handle_cast({send_wont, Cmd}, State = #state{parent=Parent}) ->
    ok = encode_and_send(Parent, [{wont, Cmd}]),
    {noreply, State};
handle_cast({send_tokens, Tokens}, State = #state{parent=Parent}) ->
    ok = encode_and_send(Parent, Tokens),
    {noreply, State};
handle_cast({token, Token}, State = #state{fsm_pid=Fsm}) ->
    mudes_connection_fsm:token(Fsm, Token),
    {noreply, State};
handle_cast(close, State = #state{parent=Parent}) ->
    Parent ! close,
    {stop, normal, State}.

encode_and_send(Parent, ToEncode) ->
    Send = telnet:encode(ToEncode),
    Parent ! {send,  Send}.

terminate(normal, _State) ->
    ok.

-module(mudes_connection_fsm).
-author('Alexander Svyazin <guybrush@live.ru>').

-behaviour(gen_fsm).

%% API
-export([start_link/1, start/2, token/2]).

%% gen_fsm callbacks
-export([init/1, connected/2, wait_password_existing/2, wait_password_new/2,
	 wait_password_new2/2, authenticated/2, terminate/3]).

-record(state, {protocol, name, password_hash}).

start(ProtocolPid, SupRef) ->
    ChildSpec = {{?MODULE, SupRef},
		 {?MODULE, start_link, [ProtocolPid]},
		 temporary,
		 5000,
		 worker,
		 [?MODULE]},
    supervisor:start_child(SupRef, ChildSpec).

start_link(ProtocolPid) ->
    gen_fsm:start_link(?MODULE, [ProtocolPid], []).

init([ProtocolPid]) ->
    telnet_protocol:send_text(ProtocolPid, <<"What is your name?">>),
    {ok, connected, #state{protocol = ProtocolPid}}.

terminate(normal, _StateName, _State) ->
    ok.

token(Pid, {text, Text}) ->
    text(Pid, Text);
token(Pid, {will, Cmd}) ->
    will(Pid, Cmd);
token(Pid, {wont, Cmd}) ->
    wont(Pid, Cmd);
token(Pid, {do, Cmd}) ->
    do(Pid, Cmd);
token(Pid, {dont, Cmd}) ->
    dont(Pid, Cmd).

text(Pid, Text) ->
    gen_fsm:send_event(Pid, {text, Text}).

will(Pid, Cmd) ->
    gen_fsm:send_all_state_event(Pid, {will, Cmd}).

wont(Pid, Cmd) ->
    gen_fsm:send_all_state_event(Pid, {wont, Cmd}).

do(Pid, Cmd) ->
    gen_fsm:send_all_state_event(Pid, {do, Cmd}).

dont(Pid, Cmd) ->
    gen_fsm:send_all_state_event(Pid, {dont, Cmd}).

connected({text, Name}, State) ->
    lager:debug("name entered: ~p", [Name]),
    case mudes_users_db:exists(Name) of
	true ->
	    existing_user(Name, State);
	false ->
	    new_user(Name, State)
    end.

existing_user(Name, State = #state{protocol = Protocol}) ->
    telnet_protocol:send_text(Protocol, <<"Enter your password:">>),
    {next_state, wait_password_existing, State#state{name = Name}}.

wait_password_existing({text, Password}, State = #state{protocol = Protocol, name = Name}) ->
    case mudes_users_db:check_password(Name, Password) of
	true ->
	    telnet_protocol:send_text(Protocol, <<"Welcome, ", Name/binary>>),
	    mudes_users:add_user(Name, Protocol),
	    {next_state, authenticated, State};
	false ->
	    telnet_protocol:send_text(Protocol, <<"Invalid password, bye!">>),
	    {stop, invalid_password, State}
    end.

new_user(Name, State = #state{protocol = Protocol}) ->
    telnet_protocol:send_text(Protocol, <<"Will register new user. Enter your password:">>),
    {next_state, wait_password_new, State#state{name = Name}}.

wait_password_new({text, Password}, State = #state{protocol = Protocol}) ->
    PasswordHash = crypto:md5(Password),
    telnet_protocol:send_text(Protocol, <<"Confirm password:">>),
    {next_state, wait_password_new2, State#state{password_hash = PasswordHash}}.

wait_password_new2({text, Password},
		   State = #state{protocol = Protocol,
				  name = Name,
				  password_hash = PasswordHash}) ->
    case crypto:md5(Password) of
	PasswordHash ->
	    telnet_protocol:send_text(Protocol, <<"Welcome, ", Name/binary>>),
	    mudes_users:add_user(Name, Protocol),
	    {next_state, authenticated, State};
	_ ->
	    telnet_protocol:send_text(Protocol, <<"Password does not match, bye!">>),
	    {stop, password_does_not_match, State}
    end.

authenticated({text, Text}, State) ->
    {ok, Cmd, Args} = parse_command(Text),
    process_command(Cmd, Args, State).

parse_command(Text) ->
    case binary:split(Text, <<" ">>) of
	[Cmd, Args] ->
	    {ok, Cmd, Args};
	[Cmd] ->
	    {ok, Cmd, <<>>}
    end.

process_command(<<"quit">>, _, State = #state{protocol = Protocol, name = Name})->
    telnet_protocol:send_text(Protocol, <<"Goodbye, ", Name/binary>>),
    telnet_protocol:close(Protocol),
    {stop, normal, State};
process_command(<<"who">>, _, State = #state{protocol = Protocol}) ->
    {ok, Users} = mudes_users:get_users(),
    Len = length(Users),
    LenBin = list_to_binary(integer_to_list(Len)),
    UsersTokens = [{text, U} || U <- Users],
    Tokens = [{text, <<"Currently online:">>}, UsersTokens,
	      {text, <<LenBin/binary, " users">>}],
    telnet_protocol:send_tokens(Protocol, Tokens),
    {next_state, authenticated, State};
process_command(<<"say">>, Say, State = #state{protocol = Protocol}) ->
    {ok, UserPids} = mudes_users:get_pids(),
    {ok, User} = mudes_users:get_user_by_pid(Protocol),
    do_say(Protocol, UserPids, Say, User),
    {next_state, authenticated, State};
process_command(Cmd, _, State = #state{protocol = Protocol}) ->
    telnet_protocol:send_text(Protocol, <<"Unknown command: ", Cmd/binary>>),
    {next_state, authenticated, State}.

do_say(Protocol, UserPids, Say, User) ->
    [do_say_to_user(Protocol, UserPid, Say, User) || UserPid <- UserPids],
    ok.

do_say_to_user(Protocol, Protocol, Say, _User) ->
    telnet_protocol:send_text(Protocol, <<"You say: ", Say/binary>>);
do_say_to_user(_Protocol, UserPid, Say, User) ->
    telnet_protocol:send_text(UserPid, <<User/binary, " says: ", Say/binary>>).

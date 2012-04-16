-module(telnet_protocol).
-behaviour(cowboy_protocol).

-export([start_link/4]). %% API
-export([init/4]).

-include("telnet.hrl").
-include("mudes.hrl").

-spec start_link(pid(), inet:socket(), module(), any()) -> {ok, pid()}.
start_link(ListenerPid, Socket, Transport, Opts) ->
    Pid = spawn_link(?MODULE, init, [ListenerPid, Socket, Transport, Opts]),
    {ok, Pid}.

%% @private
create_tables() ->
    mnesia:create_table(users, [{disc_copies, node()},
				{attributes, record_info(fields, users)}]).

ensure_loaded() ->
    ok = mnesia:wait_for_tables([users], 60000).

-spec init(pid(), inet:socket(), module(), any()) -> ok.
init(ListenerPid, Socket, Transport, _Opts) ->
    ok = cowboy:accept_ack(ListenerPid),
    create_tables(),
    ensure_loaded(),
    {ok, ConnPid} = mudes_connection:start_link(Socket, Transport, self()),
    ok = Transport:controlling_process(Socket, ConnPid),
    mudes_connection:listen(ConnPid),
    read_name(ConnPid).

receive_text() ->
    receive
	{token, {text, Text}} -> Text
    end.

read_name(ConnPid) ->
    mudes_connection:send_text(ConnPid, <<"What is your name?">>),
    Name = receive_text(),
    {atomic, Users} = mnesia:transaction(
			fun() -> mnesia:read({users, Name}) end),
    case Users of
	[] ->
	    new_user(ConnPid, Name);
	[#users{name = Name, password_hash = PasswordHash}] ->
	    existing_user(ConnPid, Name, PasswordHash)
    end.

existing_user(ConnPid, Name, PasswordHash) ->
    mudes_connection:send_tokens(ConnPid, [{text, <<"Enter your password:">>},
					   {will, ?ECHO}]),
    Password = receive_text(),
    mudes_connection:send_wont(ConnPid, ?ECHO),
    case crypto:sha(Password) of
	PasswordHash ->
	    mudes_connection:send_text(ConnPid, <<"Welcome, ", Name/binary>>),
	    start_main_loop(ConnPid, Name);
	_ ->
	    mudes_connection:send_text(ConnPid, <<"Invalid password entered... Goodbye!">>),
	    mudes_connection:close(ConnPid)
    end.

new_user(ConnPid, Name) ->
    mudes_connection:send_tokens(ConnPid, [{text, <<"Will register new user. Enter password:">>},
					   {will, ?ECHO}]),
    Password = receive_text(),
    mudes_connection:send_text(ConnPid, <<"Confirm password:">>),
    case receive_text() of
	Password ->
	    mudes_connection:send_wont(ConnPid, ?ECHO),
	    PasswordHash = crypto:sha(Password),
	    NewUser = #users{name = Name, password_hash = PasswordHash},
	    {atomic, ok} = mnesia:transaction(fun() -> mnesia:write(NewUser) end),
	    mudes_connection:send_text(ConnPid, <<"User registered. Welcome, ", Name/binary>>),
	    start_main_loop(ConnPid, Name);
	_ ->
	    mudes_connection:send_text(ConnPid, <<"Password does not match... Goodbye!">>),
	    mudes_connection:close(ConnPid)
    end.

start_main_loop(ConnPid, Name) ->
    mudes_users:add_user(Name, ConnPid),
    main_loop(ConnPid).

main_loop(ConnPid) ->
    Text = receive_text(),
    case process_command(ConnPid, Text) of
	terminate ->
	    mudes_connection:close(ConnPid);
	ok ->
	    main_loop(ConnPid)
    end.

process_command(ConnPid, Text) ->
    {ok, Cmd, Args} = parse_command(Text),
    do_command(ConnPid, Cmd, Args).

parse_command(Text) ->
    case binary:split(Text, <<" ">>) of
	[Cmd, Args] ->
	    {ok, Cmd, Args};
	[Cmd] ->
	    {ok, Cmd, <<>>}
    end.

do_command(_ConnPid, <<"quit">>, _Args) ->
    terminate;
do_command(ConnPid, <<"who">>, _Args) ->
    {ok, Users} = mudes_users:get_users(),
    Len = length(Users),
    LenBin = list_to_binary(integer_to_list(Len)),
    UsersTokens = [{text, U} || U <- Users],
    Tokens = [{text, <<"Currently online:">>}, UsersTokens, 
	      {text, <<LenBin/binary, " users">>}],
    mudes_connection:send_tokens(ConnPid, Tokens);
do_command(ConnPid, <<"say">>, Args) ->
    {ok, Pids} = mudes_users:get_pids(),
    {ok, User} = mudes_users:get_user_by_pid(ConnPid),
    do_say(ConnPid, Args, Pids, User);
do_command(ConnPid, Cmd, _Args) ->
    mudes_connection:send_text(ConnPid, <<"Unknown command: ", Cmd/binary>>).

do_say(ConnPid, Say, UserPids, User) ->
    [do_say_to_user(ConnPid, UserPid, Say, User) || UserPid <- UserPids],
    ok.

do_say_to_user(ConnPid, ConnPid, Say, _User) ->
    mudes_connection:send_text(ConnPid, <<"You say: ", Say/binary>>);
do_say_to_user(_ConnPid, UserPid, Say, User) ->
    mudes_connection:send_text(UserPid, <<User/binary, " says: ", Say/binary>>).

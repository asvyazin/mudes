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
    mudes_connection:send_text(ConnPid, <<"Enter your password:">>),
    mudes_connection:send_will(ConnPid, ?ECHO),
    Password = receive_text(),
    mudes_connection:send_wont(ConnPid, ?ECHO),
    case crypto:sha(Password) of
	PasswordHash ->
	    mudes_connection:send_text(ConnPid, <<"Welcome, ", Name/binary>>),
	    main_loop(ConnPid);
	_ ->
	    mudes_connection:send_text(ConnPid, <<"Invalid password entered... Goodbye!">>),
	    mudes_connection:close(ConnPid)
    end.

new_user(ConnPid, Name) ->
    mudes_connection:send_text(ConnPid, <<"Will register new user. Enter password:">>),
    mudes_connection:send_will(ConnPid, ?ECHO),
    Password = receive_text(),
    mudes_connection:send_text(ConnPid, <<"Confirm password:">>),
    case receive_text() of
	Password ->
	    mudes_connection:send_wont(ConnPid, ?ECHO),
	    PasswordHash = crypto:sha(Password),
	    NewUser = #users{name = Name, password_hash = PasswordHash},
	    {atomic, ok} = mnesia:transaction(fun() -> mnesia:write(NewUser) end),
	    mudes_connection:send_text(ConnPid, <<"User registered. Welcome, ", Name/binary>>),
	    main_loop(ConnPid);
	_ ->
	    mudes_connection:send_text(ConnPid, <<"Password does not match... Goodbye!">>),
	    mudes_connection:close(ConnPid)
    end.

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
    terminate.

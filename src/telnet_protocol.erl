-module(telnet_protocol).
-author('Alexander Svyazin <guybrush@live.ru>').
-behaviour(cowboy_protocol).

-export([start_link/4]). %% API
-export([init/4]).

-include("telnet.hrl").
-include("mudes.hrl").

-spec start_link(pid(), inet:socket(), module(), any()) -> {ok, pid()}.
start_link(ListenerPid, Socket, Transport, Opts) ->
    Pid = spawn_link(?MODULE, init, [ListenerPid, Socket, Transport, Opts]),
    {ok, Pid}.

-spec init(pid(), inet:socket(), module(), any()) -> ok.
init(ListenerPid, Socket, Transport, _Opts) ->
    ok = cowboy:accept_ack(ListenerPid),
    {ok, ConnSupRef} = supervisor:start_child(mudes_connections_sup, []),
    {ok, ConnPid} = mudes_connection:start(Socket, Transport, self(), ConnSupRef),
    ok = Transport:controlling_process(Socket, ConnPid),
    mudes_connection:listen(ConnPid),
    read_name(ConnPid, ConnSupRef).

receive_text() ->
    receive
	{token, {text, Text}} -> Text
    end.

read_name(ConnPid, ConnSupRef) ->
    mudes_connection:send_text(ConnPid, <<"What is your name?">>),
    Name = receive_text(),
    lager:debug("login: ~p", [Name]),
    case mudes_users_db:exists(Name) of
	true ->
	    existing_user(ConnPid, Name, ConnSupRef);
	false ->
	    new_user(ConnPid, Name, ConnSupRef)
    end.

existing_user(ConnPid, Name, ConnSupRef) ->
    mudes_connection:send_tokens(ConnPid, [{text, <<"Enter your password:">>},
					   {will, ?ECHO}]),
    Password = receive_text(),
    mudes_connection:send_wont(ConnPid, ?ECHO),
    case mudes_users_db:check_password(Name, Password) of
	true ->
	    mudes_connection:send_text(ConnPid, <<"Welcome, ", Name/binary>>),
	    start_main_loop(ConnPid, Name, ConnSupRef);
	false ->
	    mudes_connection:send_text(ConnPid, <<"Invalid password entered... Goodbye!">>),
	    mudes_connection:close(ConnPid)
    end.

new_user(ConnPid, Name, ConnSupRef) ->
    mudes_connection:send_tokens(ConnPid, [{text, <<"Will register new user. Enter password:">>},
					   {will, ?ECHO}]),
    Password = receive_text(),
    mudes_connection:send_text(ConnPid, <<"Confirm password:">>),
    case receive_text() of
	Password ->
	    mudes_connection:send_wont(ConnPid, ?ECHO),
	    ok = mudes_users_db:add(Name, Password),
	    mudes_connection:send_text(ConnPid, <<"User registered. Welcome, ", Name/binary>>),
	    start_main_loop(ConnPid, Name, ConnSupRef);
	_ ->
	    mudes_connection:send_text(ConnPid, <<"Password does not match... Goodbye!">>),
	    mudes_connection:close(ConnPid)
    end.

start_main_loop(ConnPid, Name, ConnSupRef) ->
    mudes_users:add_user(Name, ConnPid),
    {ok, Handler} = mudes_handler:start(ConnPid, ConnSupRef),
    main_loop(ConnPid, Handler).

main_loop(ConnPid, Handler) ->
    Text = receive_text(),
    case process_command(Handler, Text) of
	terminate ->
	    mudes_connection:close(ConnPid);
	ok ->
	    main_loop(ConnPid, Handler)
    end.

process_command(Handler, Text) ->
    {ok, Cmd, Args} = parse_command(Text),
    mudes_handler:process_command(Handler, Cmd, Args).

parse_command(Text) ->
    case binary:split(Text, <<" ">>) of
	[Cmd, Args] ->
	    {ok, Cmd, Args};
	[Cmd] ->
	    {ok, Cmd, <<>>}
    end.

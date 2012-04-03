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
-spec init(pid(), inet:socket(), module(), any()) -> ok.
init(ListenerPid, Socket, Transport, _Opts) ->
    ok = cowboy:accept_ack(ListenerPid),
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
    end,
    mudes_connection:close(ConnPid).

existing_user(ConnPid, _Name, PasswordHash) ->
    mudes_connection:send_text(ConnPid, <<"Enter your password:">>),
    mudes_connection:send_will(ConnPid, ?ECHO),
    Password = receive_text(),
    mudes_connection:send_wont(ConnPid, ?ECHO),
    case crypto:sha(Password) of
	PasswordHash ->
	    mudes_connection:send_text(ConnPid, <<"Good user">>);
	_ ->
	    mudes_connection:send_text(ConnPid, <<"Intruder">>)
    end.

new_user(ConnPid, Name) ->
    mudes_connection:send_text(ConnPid, <<"Will register new user. Enter password:">>),
    mudes_connection:send_will(ConnPid, ?ECHO),
    Password = receive_text(),
    mudes_connection:send_text(ConnPid, <<"Confirm password:">>),
    Password = receive_text(),
    mudes_connection:send_wont(ConnPid, ?ECHO),
    PasswordHash = crypto:sha(Password),
    NewUser = #users{name = Name, password_hash = PasswordHash},
    {atomic, ok} = mnesia:transaction(fun() -> mnesia:write(NewUser) end),
    mudes_connection:send_text(ConnPid, <<"User registered">>).

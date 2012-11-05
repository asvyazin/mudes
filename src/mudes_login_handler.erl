-module(mudes_login_handler).
-author('Alexander Svyazin <guybrush@live.ru>').
-behaviour(mudes_handler).
-behaviour(gen_server).

%% mudes_handler
-export([start_link/1]).

%% gen_server
-export([init/1, handle_cast/2]).

-record(state, {conn_pid, current_state, name, password_hash}).

start_link(ConnPid) ->
    gen_server:start_link(?MODULE, [ConnPid], []).

init([ConnPid]) ->
    mudes_connection:send_text(ConnPid, <<"What is your name?">>),
    {ok, #state{conn_pid = ConnPid, current_state = connected}}.

handle_cast({input, {text, Name}}, State = #state{conn_pid = ConnPid, current_state = connected}) ->
    lager:debug("name entered: ~p", [Name]),
    case mudes_users_db:exists(Name) of
	true ->
	    mudes_connection:send_text(ConnPid, <<"Enter your password:">>),
	    {ok, State#state{current_state = wait_password_existing, name = Name}};
	false ->
	    mudes_connection:send_text(ConnPid, <<"Will register new user. Enter your password:">>),
	    {ok, State#state{current_state = wait_password_new, name = Name}}
    end;
handle_cast({input, {text, Password}}, State = #state{conn_pid = ConnPid, current_state = wait_password_existing, name = Name}) ->
    case mudes_users_db:check_password(Name, Password) of
	true ->
	    mudes_connection:send_text(ConnPid, <<"Welcome, ", Name/binary>>),
	    mudes_users:add_user(Name, ConnPid),
	    authenticated(ConnPid),
	    {stop, normal, State};
	false ->
	    mudes_connection:send_text(ConnPid, <<"Invalid password, bye!">>),
	    {stop, invalid_password, State}
    end;
handle_cast({input, {text, Password}}, State = #state{conn_pid = ConnPid, current_state = wait_password_new, name = Name}) ->
    PasswordHash = crypto:sha(Password),
    mudes_connection:send_text(ConnPid, <<"Confirm password:">>),
    {ok, State#state{current_state = wait_password_new2, password_hash = PasswordHash}};
handle_cast({input, {text, Password}}, State = #state{conn_pid = ConnPid, current_state = wait_password_new2, name = Name, password_hash = PasswordHash}) ->
    case crypto:sha(Password) of
	PasswordHash ->
	    mudes_users_db:add(Name, Password),
	    mudes_connection:send_text(ConnPid, <<"Welcome, ", Name/binary>>),
	    mudes_users:add_user(Name, ConnPid),
	    authenticated(ConnPid);
	_ ->
	    mudes_connection:send_text(ConnPid, <<"Password does not match, bye!">>),
	    {stop, password_does_not_match, State}
    end.

authenticated(ConnPid) ->
    {ok, CommandHandler} = mudes_command_handler:start_link(ConnPid),
    mudes_connection:set_handler(ConnPid, CommandHandler).    

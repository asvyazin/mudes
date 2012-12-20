-module(mudes_basic).
-author('Alexander Svyazin <guybrush@live.ru>').
-behaviour(gen_server).

-export([start_link/0]).
-export([init/1, handle_info/2, terminate/2]).

-record(state, {}).

-define(EVENTS_TO_SUBSCRIBE, [new_connection, user_authenticated, user_authentication_failed]).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    [ mudes_events:subscribe(E) || E <- ?EVENTS_TO_SUBSCRIBE ],
    {ok, #state{}}.

handle_info({new_connection, ConnPid}, State) ->
    mudes_connection:set_handler(ConnPid, mudes_login_handler, [ConnPid]),
    {noreply, State};
handle_info({user_authenticated, Login, ConnPid}, State) ->
    mudes_connection:send_text(ConnPid, [<<"Welcome, ">>, Login]),
    mudes_users:add_user(Login, ConnPid),
    mudes_connection:set_handler(ConnPid, mudes_command_handler, [ConnPid]),
    {noreply, State};
handle_info({user_authentication_failed, _Login, ConnPid, Error}, State) ->
    ErrorText = case Error of
		    invalid_password ->
			<<"Invalid password, bye!">>;
		    password_does_not_match ->
			<<"Password do not match, bye!">>
		end,
    mudes_connection:send_text(ConnPid, ErrorText),
    mudes_connection:close(ConnPid),
    {noreply, State}.

terminate(normal, _State) ->
    [ mudes_events:unsubscribe(E) || E <- ?EVENTS_TO_SUBSCRIBE ].    

-module(mudes_command_handler).
-author('Alexander Svyazin <guybrush@live.ru>').
-behaviour(mudes_handler).
-behaviour(gen_server).

%% mudes_handler
-export([start_link/1]).

%% gen_server
-export([init/1, handle_cast/2, handle_call/3]).

-record(state, {conn_pid}).

start_link(ConnPid) ->
    gen_server:start_link(?MODULE, [ConnPid], []).

init([ConnPid]) ->
    {ok, #state{conn_pid = ConnPid}}.

handle_cast({input, {text, Text}}, State = #state{conn_pid = ConnPid}) ->
    mudes_connection:send_text(ConnPid, <<"What do you mean: ", Text/binary>>),
    {ok, State}.

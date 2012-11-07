-module(mudes_command_handler).
-author('Alexander Svyazin <guybrush@live.ru>').
-behaviour(mudes_handler).
-behaviour(gen_server).

%% mudes_handler
-export([start_link/1]).

%% gen_server
-export([init/1, handle_cast/2]).

-record(state, {conn_pid}).

start_link(ConnPid) ->
    gen_server:start_link(?MODULE, [ConnPid], []).

init([ConnPid]) ->
    {ok, #state{conn_pid = ConnPid}}.

handle_cast({input, {text, <<"quit">>}}, State = #state{conn_pid = ConnPid}) ->
    mudes_connection:send_text(ConnPid, <<"Bye!">>),
    mudes_connection:quit(ConnPid),
    {noreply, State};
handle_cast({input, {text, Text}}, State = #state{conn_pid = ConnPid}) ->
    Parsed = commands:parse(Text),
    process_command(Parsed, ConnPid, Text),
    {noreply, State}.

process_command(quit, ConnPid, _Orig) -> 
    mudes_connection:quit(ConnPid);
process_command({quit, _, _}, ConnPid, _Orig) ->
    mudes_connection:quit(ConnPid);
process_command({say, Text, _}, ConnPid, _Orig) ->
    lager:info("saying ~p", [Text]);
process_command(_Parsed, ConnPid, Orig) -> 
    mudes_connection:send_text(ConnPid, <<"Unknown command: ", Orig/binary>>).

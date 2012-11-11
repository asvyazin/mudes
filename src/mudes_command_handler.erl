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

handle_cast({input, {text, Text}}, State = #state{conn_pid = ConnPid}) ->
    Parsed = commands:parse(Text),
    process_command(Parsed, ConnPid, Text),
    {noreply, State}.

process_command(quit, ConnPid, _Orig) -> 
    quit(ConnPid);
process_command({say, Text}, ConnPid, _Orig) ->
    say(ConnPid, Text);
process_command(_Parsed, ConnPid, Orig) -> 
    mudes_connection:send_text(ConnPid, [<<"Unknown command: ">>, Orig]).

quit(ConnPid) ->
    mudes_connection:send_text(ConnPid, <<"Bye!">>),
    mudes_connection:quit(ConnPid).

say(ConnPid, Text) ->
    {ok, Me} = mudes_users:get_user_by_pid(ConnPid),
    {ok, UserPids} = mudes_users:get_pids(),
    [say(Me, Pid, ConnPid, Text) || Pid <- UserPids].

say(_Me, ConnPid, ConnPid, Text) ->
    mudes_connection:send_text(ConnPid, [<<"You say: ">>, Text]);
say(Me, Pid, _ConnPid, Text) ->
    mudes_connection:send_text(Pid, [Me, <<" say: ">>, Text]).

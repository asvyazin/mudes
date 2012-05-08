-module(mudes_handler).
-author('Alexander Svyazin <guybrush@live.ru>').

-behaviour(gen_server).

-export([start_link/1, start/2, process_command/3]).
-export([init/1, handle_call/3]).

-record(state, {conn}).

start_link(ConnPid) ->
    gen_server:start_link(?MODULE, [ConnPid], []).

start(ConnPid, SupRef) ->
    ChildSpec = {{mudes_handler, SupRef},
		 {mudes_handler, start_link, [ConnPid]},
		 temporary,
		 5000,
		 worker,
		 [mudes_handler]},
    supervisor:start_child(SupRef, ChildSpec).

init([ConnPid]) ->
    {ok, #state{conn = ConnPid}}.

process_command(Pid, Cmd, Args) ->
    gen_server:call(Pid, {process_command, Cmd, Args}).

handle_call({process_command, <<"quit">>, _Args}, _From, State) ->
    {reply, terminate, State};
handle_call({process_command, <<"who">>,_Args},_From,
	    State = #state{conn = ConnPid}) ->
    {ok, Users} = mudes_users:get_users(),
    Len = length(Users),
    LenBin = list_to_binary(integer_to_list(Len)),
    UsersTokens = [{text, U} || U <- Users],
    Tokens = [{text, <<"Currently online:">>}, UsersTokens,
	      {text, <<LenBin/binary, " users">>}],
    Result = mudes_connection:send_tokens(ConnPid, Tokens),
    {reply, Result, State};
handle_call({process_command, <<"say">>, Say}, _From,
	    State = #state{conn = ConnPid}) ->
    {ok, UserPids} = mudes_users:get_pids(),
    {ok, User} = mudes_users:get_user_by_pid(ConnPid),
    Result = do_say(ConnPid, UserPids, Say, User),
    {reply, Result, State};
handle_call({process_command, Cmd, _Args}, _From,
	    State = #state{conn = ConnPid}) ->
    Result = mudes_connection:send_text(ConnPid, <<"Unknown command: ", Cmd/binary>>),
    {reply, Result, State}.

do_say(ConnPid, UserPids, Say, User) ->
    [do_say_to_user(ConnPid, UserPid, Say, User) || UserPid <- UserPids],
    ok.

do_say_to_user(ConnPid, ConnPid, Say, _User) ->
    mudes_connection:send_text(ConnPid, <<"You say: ", Say/binary>>);
do_say_to_user(_ConnPid, UserPid, Say, User) ->
    mudes_connection:send_text(UserPid, <<User/binary, " says: ", Say/binary>>).

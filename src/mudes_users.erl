-module(mudes_users).
-author('Alexander Svyazin <guybrush@live.ru>').
-behaviour(gen_server).

-export([start_link/0, add_user/2, get_users/0, get_pids/0,
	 get_user_by_pid/1]).
-export([init/1, handle_cast/2, handle_info/2, handle_call/3]).

-record(state, {users = []}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    mudes_events:subscribe(connection_closed),
    {ok, #state{}}.

add_user(User, UserPid) ->
    gen_server:cast(?MODULE, {add_user, User, UserPid}).

get_users() ->
    gen_server:call(?MODULE, get_users).

get_pids() ->
    gen_server:call(?MODULE, get_pids).

get_user_by_pid(Pid) ->
    gen_server:call(?MODULE, {get_user, Pid}).

handle_cast({add_user, User, UserPid}, State = #state{users = Users}) ->
    {noreply, State#state{users = [{UserPid, User} | Users]}}.

handle_info({connection_closed, ConnPid, _Reason},
	    State = #state{users = Users}) ->
    Name = proplists:get_value(ConnPid, Users),
    lager:info("user ~p disconnected~n", [Name]),
    {noreply, State#state{users = proplists:delete(ConnPid, Users)}}.

handle_call(get_users, _From, State = #state{users = Users}) ->
    {reply, {ok, [V || {_K, V} <- Users]}, State};
handle_call(get_pids, _From, State = #state{users = Users}) ->
    {reply, {ok, [K || {K, _V} <- Users]}, State};
handle_call({get_user, Pid}, _From, State = #state{users = Users}) ->
    User = proplists:get_value(Pid, Users),
    {reply, {ok, User}, State}.

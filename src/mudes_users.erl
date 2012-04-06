-module(mudes_users).
-behaviour(gen_server).

-export([start_link/0, add_user/2, get_users/0]).
-export([init/1, handle_cast/2, handle_info/2, handle_call/3]).

-record(state, {users = []}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    {ok, #state{}}.

add_user(User, UserPid) ->
    gen_server:cast(?MODULE, {add_user, User, UserPid}).

get_users() ->
    gen_server:call(?MODULE, get_users).

handle_cast({add_user, User, UserPid}, State = #state{users = Users}) ->
    io:format("new user ~p from ~p~n", [User, UserPid]),
    erlang:monitor(process, UserPid),
    {noreply, State#state{users = [{UserPid, User} | Users]}}.

handle_info({'DOWN', _Ref, process, Pid, _reason},
	    State = #state{users = Users}) ->
    Name = proplists:get_value(Pid, Users),
    io:format("user ~p disconnected~n", [Name]),
    {noreply, State#state{users = proplists:delete(Pid, Users)}}.

handle_call(get_users, _From, State = #state{users = Users}) ->
    {reply, {ok, [V || {_K, V} <- Users]}, State}.

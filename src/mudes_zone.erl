-module(mudes_zone).
-behaviour(gen_server).

-export([start_link/1, set_name/2, get_name/1, lookup_pid/1, get_id/1]).
-export([init/1, handle_cast/2, handle_call/3]).

-record(state, {id, name}).

start_link(Id) ->
    gen_server:start_link(?MODULE, [Id], []).

set_name(Pid, NewName) ->
    gen_server:cast(Pid, {set_name, NewName}).

get_name(Pid) ->
    gen_server:call(Pid, get_name).

get_id(Pid) ->
    gen_server:call(Pid, get_id).

lookup_pid(Id) ->
    gproc:lookup_local_name({zone, Id}).

init([Id]) ->
    gproc:add_local_name({zone, Id}),
    {ok, #state{id = Id}}.

handle_cast({set_name, NewName}, State) ->
    {noreply, State#state{name = NewName}}.

handle_call(get_name, _From, State = #state{name = Name}) ->
    {reply, Name, State};
handle_call(get_id, _From, State = #state{id = Id}) ->
    {reply, Id, State}.

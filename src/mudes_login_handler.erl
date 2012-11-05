-module(mudes_login_handler).
-behaviour(mudes_handler).
-behaviour(gen_server).

%% mudes_handler
-export([start_link/1, input/2, quit/1]).

%% gen_server
-export([init/1, handle_cast/2, handle_call/3]).

-record(state, {conn_pid}).

start_link(ConnPid) ->
    gen_server:start_link(?MODULE, [ConnPid], []).

init([ConnPid]) ->
    {ok, #state{conn_pid = ConnPid}}.

quit(Pid) ->
    gen_server:cast(Pid, quit).

input(Pid, Input) ->
    gen_server:call(Pid, {input, Input}).

handle_cast(quit, State) ->
    {stop, normal, State}.
    

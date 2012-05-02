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

handle_call({process_command, Cmd, Args}, _From, State) ->
    {reply, terminate, State}.

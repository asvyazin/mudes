-module(mudes_sup).
-author('Alexander Svyazin <guybrush@live.ru>').
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).
-define(CHILD(I, Type, Params), {I, {I, start_link, Params}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    {ok, {{one_for_one, 5, 10},
	  [?CHILD(mudes_zones_sup, supervisor),
	   ?CHILD(mudes_users, worker),
	   ?CHILD(mudes_users_db, worker),
	   ?CHILD(mudes_connections_sup, supervisor)]}}.

-module(mudes_zones_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    ChildSpec = {mudes_zone, {mudes_zone, start_link, []},
		 transient, 5000, worker, [mudes_zone]},
    {ok, {{simple_one_for_one, 1, 60}, [ChildSpec]}}.

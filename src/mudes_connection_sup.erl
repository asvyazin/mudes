-module(mudes_connection_sup).
-author('Alexander Svyazin <guybrush@live.ru>').

-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
    supervisor:start_link(?MODULE, []).

init([]) ->
    {ok, {{one_for_all, 5, 10}, []}}.

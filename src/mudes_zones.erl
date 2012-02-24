-module(mudes_zones).
-author('Alexander Svyazin <guybrush@live.ru>').

-export([add_zone/1, get_zones_pids/0, remove_zone/1, get_zones/0]).

add_zone(Id) ->
    supervisor:start_child(mudes_zones_sup, [Id]).

get_zones_pids() ->
    ZoneSpecs = supervisor:which_children(mudes_zones_sup),
    [ Id || {_, Id, _, _} <- ZoneSpecs ].

get_zones() ->
    ZonesPids = get_zones_pids(),
    [ mudes_zone:get_id(Pid) || Pid <- ZonesPids ].

remove_zone(Id) ->
    Pid = mudes_zone:lookup_pid(Id),
    supervisor:terminate_child(mudes_zones_sup, Pid).

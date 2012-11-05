-module(mudes_handler).
-author('Alexander Svyazin <guybrush@live.ru>').
-export([behaviour_info/1]).
-export([input/2]).

behaviour_info(callbacks) ->
    [{start_link, 1}];
behaviour_info(_) ->
    undefined.

input(Pid, Input) ->
    gen_server:cast(Pid, {input, Input}).

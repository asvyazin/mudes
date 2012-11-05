-module(mudes_handler).
-export([behaviour_info/1]).

behaviour_info(callbacks) ->
    [{start_link, 1}, {input, 2}, {quit, 1}];
behaviour_info(_) ->
    undefined.

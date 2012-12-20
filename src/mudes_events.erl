-module(mudes_events).
-author('Alexander Svyazin <guybrush@live.ru>').
-export([subscribe/1, unsubscribe/1, notify/2]).

subscribe(EventType) ->
    gproc:reg({p, l, {?MODULE, EventType}}).

unsubscribe(EventType) ->
    gproc:unreg({p, l, {?MODULE, EventType}}).

notify(EventType, Msg) ->
    gproc:send({p, l, {?MODULE, EventType}}, Msg).

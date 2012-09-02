-module(mudes).
-author('Guybrush Threepwood <guybrush@live.ru>').

-export([start/0]).

start() ->
    application:start(compiler),
    application:start(syntax_tools),
    application:start(crypto),
    application:start(lager),
    application:start(ranch),
    application:start(mnesia),
    application:start(mudes),
    ranch:start_listener(mudes, 1, ranch_tcp, [{port, 10101}], telnet_protocol, []).

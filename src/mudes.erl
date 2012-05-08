-module(mudes).
-author('Guybrush Threepwood <guybrush@live.ru>').

-export([start/0]).

start() ->
    application:start(compiler),
    application:start(syntax_tools),
    application:start(crypto),
    application:start(lager),
    application:start(mnesia),
    application:start(cowboy),
    application:start(mudes),
    cowboy:start_listener(mudes, 1, cowboy_tcp_transport, [{port, 10101}], telnet_protocol, []).

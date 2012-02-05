-module(telnet).

-export([decode/1]).
-include("telnet.hrl").

decode(Data) ->
    decode(Data, []).

decode(<<?IAC, ?IAC, Rest/binary>>, L) ->
    decode(Rest, [255 | L]);
decode(<<?IAC, ?DO, C, Rest/binary>>, L) ->
    {ok, parse_result(L, [{do, C}]), Rest};
decode(<<?IAC, ?DONT, C, Rest/binary>>, L) ->
    {ok, parse_result(L, [{dont, C}]), Rest};
decode(<<?IAC, ?WILL, C, Rest/binary>>, L) ->
    {ok, parse_result(L, [{will, C}]), Rest};
decode(<<?IAC, ?WONT, C, Rest/binary>>, L) ->
    {ok, parse_result(L, [{wont, C}]), Rest};
decode(<<?IAC, C>> = Buf, L)
  when C == ?DO; C == ?DONT; C == ?WILL; C == ?WONT; C == ?SB ->
    return_rest(L, Buf);
decode(<<?IAC, ?SB, C, Rest/binary>> = Buf, L) ->
    decode_sb(Rest, L, C, Buf, []);
decode(<<?IAC, C, Rest/binary>>, L) ->
    {ok, parse_result(L, [{command, C}]), Rest};
decode(<<?CR, ?LF, Rest/binary>>, L) ->
    return_rest(L, Rest);
decode(<<?LF, ?CR, Rest/binary>>, L) ->
    return_rest(L, Rest);
decode(<<C, D, Rest/binary>>, L) when C == ?CR; C == ?LF ->
    return_rest(L, <<D, Rest>>);
decode(<<C>> = Buf, L) when C == ?CR; C == ?LF ->
    return_rest(L, Buf);
decode(<<C, Rest/binary>>, L) ->
    decode(Rest, [C | L]).

decode_sb(<<?IAC, ?SE, Rest/binary>>, L, C, _, SBData) ->
    {ok, parse_result(L, [{subnego, C, return_binary(SBData)}]), Rest};
decode_sb(<<>>, L, _, Buf, _) ->
    return_rest(L, Buf).

parse_result([], Tail) ->
    Tail;
parse_result(L, Tail) ->
    [return_text(L) | Tail].

return_text(L) ->
    {text, return_binary(L)}.

return_binary(L) ->
    list_to_binary(lists:reverse(L)).

return_rest([], Rest) ->
    {more, Rest};
return_rest(L, Rest) ->
    {ok, return_text(L), Rest}.

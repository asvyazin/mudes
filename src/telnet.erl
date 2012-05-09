-module(telnet).

-export([decode/1, encode/1]).
-include("telnet.hrl").

encode(Input) when is_list(Input) ->
    [encode(Chunk) || Chunk <- Input];
encode({do, C}) ->
    <<?IAC, ?DO, C>>;
encode({dont, C}) ->
    <<?IAC, ?DONT, C>>;
encode({will, C}) ->
    <<?IAC, ?WILL, C>>;
encode({wont, C}) ->
    <<?IAC, ?WONT, C>>;
encode({command, C}) ->
    <<?IAC, C>>;
encode({subnego, C, Data}) ->
    <<?IAC, ?SB, C, Data/binary, ?IAC, ?SE>>;
encode({text, Text}) ->
    Text2 = double_iac(Text),
    <<Text2/binary, ?CR, ?LF>>.

double_iac(Text) ->
    double_iac(Text, <<>>).

double_iac(<<>>, Buffer) ->
    Buffer;
double_iac(<<?IAC, Rest/binary>>, Buffer) ->
    double_iac(Rest, <<Buffer/binary, ?IAC, ?IAC>>);
double_iac(<<C, Rest/binary>>, Buffer) ->
    double_iac(Rest, <<Buffer/binary, C>>).

decode(<<>>) ->
    {more, <<>>};
decode(<<?IAC, ?IAC, Rest/binary>> = Data) ->
    decode(Rest, [?IAC], Data);
decode(<<?IAC, ?DO, C, Rest/binary>>) ->
    {ok, {do, C}, Rest};
decode(<<?IAC, ?DONT, C, Rest/binary>>) ->
    {ok, {dont, C}, Rest};
decode(<<?IAC, ?WILL, C, Rest/binary>>) ->
    {ok, {will, C}, Rest};
decode(<<?IAC, ?WONT, C, Rest/binary>>) ->
    {ok, {wont, C}, Rest};
decode(<<?IAC, C>> = Buf)
  when C == ?DO; C == ?DONT; C == ?WILL; C == ?WONT; C == ?SB ->
    {more, Buf};
decode(<<?IAC, ?SB, C, Rest/binary>> = Buf) ->
    decode_sb(Rest, C, Buf, []);
decode(<<?IAC, C, Rest/binary>>) ->
    {ok, {command, C}, Rest};
decode(<<C, Rest/binary>>) when C == ?CR; C == ?LF ->
    decode(Rest);
decode(Data) ->
    decode(Data, [], Data).

decode(<<?IAC>>, _, Data) ->
    {more, Data};
decode(<<?IAC, ?IAC, Rest/binary>>, L, Data) ->
    decode(Rest, [255 | L], Data);
decode(<<?IAC, _Rest/binary>> = Buf, L, _Data) ->
    {ok, return_text(L), Buf};
decode(<<C, Rest/binary>>, L, _Data) when C == ?CR; C == ?LF ->
    {ok, return_text(L), Rest};
decode(<<C, Rest/binary>>, L, Data) ->
    decode(Rest, [C | L], Data);
decode(<<>>, _L, Data) ->
    {more, Data}.

decode_sb(<<?IAC, ?SE, Rest/binary>>, C, _, SBData) ->
    {ok, {subnego, C, return_binary(SBData)}, Rest};
decode_sb(<<>>, _, Buf, _) ->
    {more, Buf};
decode_sb(<<D, Rest/binary>>, C, Buf, SBData) ->
    decode_sb(Rest, C, Buf, [D | SBData]).

return_text(L) ->
    {text, return_binary(L)}.

return_binary(L) ->
    list_to_binary(lists:reverse(L)).

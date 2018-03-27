%%%-------------------------------------------------------------------
%%% @author yujian
%%% @doc
%%%
%%% Created : 13. 三月 2017 10:55
%%%-------------------------------------------------------------------
-module(network_mod).

-include("network_pub.hrl").

-export([
    init_random/0,
    init_ws/0,
    send/1, send/2,
    sign/1, tcp_sign/1,
    ws_encode/1,
    ws_encode/2
]).


init_random() ->
    {MegaSecs, Secs, MicroSecs} = os:timestamp(),
    put(?pack_random, {MegaSecs, Secs, MicroSecs}),
    {MegaSecs, Secs, MicroSecs}.

init_ws() ->
    {MegaSecs, Secs, MicroSecs} = os:timestamp(),
    put(?pack_random, {MegaSecs, Secs, MicroSecs}),
    [MegaSecs, Secs, MicroSecs].

sign(Sign) ->
    case Sign =:= ws_uniform() of
        true ->
            ok;
        false ->
            ?return_err(?ERR_BAD_SIGN)
    end.

tcp_sign(Sign) ->
    case Sign =:= tcp_uniform() of
        true ->
            ok;
        false ->
            ?return_err(?ERR_BAD_SIGN)
    end.

ws_uniform() ->
    {A1, A2, A3} = get(?pack_random),
    B1 = (A1 * 171) rem 30269,
    B2 = (A2 * 172) rem 30307,
    B3 = (A3 * 170) rem 30323,
    put(?pack_random, {B1, B2, B3}),
    B1 + B2 + B3.

tcp_uniform() ->
    {A1, A2, A3} = get(?pack_random),
    B1 = (A1 * 171) rem 30269,
    B2 = (A2 * 172) rem 30307,
    B3 = (A3 * 170) rem 30323,
    put(?pack_random, {B1, B2, B3}),
    (B1 + B2 + B3) rem 255.


send(Port, Pack) ->
%%    ?DEBUG("debug:~p~n", [[self(), Pack]]),
    if
        Pack =:= <<>> -> ok;
        Pack =:= [] -> ok;
        is_binary(Pack) ->
            erlang:port_command(Port, Pack, [force]);
        true ->
            erlang:port_command(Port, ws_encode(?encode(Pack)), [force])
    end.

send(Pack) ->
%%    ?DEBUG("debug:~p~n", [[self(), Pack]]),
    Port = erlang:get(?c_socket),
    if
        Pack =:= <<>> -> ok;
        Pack =:= [] -> ok;
        is_binary(Pack) ->
            erlang:port_command(Port, Pack, [force]);
        true ->
            erlang:port_command(Port, ws_encode(?encode(Pack)), [force])
    end.

% 打包json数据
ws_encode(Bin) ->
    ws_encode(Bin, 1).

% 打包二进制数据
ws_encode(Bin, Opcode) ->
    Len = size(Bin),
    case Len of
        Len when Len < 126 ->
            <<1:1, 0:3, Opcode:4, 0:1, Len:7, Bin/binary>>;
        Len when Len < 65535 ->
            <<1:1, 0:3, Opcode:4, 0:1, 126:7, Len:16/unsigned-big-integer, Bin/binary>>;
        Len ->
            <<1:1, 0:3, Opcode:4, 0:1, 127:7, Len:32/unsigned-big-integer, Bin/binary>>
    end.
%%% -------------------------------------------------------------------
%%% Author  : Administrator
%%% Description : tcp封装一层，该模块只用来
%%% 1.维护连接状态。（tcp连接、管理、心跳）
%%% 2.收发信息 （tcp收发信息）
%%% 3.转发进程信息（进程接收到的消息，转发给玩家回調模块（同一个进程）做逻辑处理）
%%%
%%% Created : 2012-9-24
%%% -------------------------------------------------------------------

-module(l2c_ws).

-include("network_pub.hrl").

-behaviour(gen_server).

-export([pack_encode/1, pack_encode/2]).

-export([start_link/1, init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).


start_link([LSocet, Socket, CallBack]) ->
    gen_server:start_link(?MODULE, [LSocet, Socket, CallBack], []).


init([_LSocet, Socket, _CallBack]) ->
    process_flag(trap_exit, true),
    inet:setopts(Socket, [{active, once}]),
    
    ?put_new(?TCP_CONNECT_STATE, 0),
    ?put_new(?c_socket, Socket),
    ?process_call_mod:init({Socket}).


handle_call(Msg, From, State) ->
    ?process_call_mod:handle_call(Msg, From, State).


handle_cast(Msg, State) ->
    ?process_call_mod:handle_cast(Msg, State).


%% 恶意连接,发大数据,具体最大数据还要测试,暂定40000,(5000　* 8)
handle_info({tcp, _Socket, RecvBin}, State) when bit_size(RecvBin) > 40000 ->
    ?INFO("tcp big data~n"),
    {stop, {tcp, max_bit}, State};

%% 收到tcp数据,握手
handle_info({tcp, Socket, RecvBin}, State) ->
%%    ?INFO("ws handle_info:~p~n", [[self(), Socket, RecvBin, State]]),
    CSocket = ?get(?c_socket),
    Ret =
        if
            CSocket =:= Socket ->
                case ?get(?TCP_CONNECT_STATE) of
                    0 ->
                        HeaderList = binary:split(RecvBin, <<"\r\n">>, [global]),
                        HeaderList1 = [list_to_tuple(binary:split(I, <<": ">>)) || I <- HeaderList, I /= <<>>],
                        SecWebSocketKey = proplists:get_value(<<"Sec-WebSocket-Key">>, HeaderList1, <<>>),
                        IP = proplists:get_value(<<"X-Real-IP">>, HeaderList1, <<>>),
                        Sha1 = crypto:hash(sha, [SecWebSocketKey, <<"258EAFA5-E914-47DA-95CA-C5AB0DC85B11">>]),
                        Base64 = base64:encode(Sha1),
                        Handshake = [<<"HTTP/1.1 101 Switching Protocols\r\n">>, <<"Upgrade: websocket\r\n">>, <<"Connection: Upgrade\r\n">>,
                            <<"Sec-WebSocket-Accept: ">>, Base64, <<"\r\n">>, <<"\r\n">>],
                        gen_tcp:send(Socket, Handshake),
                        ?put_new(?c_ip, IP),
                        network_mod:send(network_mod:init_ws()),
                        erlang:erase(?TCP_CONNECT_STATE),
                        {noreply, State};
                    
                    ?undefined ->
                        handle_pack(Socket, RecvBin, State)
                end;
            true ->
                {noreply, State}
        end,
    inet:setopts(CSocket, [{active, once}]),
%%    ?INFO("ws handle_info:~p~n", [[self(), RecvBin, Ret, State]]),
    Ret;


%% 用户正常下线,返回stop即可,析构函数在terminate()中
handle_info({tcp_closed, _Socket}, State) ->
%%    ?INFO("ws tcp_closed:~p~n", [[self(), _Socket, State]]),
    {stop, normal, State};

handle_info({tcp_passive, _Socket}, State) ->
%%    ?INFO("tcp tcp_passive:~p~n", [[self(), erlang:get(?c_socket), _Socket, State]]),
    {stop, normal, State};

handle_info({tcp_error, _Socket, _Reason}, State) ->
%%    ?INFO("tcp tcp_error:~p~n", [[self(), erlang:get(?c_socket), _Socket, State, _Reason]]),
    {stop, normal, State};

handle_info({inet_reply, _Sock, _Error}, State) ->
    if
        _Error =:= ok -> ok;
        true ->
            ?INFO("inet reply error: ~p~n", [_Error])
    end,
    {noreply, State};

handle_info(Info, State) ->
    ?process_call_mod:handle_info(Info, State).


%% 进程关闭(包括正常下线,非正常下线都会调用此函数)
terminate(Reason, State) ->
%%    ?INFO("ws terminate:~p~n", [[self(), Reason, State]]),
    ?process_call_mod:terminate(Reason, State).


code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%% 正式接受数据
handle_pack(Socket, Data, State) ->
    case Data of
        <<_Fin:1, _Rsv:3, 8:4, _Rest/binary>> -> %% 关闭连接
%%            ?INFO("clietn close connect"),
            gen_tcp:send(Socket, <<1:1, 0:3, 8:4>>),
            {stop, normal, State};
        <<_Fin:1, _Rsv:3, 9:4, _Rest/binary>> -> %% ping,返回pong
            gen_tcp:send(Socket, <<1:1, 0:3, 9:4>>),
            {noreply, State};
        <<_Fin:1, _Rsv:3, _Opcode:4, _Mask:1, 126:7, Len:16/unsigned-big-integer, Rest/binary>> ->
            handle_pack(Socket, Len, Rest, State);
        <<_Fin:1, _Rsv:3, _Opcode:4, _Mask:1, 127:7, Len:32/unsigned-big-integer, Rest/binary>> ->
            handle_pack(Socket, Len, Rest, State);
        <<_Fin:1, _Rsv:3, _Opcode:4, _Mask:1, Len:7, Rest/binary>> ->
            handle_pack(Socket, Len, Rest, State);
        _ ->
%%            ?INFO("tcp websocket RecvBin init error:~p~n", [[Data, State]]),
%%            {stop, normal, State}
            {noreply, State}
    end.

handle_pack(Socket, Len, Rest, State) ->
    case Rest of
        <<Masking:4/binary, Payload:Len/binary, Next/binary>> ->
            Str = unmask(Payload, Masking, <<>>),
            case handle_str(Socket, Str, State) of
                {noreply, State2} ->
                    case size(Next) of
                        0 -> {noreply, State2};
                        _Other ->
                            handle_pack(Socket, Next, State2)
                    end;
                {stop, _Normal, State} ->
%%                    ?INFO("handle_info({tcp, Socket, Str}, State) return close"),
                    {stop, normal, State}
            end;
        _ ->
%%            ?INFO("tcp websocket RecvBin error handle_pack/5:~p~n", [Rest]),
%%            {stop, normal, State}
            {noreply, State}
    end.



handle_str(Socket, Str, State) ->
    ?process_call_mod:handle_info({tcp, Socket, Str}, State).


unmask(Payload, Masking = <<MA:8, MB:8, MC:8, MD:8>>, Acc) ->
    case size(Payload) of
        0 -> Acc;
        1 ->
            <<A:8>> = Payload,
            <<Acc/binary, (MA bxor A)>>;
        2 ->
            <<A:8, B:8>> = Payload,
            <<Acc/binary, (MA bxor A), (MB bxor B)>>;
        3 ->
            <<A:8, B:8, C:8>> = Payload,
            <<Acc/binary, (MA bxor A), (MB bxor B), (MC bxor C)>>;
        _Other ->
            <<A:8, B:8, C:8, D:8, Rest/binary>> = Payload,
            Acc1 = <<Acc/binary, (MA bxor A), (MB bxor B), (MC bxor C), (MD bxor D)>>,
            unmask(Rest, Masking, Acc1)
    end.


pack_encode(Bin) ->
    pack_encode(Bin, 1).

pack_encode(Bin, Opcode) ->
    Len = size(Bin),
    case Len of
        Len when Len < 126 ->
            <<1:1, 0:3, Opcode:4, 0:1, Len:7, Bin/binary>>;
        Len when Len < 65535 ->
            <<1:1, 0:3, Opcode:4, 0:1, 126:7, Len:16/unsigned-big-integer, Bin/binary>>;
        Len ->
            <<1:1, 0:3, Opcode:4, 0:1, 127:7, Len:32/unsigned-big-integer, Bin/binary>>
    end.
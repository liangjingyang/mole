%%===========================================================================
%%     FileName: udp_client.erl
%%         Desc: 
%%       Author: liangjingyang
%%        Email: ljy0922@gmail.com
%%     HomePage: http://www.cnblogs.com/liangjingyang
%%      Version: 0.0.1
%%   LastChange: 2013-04-17 16:52:14
%%      History:
%%===========================================================================
-module(udp_client).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-export([start/3]).

-behaviour(gen_server).

-record(client_data, {id, aim_id, wan, lan, conn, hand}).
-define(SERVER_IP, {106,186,30,200}).
-define(SERVER_PORT, 6666).

start(Id, AimId, Port) ->
    gen_server:start({local, ?MODULE}, ?MODULE, [Id, AimId, Port], []).

init([Id, AimId, Port]) ->
    {ok, Socket} = gen_udp:open(Port),
    put_socket(Socket),
    put_hole_tag(false),
    IpList = get_local_ip(),
    put_my_data(#client_data{id = Id, aim_id = AimId, lan = {IpList, Port}}),
    erlang:send_after(1000, self(), connect),
    {ok, []}.

handle_call(_Request, _From, State) ->
    {noreply, State}.

handle_cast(_Request, State) ->
    {noreply, State}.

%% go
handle_info({udp, _Socket, _Ip, _Port, [3|_L]}, State) ->
    ClientData = get_hole_tag(),
    put_hole_tag(ClientData#client_data{hand = true}),
    erlang:send_after(3 * 1000, self(), go),
    io:format("let's go~n", []),
    {noreply, State};

%% lan
handle_info({udp, Socket, Ip, Port, [1|_L]}, State) ->
    case get_hole_tag() of
        #client_data{} = AimClient ->
            %{LanIp, LanPort} = binary_to_term(list_to_binary(L)),
            AimClient2 = AimClient#client_data{conn = {Ip, Port}},
            io:format("connect from lan lan lan, ~w~n", [{Ip, Port}]), 
            put_hole_tag(AimClient2),
            gen_udp:send(Socket, Ip, Port, <<3:8>>);
        _ ->
            ignore
    end,
    {noreply, State};
%% wan
handle_info({udp, Socket, Ip, Port, [2|_L]}, State) ->
    case get_hole_tag() of
        #client_data{} = AimClient ->
            case AimClient#client_data.conn of 
                undefined ->
                    AimClient2 = AimClient#client_data{conn = {Ip, Port}},
                    io:format("connect from wan wan wan, ~w~n", [{Ip, Port}]), 
                    put_hole_tag(AimClient2),
                    gen_udp:send(Socket, Ip, Port, <<3:8>>);
                _ ->
                    ignore
            end;
        _ ->
            ignore
    end,
    {noreply, State};

handle_info({udp, _Socket, _Ip, _Port, Packet}, State) ->
    Data = binary_to_term(list_to_binary(Packet)),
    handle_data(Data),
    {noreply, State};

handle_info(connect, State) ->
    Socket = get_socket(),
    case get_hole_tag() of
        false ->
            erlang:send_after(1000, self(), connect),
            gen_udp:send(Socket, ?SERVER_IP, ?SERVER_PORT, term_to_binary(get_my_data()));
        _ ->
            ignore
    end,
    {noreply, State};

handle_info(p2p_conn, State) ->
    ClientData = get_hole_tag(),
    case ClientData#client_data.hand of
        true ->
            ignore;
        _ ->
            erlang:send_after(2 * 1000, self(), p2p_conn)
    end,
    p2p_conn(ClientData),
    {noreply, State};

handle_info(go, State) ->
    erlang:send_after(3 * 1000, self(), go),
    #client_data{id=Id, aim_id=AimId, conn = {Ip, Port}} = get_hole_tag(),
    gen_udp:send(get_socket(), Ip, Port, term_to_binary({hello, Id, im, AimId})),
    {noreply, State};

handle_info(Info, State) ->
    io:format("recv other info : ~w~n", [Info]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

handle_data(#client_data{id=Id} = ClientData) ->
    io:format("begin make hole with ~w~n", [Id]),
    p2p_conn(ClientData),
    put_hole_tag(ClientData),
    erlang:send_after(2* 1000, self(), p2p_conn);

handle_data(Data) ->
    io:format("Data: ~p~n", [Data]).

get_socket() ->
    erlang:get(udp_socket).
put_socket(Socket) ->
    erlang:put(udp_socket, Socket).

get_hole_tag() ->
    erlang:get(hole_tag).
put_hole_tag(Tag) ->
    erlang:put(hole_tag, Tag).

get_my_data() ->
    erlang:get(my_data).
put_my_data(Data) ->
    erlang:put(my_data, Data).


get_local_ip() ->
    Ip192 = get_local_192(),
    Ip172 = get_local_172(),
    Ip10 = get_local_10(),
    Ip192 ++ Ip172 ++ Ip10.

get_local_192() ->
    Cmd = "ifconfig |grep \"inet addr:\" | awk -F:  '{print $2}'|awk '{print $1}'|grep '^192\\.168'",
    get_ip_from_os(Cmd).

get_local_172() ->
    Cmd = "ifconfig |grep \"inet addr:\" | awk -F:  '{print $2}'|awk '{print $1}'|grep '^172\\.16'",
    get_ip_from_os(Cmd).

get_local_10() ->
    Cmd = "ifconfig |grep \"inet addr:\" | awk -F:  '{print $2}'|awk '{print $1}'|grep '^10\\.'",
    get_ip_from_os(Cmd).

get_ip_from_os(Cmd) ->
    Ip = os:cmd(Cmd),
    IpList = string:tokens(Ip, "\n"),
    lists:map(fun(IpStr) ->
                IpStr2 = string:tokens(IpStr, "."),
                IpStr3 = lists:map(fun(X) ->list_to_integer(X) end, IpStr2),
                list_to_tuple(IpStr3)
        end, IpList).

p2p_conn(ClientData) ->
    Socket = get_socket(),
    #client_data{id=Id, aim_id=AimId, wan={WanIp, WanPort}=Wan, lan = {LanIpList, LanPort}} = ClientData,
    WanBinary = term_to_binary(Wan), 
    gen_udp:send(Socket, WanIp, WanPort, <<2:8, WanBinary/binary>>),
    io:format("p2p_conn, wan, aimId:~w, id:~w, wanip:~w, wanport:~w~n", [Id, AimId, WanIp, WanPort]),
    lists:foreach(fun(LIp) ->
                LanBinary = term_to_binary({LIp, LanPort}), 
                io:format("p2p_conn, lan, aimId:~w, id:~w, lanip:~w, lanport:~w~n", [Id, AimId, LIp, LanPort]),
                gen_udp:send(Socket, LIp, LanPort, <<1:8, LanBinary/binary>>)
        end, LanIpList).


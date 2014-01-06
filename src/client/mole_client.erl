-module(mole_client).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-export([start/5]).

-behaviour(gen_server).

-record(state, {socket, s_ip, s_port, conn, my_lan, his_net, his_key, my_packet}).

-define(SERVER_REQ, 1).
-define(SERVER_RES, 2).
-define(WAN_CONN, 3).
-define(LAN_CONN, 4).
-define(P2P_DATA, 5).

start(MyKey, HisKey, Port, ServerIp, ServerPort) ->
    gen_server:start({local, ?MODULE}, ?MODULE, [MyKey, HisKey, Port, ServerIp, ServerPort], []).

init([MyKey, HisKey, Port, ServerIp, ServerPort]) ->
    {ok, Socket} = gen_udp:open(Port, [binary]),
    IpList = get_local_ip(),
    LanBin = term_to_binary({IpList, Port}),
    MyPacket = <<?SERVER_REQ:8, MyKey:128, HisKey:128, LanBin/binary>>,
    erlang:send_after(1000, self(), server_req),
    {ok, #state{socket = Socket, s_ip = ServerIp, s_port = ServerPort, my_lan = {IpList, Port}, my_packet = MyPacket}}.

handle_call(_Request, _From, State) ->
    {noreply, State}.

handle_cast(_Request, State) ->
    {noreply, State}.


%% lan
handle_info({udp, _Socket, Ip, Port, <<?LAN_CONN:8>>}, State) ->
    case State#state.his_net of
        undefined ->
            State2 = State;
        _His ->
            State2 = State#state{conn = {Ip, Port}},
            io:format("connect from lan lan lan, ~w~n", [{Ip, Port}]), 
            erlang:send_after(3 * 1000, self(), heartbeat)
    end,
    {noreply, State2};

%% wan
handle_info({udp, _Socket, Ip, Port, <<?WAN_CONN:8>>}, State) ->
    case State#state.his_net of
        undefined ->
            State2 = State;
        _His ->
            case State#state.conn of 
                undefined ->
                    State2 = State#state{conn = {Ip, Port}},
                    io:format("connect from wan wan wan, ~w~n", [{Ip, Port}]), 
                    erlang:send_after(3 * 1000, self(), heartbeat);
                _ ->
                    State2 = State 
            end
    end,
    {noreply, State2};

%% server response
handle_info({udp, _Socket, _Ip, _Port, <<?SERVER_RES:8, HisKey:128, I1:8, I2:8, I3:8, I4:8, WanPort:16, Packet/binary>>}, State) ->
    WanIp = {I1, I2, I3, I4},
    io:format("begin make hole with ~w~n", [WanIp]),
    LanArgs = binary_to_term(Packet),
    p2p_conn(State#state.socket, {WanIp, WanPort, LanArgs}),
    erlang:send_after(2* 1000, self(), p2p_conn),
    {noreply, State#state{his_net = {WanIp, WanPort, LanArgs}, his_key = HisKey}};


handle_info({udp, _Socket, Ip, _Port, Packet}, State) ->
    io:format("normal ip:~p, packet: ~p~n", [Ip, Packet]),
    {noreply, State};

%% server request
handle_info(server_req, State) ->
    case State#state.his_net of
        undefined ->
            erlang:send_after(1000, self(), server_req),
            gen_udp:send(State#state.socket, State#state.s_ip, State#state.s_port, State#state.my_packet);
        _ ->
            ignore
    end,
    {noreply, State};

%% p2p_conn
handle_info(p2p_conn, State) ->
    case State#state.conn of
        undefined ->
            erlang:send_after(2 * 1000, self(), p2p_conn),
            p2p_conn(State#state.socket, State#state.his_net);
        _ ->
            ignore
    end,
    {noreply, State};

%% heartbeat 
handle_info(heartbeat, State) ->
    erlang:send_after(3 * 1000, self(), heartbeat),
    {Ip, Port} = State#state.conn,
    gen_udp:send(State#state.socket, Ip, Port, <<?P2P_DATA:8, <<"heartbeat">>/binary>>),
    {noreply, State};

handle_info(Info, State) ->
    io:format("recv other info : ~w~n", [Info]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


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

p2p_conn(Socket, {WanIp, WanPort, {LanIpList, LanPort}}) ->
    gen_udp:send(Socket, WanIp, WanPort, <<?WAN_CONN:8>>),
    io:format("p2p_conn, wan, wanip:~w, wanport:~w~n", [WanIp, WanPort]),
    lists:foreach(fun(LIp) ->
                io:format("p2p_conn, lan, lanip:~w, lanport:~w~n", [LIp, LanPort]),
                gen_udp:send(Socket, LIp, LanPort, <<?LAN_CONN:8>>)
        end, LanIpList).


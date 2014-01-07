
-module(mole_worker).

-author("simple.continue@gmail.com").

-behaviour(gen_server).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-export([start_link/0]).

-include("mole.hrl").

-record(state, {server, socket, self}).

start_link() ->
    gen_server:start_link(?MODULE, [], []).

init([]) ->
    Self = self(),
    insert_worker_pid(Self),
    io:format("worker start~n", []),
    {ok, #state{self = Self}}.
            
handle_call(_Request, _From, State) ->
    {noreply, State}.

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info({ready_to_work, Server, Socket}, State) ->
    get_work(Server),
    {noreply, State#state{server = Server, socket = Socket}};

handle_info({work, Data}, #state{server = Server, socket = Socket} = State) ->
    do_work(Socket, Data),
    get_work(Server),
    {noreply, State};

handle_info(Info, State) ->
    io:format("recv other info : ~w~n", [Info]),
    {noreply, State}.

terminate(_Reason, State) ->
    delete_worker_pid(State#state.self),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
    
get_work(Server) ->
    Server ! {get_work, self()}.

do_work(Socket, {MyAddr, MyPort, <<?SERVER_REQ:8, MyKey:128/bitstring, HisKey:128/bitstring, MyPacket/binary>>}) ->
    MyEtsKey = #key{my_key = MyKey, his_key = HisKey},
    HisEtsKey = #key{my_key = HisKey, his_key = MyKey},
    MyAddrBin = ip_tup2bin(MyAddr),
    MyPortBin = <<MyPort:16>>,
    MyPacketBin = <<MyAddrBin/binary, MyPortBin/binary, MyPacket/binary>>,
    case ets:lookup(?ETS_MOLE, HisEtsKey) of
        [] ->
            Now = mole:now(),
            ets:insert(?ETS_MOLE, {MyEtsKey, Now, MyAddr, MyPort, MyPacketBin});
        [{_, _, HisAddr, HisPort, HisPacketBin}] ->
            %% to he
            gen_udp:send(Socket, HisAddr, HisPort, <<?SERVER_RES:8, MyKey/binary, MyPacketBin/binary>>),
            %% to me
            gen_udp:send(Socket, MyAddr, MyPort, <<?SERVER_RES:8, HisKey/binary, HisPacketBin/binary>>),
            ets:delete(?ETS_MOLE, MyEtsKey),
            ets:delete(?ETS_MOLE, HisEtsKey)
    end.

insert_worker_pid(Pid) ->
    case ets:lookup(?ETS_MOLE, worker) of
        [] ->
            ets:insert(?ETS_MOLE, {worker, [Pid]});
        [{worker, L}] ->
            ets:insert(?ETS_MOLE, {worker, [Pid|L]})
    end.

delete_worker_pid(Pid) ->
    case ets:lookup(?ETS_MOLE, worker) of
        [{worker, L}] ->
            ets:insert(?ETS_MOLE, {worker, lists:delete(Pid, L)});
        _ ->
            ignore
    end.

ip_tup2bin(IpTup) ->
    {A, B, C, D} = IpTup,
    <<A:8, B:8, C:8, D:8>>.


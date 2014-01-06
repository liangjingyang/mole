
-module(mole_worker).

-author("simple.continue@gmail.com").

-behaviour(gen_server).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-export([start_link/0]).

-record(state, {server, socket, self}).
-record(key, {my, his}).

-define(SERVER_REQ, 1).
-define(SERVER_RES, 2).


start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    Self = self(),
    insert_worker_pid(Self),
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

do_work(Socket, {MyAddr, MyPort, <<?SERVER_REQ:8, My:128, His:128, MyPacket/binary>>}) ->
    MyKey = #key{my = My, his = His},
    HisKey = #key{my = His, his = My},
    MyAddrBin = ip_tup2bin(MyAddr),
    MyPortBin = <<MyPort:16>>,
    MyPacketBin = <<MyAddrBin/binary, MyPortBin/binary, MyPacket/binary>>,
    case ets:lookup(ets_mole, HisKey) of
        [] ->
            ets:insert(ets_mon, {MyKey, MyAddr, MyPort, MyPacketBin});
        [{_, HisAddr, HisPort, HisPacketBin}] ->
            %% to he
            gen_udp:send(Socket, HisAddr, HisPort, <<?SERVER_RES:8, MyKey:128, MyPacketBin/binary>>),
            %% to me
            gen_udp:send(Socket, MyAddr, MyPort, <<?SERVER_RES:8, HisKey:128, HisPacketBin/binary>>),
            ets:delete(ets_mon, MyKey),
            ets:delete(ets_mon, HisKey)
    end.

insert_worker_pid(Pid) ->
    case ets:lookup(ets_mole, worker) of
        [] ->
            ets:insert(ets_mole, {worker, [Pid]});
        [{worker, L}] ->
            ets:insert(ets_mole, {worker, [Pid|L]})
    end.

delete_worker_pid(Pid) ->
    case ets:lookup(ets_mole, worker) of
        [{worker, L}] ->
            ets:insert(ets_mole, {worker, lists:delete(Pid, L)});
        _ ->
            ignore
    end.

ip_tup2bin(IpTup) ->
    {A, B, C, D} = IpTup,
    <<A:8, B:8, C:8, D:8>>.


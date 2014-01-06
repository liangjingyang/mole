-module(mole_server).

-author("simple.continue@gmail.com").

-behaviour(gen_server).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([start/0]).


-define(UDP_PORT, 6666).

start() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    {ok, Socket} = gen_udp:open(?UDP_PORT, [{active, false}, binary]),
    Server = self(),
    insert_server_pid(Server),
    case worker_go(Server, Socket) of
        no_worker ->
            {stop, no_worker};
        ok ->
            {ok, Socket}
    end.

handle_call(_Request, _From, State) ->
    {noreply, State}.

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info({get_work, WorkerPid} = Info, Socket) ->
    case gen_udp:recv(Socket, 0) of
        {ok, {Addr, Port, Packet}} ->
            WorkerPid ! {work, Socket, {Addr, Port, Packet}};
        _ ->
            self() ! Info
    end,
    {noreply, Socket};

handle_info(Info, State) ->
    io:format("recv other info : ~w~n", [Info]),
    {noreply, State}.

terminate(_Reason, _State) ->
    delete_server_pid(),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

insert_server_pid(Server) ->
    ets:insert(ets_mole, {server, Server}).

delete_server_pid() ->
    ets:delete(ets_mole, server).

worker_go(Server, Socket) ->
    case ets:lookup(ets_mole, worker) of
        [] ->
            no_worker; 
        [{worker, Workers}] ->
            lists:foreach(fun(W) -> W ! {ready_to_work, Server, Socket} end, Workers),
            ok
    end.

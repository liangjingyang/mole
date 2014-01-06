-module(mole_server).

-author("simple.continue@gmail.com").

-behaviour(gen_server).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([start_link/0]).


-include("mole.hrl").

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    {ok, Port} = application:get_env(?APP, port),
    {ok, Socket} = gen_udp:open(Port, [{active, false}, binary]),
    Server = self(),
    case worker_go(Server, Socket) of
        no_worker ->
            io:format("error, no_worker~n", []),
            {stop, no_worker};
        ok ->
            insert_server_pid(Server),
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
    ets:insert(?ETS_MOLE, {server, Server}).

delete_server_pid() ->
    ets:delete(?ETS_MOLE, server).

worker_go(Server, Socket) ->
    case ets:lookup(?ETS_MOLE, worker) of
        [] ->
            no_worker; 
        [{worker, Workers}] ->
            lists:foreach(fun(W) -> W ! {ready_to_work, Server, Socket} end, Workers),
            ok
    end.

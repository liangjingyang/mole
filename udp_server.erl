%%===========================================================================
%%     FileName: udp_server.erl
%%         Desc: 
%%       Author: liangjingyang
%%        Email: ljy0922@gmail.com
%%     HomePage: http://www.cnblogs.com/liangjingyang
%%      Version: 0.0.1
%%   LastChange: 2013-04-17 15:15:20
%%      History:
%%===========================================================================
-module(udp_server).

-behaviour(gen_server).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([start/0]).

-record(client_data, {id, aim_id, wan, lan, conn, hand}).

-define(UDP_PORT, 6666).

start() ->
    gen_server:start({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    {ok, Socket} = gen_udp:open(?UDP_PORT),
    put_socket(Socket),
    put_client_list([]),
    {ok, []}.

handle_call(_Request, _From, State) ->
    {noreply, State}.

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info({udp, _Socket, IP, InPortNo, Packet} = Info, State) ->
    io:format("recv info : ~w~n", [Info]),
    check_client(get_socket(), IP, InPortNo, Packet),
    {noreply, State};

handle_info(Info, State) ->
    io:format("recv other info : ~w~n", [Info]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

check_client(Socket, WanIpA, WanPortA, Packet) ->
    ClientDataA = binary_to_term(list_to_binary(Packet)),
    ClientDataA2 = ClientDataA#client_data{wan = {WanIpA, WanPortA}},
    ClientList = get_client_list(),
    ClientList2 = lists:keystore(ClientDataA2#client_data.id, #client_data.id, ClientList, ClientDataA2),
    case lists:keyfind(ClientDataA2#client_data.aim_id, #client_data.id, ClientList2) of
        false ->
            ClientList4 = ClientList2;
        #client_data{wan={WanIpB, WanPortB}}=ClientDataB ->
            gen_udp:send(Socket, WanIpA, WanPortA, term_to_binary(ClientDataB)),
            gen_udp:send(Socket, WanIpB, WanPortB, term_to_binary(ClientDataA2)),
            io:format("let client make hole: ~w ~w~n", [ClientDataA2, ClientDataB]),
            ClientList3 = lists:keydelete(ClientDataA2#client_data.id, #client_data.id, ClientList2),
            ClientList4 = lists:keydelete(ClientDataB#client_data.id, #client_data.id, ClientList3)
    end,
    put_client_list(ClientList4).

get_client_list() ->
    erlang:get(client_list).
put_client_list(List) ->
    erlang:put(client_list, List).

get_socket() ->
    erlang:get(udp_socket).
put_socket(Socket) ->
    erlang:put(udp_socket, Socket).

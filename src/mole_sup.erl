-module(mole_sup).

-author("simple.continue@gmail.com").

-behaviour(supervisor).

-export([
        start/0,
        init/1,
        start_ttl/0
    ]).

-include("mole.hrl").

start() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    ets:new(?ETS_MOLE, [named_table, public]),
    {ok, {{one_for_one, 10, 10}, []}}.

start_ttl() ->
    Pid = spawn_link(fun() -> check_ttl() end),
    io:format("check_ttl pid ~p~n", [Pid]),
    {ok, Pid}.

check_ttl() ->
    receive
        ttl ->
            List = ets:tab2list(?ETS_MOLE),
            Now = mole:now(),
            lists:foreach(fun
                    ({#key{} = Key, Ttl, _, _, _}) ->
                        case Now > Ttl + 10 of
                            true ->
                                ets:delete(?ETS_MOLE, Key);
                            false ->
                                ignore
                        end;
                    (_) ->
                        ignore
                end, List)
    after 10 ->
            self() ! ttl
    end,
    check_ttl().

-module(mole).

-author("simple.continue@gmail.com").

-export([
        start/0,
        start/2,
        stop/0,
        stop/1,
        now/0
    ]).

-include("mole.hrl").

start() ->
    application:start(mole).

start(_, _) ->
    {ok, Sup} = mole_sup:start(),
    case application:get_env(?APP, worker_num) of
        {ok, Num} ->
            lists:foreach(fun(N) ->
                       _Res =  supervisor:start_child(Sup, 
                            {list_to_atom(lists:concat([mole_worker, N])), 
                                {mole_worker, start_link, []}, 
                                permanent, 2000, worker, [mole_worker]
                            })
                        %io:format("start worker ~w res: ~w~n", [N, _Res])
                end, lists:seq(1, Num)),
            supervisor:start_child(Sup, 
                {mole_server,
                    {mole_server, start_link, []},
                    permanent, 2000, worker, [mole_server]
                }),
            supervisor:start_child(Sup, 
                {mole_ttl, 
                    {mole_sup, start_ttl, []},
                    permanent, 2000, worker, [mole_sup]
                }),
            {ok, Sup};
        _ ->
            {stop, Sup}
    end.

stop() ->
    application:stop(mole).

stop(_State) ->
    ok.


now() ->
    {A, B, _} = erlang:now(),
    A * 1000000 + B.

    


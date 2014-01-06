-module(mole).

-author("simple.continue@gmail.com").

-export([
        start/0,
        start/2
    ]).

-define(APP, mole).

start() ->
    application:start(mole).

start(_, _) ->
    {ok, Sup} = mole_sup:start(),
    case application:get_env(?APP, worker_num) of
        {ok, Num} ->
            lists:foreach(fun(N) ->
                        supervisor:start_child(Sup, 
                            {list_to_atom(lists:concat([mole_worker, N])), 
                                {mole_worker, start_link, []}, 
                                permanent, 2000, worker, [mole_worker]
                            })
                end, lists:seq(1, Num)),
            supervisor:start_ckild(Sup, 
                {mole_server,
                    {mole_server, start_link, []},
                    permanent, 2000, worker, [mole_server]
                }),
            {ok, Sup};
        _ ->
            {stop, Sup}
    end.


    


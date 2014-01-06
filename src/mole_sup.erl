-module(mole_sup).

-author("simple.continue@gmail.com").

-behaviour(supervisor).

-export([
        start/0,
        init/1
    ]).

-define(ETS_MOLE, ete_mole).

start() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    ets:new(?ETS_MOLE, [named_table, public]),
    {ok, {{one_for_one, 10, 10}, []}}.


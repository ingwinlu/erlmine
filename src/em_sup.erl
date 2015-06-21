-module(em_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
	Procs = [
            {
                em_game_sup,
                {em_game_sup, start_link, []},
                permanent, infinity, supervisor, [em_game_sup]
            }
        ],
	{ok, {{one_for_one, 1, 5}, Procs}}.

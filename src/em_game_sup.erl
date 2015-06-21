-module(em_game_sup).
-behaviour(supervisor).

-export([start_link/0, new/0]).
-export([init/1]).


% API
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

new() ->
    {ok, Pid} = supervisor:start_child(?MODULE, []),
    Pid.


% Callbacks
init([]) ->
    Procs = [
        {
            em_game,
            {em_game, start_link, []},
            temporary, 5000, worker, [em_game]
        }
    ],
    {ok, {{simple_one_for_one, 0, 1}, Procs}}.

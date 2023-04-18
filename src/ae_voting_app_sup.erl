-module(ae_voting_app_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    Procs = [
        #{
            id => counter,
            start => {counter, start_link, [0]}
        },
        #{
            id => poll_keeper,
            start => {poll_keeper, start_link, []}
        },
        #{
            id => vanillae_man,
            start => {vanillae_man, start_link, []}
        }
    ],
    {ok, {{one_for_one, 1, 5}, Procs}}.

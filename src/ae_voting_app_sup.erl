-module(ae_voting_app_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    Procs = [
        %#{
        %    id => contract_man,
        %    start => {contract_man, start_link, []}
        %},

        % Just run the query manager, while we migrate this project to new
        % smart contracts.
        #{
            id => query_man,
            start => {query_man, start_link, []}
        }

        % Maybe this should go after poll_keeper, since it sends things to
        % poll_keeper, or before, since poll_keeper calls it... Or we could use
        % the first call from poll_keeper to start the timer.
        %#{
        %    id => ground_truth,
        %    start => {ground_truth, start_link, []}
        %},

        %#{
        %    id => poll_keeper,
        %    start => {poll_keeper, start_link, []}
        %},

        %#{
        %    id => permissions,
        %    start => {permissions, start_link, []}
        %},
        %#{
        %    id => aev_auth,
        %    start => {aev_auth, start_link, []}
        %}
    ],
    {ok, {{one_for_one, 1, 5}, Procs}}.

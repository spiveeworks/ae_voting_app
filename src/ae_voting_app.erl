-module(ae_voting_app).
-behaviour(application).

-export([start/2]).
-export([stop/1]).

start(_Type, _Args) ->
    Dispatch = cowboy_router:compile([
        {'_', [
            {"/", hello_handler, []},
            {"/api/increaseCounter", restful, increase_counter}
        ]}
    ]),
    {ok, _} = cowboy:start_clear(my_http_listener,
        [{port, 8080}],
        #{env => #{dispatch => Dispatch}}
    ),
    {ok, Sup} = ae_voting_app_sup:start_link(),
    vanillae_man:ae_nodes([{"localhost",3013}]),
    fetch_polls(),
    {ok, Sup}.

fetch_polls() ->
    {ok, AACI} = vanillae:prepare_contract("contracts/Registry_Compiler_v6.aes"),
    io:format("~nAACI:~n~p~n", [AACI]),

    CallerID = "ak_2SG9SK3ZLtKvxuNfeQms8BMPR2sHPFYJgLz997CB5bnYQK7Kmx",
    ContractID = "ct_ouZib4wT9cNwgRA1pxgA63XEUd8eQRrG8PcePDEYogBc1VYTq",
    {ok, TX} = vanillae:contract_call(CallerID, AACI, ContractID, "polls", []),
    io:format("~nContract transaction:~n~p~n", [TX]),
    Result = vanillae:dry_run(TX),
    io:format("~nDry run result:~n~p~n", [Result]),
    ok.

stop(_State) ->
    ok.

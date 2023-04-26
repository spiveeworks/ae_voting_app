-module(ae_voting_app).
-behaviour(application).

-export([start/2]).
-export([stop/1]).

-export([create_poll_registry/0]).

-record(ak, {public :: string()}).

get_key() ->
    #ak{public = "ak_2SG9SK3ZLtKvxuNfeQms8BMPR2sHPFYJgLz997CB5bnYQK7Kmx"}.

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
    create_poll_registry(),
    %fetch_polls(),
    {ok, Sup}.

create_poll_registry() ->
    Key = get_key(),
    CreatorID = Key#ak.public,
    Path = "contracts/Registry_Compiler_v6.aes",

    {ok, CreateTX} = vanillae:contract_create(CreatorID, Path, []),
    io:format("~nCreate TX:~n~p~n", [CreateTX]),

    {ok, Result} = vanillae:dry_run(CreateTX),
    #{"results" := [#{"call_obj" := #{"contract_id" := ContractID}}]} = Result,
    io:format("~nDry run contract id: ~s~n", [ContractID]),

    ok.

fetch_polls() ->
    {ok, AACI} = vanillae:prepare_contract("contracts/Registry_Compiler_v6.aes"),
    io:format("~nAACI:~n~p~n", [AACI]),

    Key = get_key(),
    CallerID = Key#ak.public,
    ContractID = "ct_ouZib4wT9cNwgRA1pxgA63XEUd8eQRrG8PcePDEYogBc1VYTq",
    {ok, TX} = vanillae:contract_call(CallerID, AACI, ContractID, "polls", []),
    io:format("~nContract transaction:~n~p~n", [TX]),
    Result = vanillae:dry_run(TX),
    io:format("~nDry run result:~n~p~n", [Result]),
    ok.

stop(_State) ->
    ok.

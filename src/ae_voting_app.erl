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
    vanillae_man:ae_nodes(seed_nodes()),
    fetch_polls(),
    {ok, Sup}.

% These are the seeds that new aeternity nodes connect to first.
% https://github.com/aeternity/aeternity/blob/master/config/sys.config#L52-L69
seed_nodes() ->
    [{"52.220.198.72",3013},
     {"18.217.69.24",3013},
     {"3.17.15.122",3013},
     {"35.166.231.86",3013},
     {"52.11.110.179",3013},
     {"13.250.144.60",3013},
     {"3.17.15.239",3013},
     {"52.26.157.37",3013},
     {"13.228.202.140",3013},
     {"13.53.114.199",3013},
     {"13.53.213.137",3013},
     {"13.53.78.163",3013}
    ].

fetch_polls() ->
    {ok, AACI} = vanillae:prepare_contract("contracts/Registry_Compiler_v6.aes"),
    io:format("AACI:~n~p~n", [AACI]),

    CallerID = "ak_2SG9SK3ZLtKvxuNfeQms8BMPR2sHPFYJgLz997CB5bnYQK7Kmx",
    ContractID = "ct_ouZib4wT9cNwgRA1pxgA63XEUd8eQRrG8PcePDEYogBc1VYTq",
    case vanillae:contract_call(CallerID, AACI, ContractID, "polls", []) of
        {error, _} -> io:format("Contract call failed to build.~n");
        TX -> io:format("Contract transaction:~n~p~n", [TX])
    end,
    ok.

stop(_State) ->
    ok.

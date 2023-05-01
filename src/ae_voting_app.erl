-module(ae_voting_app).
-behaviour(application).

-export([start/0, start/2]).
-export([stop/1]).

start() ->
    application:ensure_all_started(?MODULE).

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

    vanillae:ae_nodes([{"localhost",3013}]),
    vanillae:network_id("ae_uat"),

    ae_voting_app_sup:start_link().

stop(_State) ->
    ok.

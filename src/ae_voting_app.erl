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

stop(_State) ->
    ok.

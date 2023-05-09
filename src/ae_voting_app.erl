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

    Nodes = peer_list("peerlist"),
    vanillae:ae_nodes(Nodes),
    %vanillae:ae_nodes([{"localhost",3013}]),
    vanillae:network_id("ae_uat"),

    ae_voting_app_sup:start_link().

peer_list(Path) ->
    case file:read_file(Path) of
        {ok, Str} ->
            Lines = string:lexemes(Str, [$\r, $\n]),
            peer_list_each(Lines, []);
        _ -> [{"localhost", 3013}]
    end.

peer_list_each([Next | Rest], Acc) ->
    case string:split(Next, "@") of
        [_, AddrPort] ->
            case string:split(AddrPort, ":") of
                [IP, <<"3015">>] ->
                    Peer = {unicode:characters_to_list(IP), 3013},
                    peer_list_each(Rest, [Peer | Acc]);
                It ->
                    io:format("It: ~p~n", [It]),
                    peer_list_each(Rest, Acc)
            end;
        _ -> peer_list_each(Rest, Acc)
    end;
peer_list_each([], []) ->
    [{"localhost", 3013}];
peer_list_each([], Acc) ->
    lists:reverse(Acc).

stop(_State) ->
    ok.

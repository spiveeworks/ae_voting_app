-module(ae_voting_app).
-behaviour(application).

-export([start/0, start/2]).
-export([stop/1]).

start() ->
    application:ensure_all_started(?MODULE).

start(_Type, _Args) ->
    Dispatch = cowboy_router:compile([
        {'_', [
            {"/[polls]", cowboy_static, {file, "site/index.html"}},
            {"/polls/:id", [{id, int}], cowboy_static, {file, "site/show_poll.html"}},
            {"/polls/:poll/option/:option", [{poll, int}, {option, int}], cowboy_static, {file, "site/show_option.html"}},
            {"/polls/admin", cowboy_static, {file, "site/poll_admin.html"}},
            {"/polls/create", cowboy_static, {file, "site/create_poll.html"}},
            {"/counter", cowboy_static, {file, "site/counter.html"}},
            {"/sidekick.js", cowboy_static, {file, "site/sidekick.js"}},
            {"/sidekick.js.map", cowboy_static, {file, "site/sidekick.js.map"}},
            {"/aev_util.js", cowboy_static, {file, "site/aev_util.js"}},
            {"/api/increaseCounter", aev_json_counter, increase_counter},
            {"/api/getPolls", aev_json_polls, get_polls},
            {"/api/poll/:id", [{id, int}], aev_json_polls, get_poll_info},
            {"/api/poll/:poll/user/:user", [{poll, int}], aev_json_polls, get_user_status},
            {"/api/poll/:poll/option/:option", [{poll, int}, {option, int}], aev_json_polls, get_option_info},
            % TODO: Should these be in /api/poll/:poll as well?
            {"/api/vote/formTransaction", aev_json_client, form_vote_tx},
            {"/api/vote/postTransaction", aev_json_client, post_vote_tx},
            {"/api/revokeVote/formTransaction", aev_json_client, form_revoke_vote_tx},
            {"/api/revokeVote/postTransaction", aev_json_client, post_revoke_vote_tx},

            {"/api/getSettings/formMessage", aev_json_admin, get_settings_form_message},
            {"/api/getSettings/submitSig", aev_json_admin, get_settings},
            {"/api/setPollCategory/formMessage", aev_json_admin, filter_poll_form_message},
            {"/api/setPollCategory/submitSig", aev_json_admin, filter_poll},
            {"/api/setAccountCategory/formMessage", aev_json_admin, filter_account_form_message},
            {"/api/setAccountCategory/submitSig", aev_json_admin, filter_account},
            {"/api/setAccountPermissions/formMessage", aev_json_admin, set_permissions_form_message},
            {"/api/setAccountPermissions/submitSig", aev_json_admin, set_permissions},

            {"/api/createPoll/formTransaction", aev_json_admin, form_poll_tx},
            {"/api/createPoll/postTransaction", aev_json_admin, post_poll_tx},
            {"/api/unregisteredPolls/formMessage", aev_json_admin, get_polls_form_message},
            {"/api/unregisteredPolls/submitSig", aev_json_admin, get_polls},
            {"/api/registerPoll/formTransaction", aev_json_admin, form_register_tx},
            {"/api/registerPoll/postTransaction", aev_json_admin, post_register_tx},
            {"/api/unregisteredPolls/count", aev_json_admin, unregistered_polls_count}
        ]}
    ]),
    {ok, _} = cowboy:start_clear(my_http_listener,
        [{port, 8080}],
        #{env => #{dispatch => Dispatch}}
    ),

    Nodes = peer_list("peerlist"),
    vanillae:ae_nodes(Nodes),
    %vanillae:ae_nodes([{"localhost",3013}]),
    %vanillae:ae_nodes([{"testnet.aeternity.io",3013}]),
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

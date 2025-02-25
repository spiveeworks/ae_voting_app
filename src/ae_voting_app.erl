-module(ae_voting_app).
-behaviour(application).

-export([start/0, start/2]).
-export([stop/1]).

start() ->
    application:ensure_all_started(?MODULE).

start(_Type, _Args) ->
    application:ensure_started(hz),
    application:ensure_started(cowboy),

    {Network, TLS, Nodes} = network(),
    hz:network_id(Network),
    hz:tls(TLS),
    hz:chain_nodes(Nodes),

    {ok, SupRoot} = ae_voting_app_sup:start_link(),

    Dispatch = cowboy_router:compile([
        {'_', [
            {"/[polls]", cowboy_static, {file, "site/index.html"}},
            {"/polls/:id", [{id, int}], cowboy_static, {file, "site/show_poll.html"}},
            {"/polls/:poll/option/:option", [{poll, int}, {option, int}], cowboy_static, {file, "site/show_option.html"}},
            {"/polls/admin", cowboy_static, {file, "site/poll_admin.html"}},
            {"/polls/create", cowboy_static, {file, "site/create_poll.html"}},
            {"/sidekick.js", cowboy_static, {file, "site/sidekick.js"}},
            {"/sidekick.js.map", cowboy_static, {file, "site/sidekick.js.map"}},
            {"/aev_util.js", cowboy_static, {file, "site/aev_util.js"}},
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
            {"/api/createPoll/postTransaction", aev_json_admin, post_poll_tx}
        ]}
    ]),
    {ok, _} = cowboy:start_clear(my_http_listener,
        [{port, 8080}],
        #{env => #{dispatch => Dispatch}}
    ),

    {ok, SupRoot}.

network() ->
    case file:consult("peerlist") of
        {ok, [{network, Network}, {tls, TLS}, {peers, Peers}]} ->
            {Network, TLS, Peers};
        {ok, _} ->
            io:format("Error reading peerlist. Defaulting to ae_uat on localhost.~n", []),
            {"ae_uat", false, [{"localhost", 3013}]};
        {error, enoent} ->
            {"ae_uat", false, [{"localhost", 3013}]}
    end.

stop(_State) ->
    ok.

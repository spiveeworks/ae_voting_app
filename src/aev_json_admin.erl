-module(aev_json_admin).
-behavior(cowboy_handler).

-export([init/2, allowed_methods/2]).
-export([content_types_provided/2, content_types_accepted/2, handle_post/2]).

init(Req, State) ->
    {cowboy_rest, Req, State}.

allowed_methods(Req, State) ->
    {[<<"POST">>, <<"OPTIONS">>], Req, State}.

content_types_provided(Req, State) ->
    Accepted = [
        {{<<"application">>, <<"json">>, '*'}, handle_get}
    ],
    {Accepted, Req, State}.

content_types_accepted(Req, State) ->
    Accepted = [
        {{<<"application">>, <<"json">>, '*'}, handle_post}
    ],
    {Accepted, Req, State}.

handle_post(Req, State = get_settings_form_message) ->
    form_message(Req, State, fun get_settings_convert/2,
                 fun get_settings_payload/1);
handle_post(Req, State = get_settings) ->
    check_signature(Req, State, fun get_settings_convert/2,
                    fun get_settings_payload/1, fun get_settings/1);
handle_post(Req, State = filter_poll_form_message) ->
    form_message(Req, State, fun filter_poll_convert/2,
                 fun filter_poll_payload/1);
handle_post(Req, State = filter_poll) ->
    check_signature(Req, State, fun filter_poll_convert/2,
                    fun filter_poll_payload/1, fun filter_poll/1);
handle_post(Req, State = filter_account_form_message) ->
    form_message(Req, State, fun filter_account_convert/2,
                 fun filter_account_payload/1);
handle_post(Req, State = filter_account) ->
    check_signature(Req, State, fun filter_account_convert/2,
                    fun filter_account_payload/1, fun filter_account/1);
handle_post(Req, State = set_permissions_form_message) ->
    form_message(Req, State, fun set_permissions_convert/2,
                 fun set_permissions_payload/1);
handle_post(Req, State = set_permissions) ->
    check_signature(Req, State, fun set_permissions_convert/2,
                    fun set_permissions_payload/1, fun set_permissions/1).

%%%%%%%%%%%%%%%%%
% Generic Logic

form_message(Req0, State, Convert, FormPayload) ->
    case aev_json_parse:parse_req_body(Req0) of
        {ok, A = #{<<"address">> := ID}, Req1} when is_map(A) ->
            B = maps:remove(<<"address">>, A),
            case Convert(ID, B) of
                {ok, C} ->
                    Payload = FormPayload(C),
                    reply_message(Req1, State, Payload, ID);
                error ->
                    {false, Req1, State}
            end;
        {ok, A, Req1} ->
            io:format("Invalide data received: ~p~n", [A]),
            {false, Req1, State};
        {error, Req1} ->
            io:format("Failure.~n", []),
            {false, Req1, State}
    end.

check_signature(Req0, State, Convert, FormPayload, Action) ->
    case aev_json_parse:parse_req_body(Req0) of
        {ok, A = #{<<"address">> := ID,
                   <<"timestamp">> := Timestamp,
                   <<"nonce">> := Nonce,
                   <<"message_signature">> := Signature}, Req1} ->
            B = maps:without([<<"address">>, <<"timestamp">>, <<"nonce">>, <<"message_signature">>], A),
            check_signature2(Req1, State, Convert, FormPayload, Action, ID,
                             Timestamp, Nonce, Signature, B);
        {ok, Body, Req1} ->
            io:format("Invalid data received: ~p~n", [Body]),
            {false, Req1, State};
        {error, Req1} ->
            io:format("Failure.~n", []),
            {false, Req1, State}
    end.

check_signature2(Req, State, Convert, FormPayload, Action, ID,
                 Timestamp, Nonce, Signature, B) ->
    case Convert(ID, B) of
        {ok, C} ->
            Payload = FormPayload(C),
            check_signature3(Req, State, Action, ID, Timestamp, Nonce,
                             Signature, C, Payload);
        error ->
            {false, Req, State}
    end.

check_signature3(Req0, State, Action, ID, Timestamp, Nonce,
                 Signature, C, Payload) ->
    case aev_auth:verify_sig(Payload, ID, Timestamp, Nonce, Signature) of
        ok ->
            Result = Action(C),
            Data = zj:encode(Result),
            Req1 = cowboy_req:set_resp_body(Data, Req0),
            {true, Req1, State};
        error ->
            {false, Req0, State}
    end.

reply_message(Req0, State, Payload, ID) ->
    {ok, Timestamp, Nonce, Message} = aev_auth:start_auth(Payload, ID, ttl()),
    Data = zj:encode(#{timestamp => Timestamp, nonce => Nonce, message => Message}),
    Req1 = cowboy_req:set_resp_body(Data, Req0),
    {true, Req1, State}.

ttl() ->
    60000.

%%%%%%%%%%%%%%%%%%%%
% Setting Fetching

get_settings_convert(ID, Body) when Body == #{} ->
    case permissions:can_set_categories(ID) of
        true -> {ok, {}};
        false -> error
    end;
get_settings_convert(_, _) ->
    error.

get_settings_payload(_) ->
    "view poll and account settings".

get_settings(_) ->
    {AF, CF} = poll_keeper:get_filters(),
    AFNamed = maps:map(fun map_id_to_name/2, AF),
    CFNamed = maps:map(fun map_id_to_name/2, CF),

    Permissions = permissions:get_all(),
    PermissionsNamed = maps:map(fun map_permission_id_to_name/2, Permissions),

    #{poll_categories => CFNamed,
      account_categories => AFNamed,
      account_permissions => PermissionsNamed}.

map_id_to_name(_, default) ->
    default;
map_id_to_name(_, ID) ->
    aev_category_names:from_id(ID).

map_permission_id_to_name(_, ID) ->
    aev_category_names:permissions_from_id(ID).

%%%%%%%%%%%%%%%%%%%
% Poll Categories

% TODO: check that nothing else is in the map?
filter_poll_convert(ID, Body) ->
    case Body of
        #{<<"poll_id">> := Poll,
          <<"category">> := Category} ->
            case permissions:can_set_categories(ID) of
                false -> error;
                true ->
                    case Category of
                        <<"default">> ->
                            {ok, {Poll, Category, none}};
                        _ ->
                            case aev_category_names:to_id(Category) of
                                {ok, CategoryID} ->
                                    {ok, {Poll, Category, CategoryID}};
                                error ->
                                    error
                            end
                    end
            end;
        _ ->
            io:format("Invalid data received: ~p~n", [Body]),
            error
    end.

filter_poll_payload({PollIndex, Category, _}) ->
    {ok, PollID} = poll_keeper:get_poll_address(PollIndex),
    % There might be a slight risk that two different poll sites will give the
    % same nonce for the same poll address, and that the user will categorise a
    % poll on one site, and that signature gets used to sign a transaction on a
    % different site??? But that's not a big deal when we are just filtering
    % polls, lol.
    Txt = io_lib:format("set category of poll ~s to ~s", [PollID, Category]),
    unicode:characters_to_list(Txt).

filter_poll({Poll, <<"default">>, _}) ->
    poll_keeper:filter_poll_remove(Poll),
    #{};
filter_poll({Poll, _, CategoryID}) ->
    poll_keeper:filter_poll(Poll, CategoryID),
    #{}.

%%%%%%%%%%%%%%%%%%%%%%
% Account Categories

filter_account_convert(ID, Body) ->
    case Body of
        #{<<"account">> := Account,
          <<"category">> := Category} ->
            case permissions:can_set_categories(ID) of
                false -> error;
                true ->
                    case aev_category_names:to_id(Category) of
                        {ok, CategoryID} ->
                            {ok, {Account, Category, CategoryID}};
                        error ->
                            error
                    end
            end;
        _ ->
            io:format("Invalid data received: ~p~n", [Body]),
            error
    end.

filter_account_payload({Account, Category, _}) ->
    Txt = io_lib:format("set category of account ~s to ~s", [Account, Category]),
    unicode:characters_to_list(Txt).

filter_account({Account, _, CategoryID}) ->
    poll_keeper:filter_account(Account, CategoryID),
    #{}.

%%%%%%%%%%%%%%%%%%%%%%%
% Account Permissions

set_permissions_convert(ID, Body) ->
    case Body of
        #{<<"account">> := Account,
          <<"permission_level">> := PermissionLevelName} ->
            case permissions:can_change_permissions(ID) of
                false -> error;
                true ->
                    case aev_category_names:permissions_to_id(PermissionLevelName) of
                        {ok, PermissionLevel} ->
                            {ok, {Account, PermissionLevelName, PermissionLevel}};
                        error ->
                            error
                    end
            end;
        _ ->
            io:format("Invalid data received: ~p~n", [Body]),
            error
    end.

set_permissions_payload({Account, PermissionLevel, _}) ->
    Txt = io_lib:format("set permission level of account ~s to ~s", [Account, PermissionLevel]),
    unicode:characters_to_list(Txt).

set_permissions({Account, _, PermissionLevel}) ->
    permissions:set(Account, PermissionLevel),
    #{}.


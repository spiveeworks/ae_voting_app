-module(aev_json_admin).
-behavior(cowboy_handler).

-export([init/2, allowed_methods/2]).
-export([content_types_provided/2, content_types_accepted/2, handle_get/2, handle_post/2]).

init(Req, State) ->
    {cowboy_rest, Req, State}.

allowed_methods(Req, State = get_filters) ->
    {[<<"GET">>, <<"OPTIONS">>], Req, State};
allowed_methods(Req, State) ->
    {[<<"POST">>, <<"OPTIONS">>], Req, State}.

content_types_provided(Req, State) ->
    Accepted = [
        {{<<"application">>, <<"json">>, '*'}, handle_get}
    ],
    {Accepted, Req, State}.

handle_get(Req, State = get_filters) ->
    {AF, CF} = poll_keeper:get_filters(),
    AFNamed = maps:map(fun map_id_to_name/2, AF),
    CFNamed = maps:map(fun map_id_to_name/2, CF),
    Data = zj:encode(#{account_categories => AFNamed,
                       poll_categories => CFNamed}),
    {Data, Req, State}.

map_id_to_name(_, default) ->
    default;
map_id_to_name(_, ID) ->
    aev_category_names:from_id(ID).

content_types_accepted(Req, State) ->
    Accepted = [
        {{<<"application">>, <<"json">>, '*'}, handle_post}
    ],
    {Accepted, Req, State}.

handle_post(Req, State = filter_poll_form_message) ->
    filter_poll_form_message(Req, State);
handle_post(Req, State = filter_poll) ->
    filter_poll(Req, State);
handle_post(Req, State = filter_account_form_message) ->
    filter_account_form_message(Req, State);
handle_post(Req, State = filter_account) ->
    filter_account(Req, State).

%%%%%%%%%%%%%%%%%%%
% Poll Categories

filter_poll_form_message(Req0, State) ->
    case aev_json_parse:parse_req_body(Req0) of
        {ok, #{<<"poll_id">> := Poll,
               <<"category">> := Category,
               <<"address">> := ID}, Req1} ->
            case permissions:can_set_categories(ID) of
                false -> {false, Req1, State};
                true ->
                    Payload = filter_poll_payload(Poll, Category),
                    reply_message(Req1, State, Payload, ID)
            end;
        {ok, Body, Req1} ->
            io:format("Invalid data received: ~p~n", [Body]),
            {false, Req1, State};
        {error, Req1} ->
            io:format("Failure.~n", []),
            {false, Req1, State}
    end.

filter_poll(Req0, State) ->
    case aev_json_parse:parse_req_body(Req0) of
        {ok, #{<<"poll_id">> := Poll,
               <<"category">> := Category,
               <<"address">> := ID,
               <<"timestamp">> := Timestamp,
               <<"nonce">> := Nonce,
               <<"message_signature">> := Signature}, Req1} ->
            filter_poll2(Req1, State, Poll, Category, ID, Timestamp, Nonce,
                         Signature);
        {ok, Body, Req1} ->
            io:format("Invalid data received: ~p~n", [Body]),
            {false, Req1, State};
        {error, Req1} ->
            io:format("Failure.~n", []),
            {false, Req1, State}
    end.

filter_poll2(Req0, State, Poll, CategoryName, ID, Timestamp, Nonce, Signature) ->
    case CategoryName of
        <<"default">> ->
            filter_poll_remove(Req0, State, Poll, ID,
                               Timestamp, Nonce, Signature);
        _ ->
            filter_poll3(Req0, State, Poll, CategoryName, ID, Timestamp, Nonce,
                         Signature)
    end.

% TODO: we could combine these commands together a little, since they are quite
%       redundant
% TODO: we could move this stuff over to admin_ops.erl once all the pieces have
%       been decoded
filter_poll3(Req0, State, Poll, CategoryName, ID, Timestamp, Nonce, Signature) ->
    case aev_category_names:to_id(CategoryName) of
        {ok, Category} ->
            filter_poll4(Req0, State, Poll, CategoryName, Category, ID,
                         Timestamp, Nonce, Signature);
        error ->
            {false, Req0, State}
    end.

filter_poll_remove(Req0, State, Poll, ID, Timestamp, Nonce, Signature) ->
    Message = filter_poll_payload(Poll, "default"),
    case aev_auth:verify_sig(Message, ID, Timestamp, Nonce, Signature) of
        ok ->
            poll_keeper:filter_poll_remove(Poll),
            Req1 = cowboy_req:set_resp_body("{}", Req0),
            {true, Req1, State};
        error ->
            {false, Req0, State}
    end.

filter_poll4(Req0, State, Poll, CategoryName, Category, ID, Timestamp, Nonce, Signature) ->
    Message = filter_poll_payload(Poll, CategoryName),
    case aev_auth:verify_sig(Message, ID, Timestamp, Nonce, Signature) of
        ok ->
            poll_keeper:filter_poll(Poll, Category),
            Req1 = cowboy_req:set_resp_body("{}", Req0),
            {true, Req1, State};
        error ->
            {false, Req0, State}
    end.

filter_poll_payload(PollIndex, Category) ->
    {ok, PollID} = poll_keeper:get_poll_address(PollIndex),
    % There might be a slight risk that two different poll sites will give the
    % same nonce for the same poll address, and that the user will categorise a
    % poll on one site, and that signature gets used to sign a transaction on a
    % different site??? But that's not a big deal when we are just filtering
    % polls, lol.
    Txt = io_lib:format("set category of poll ~s to ~s", [PollID, Category]),
    unicode:characters_to_list(Txt).

%%%%%%%%%%%%%%%%%%%%%%
% Account Categories

filter_account_form_message(Req0, State) ->
    case aev_json_parse:parse_req_body(Req0) of
        {ok, #{<<"account">> := Account,
               <<"category">> := Category,
               <<"address">> := ID}, Req1} ->
            case permissions:can_set_categories(ID) of
                false -> {false, Req1, State};
                true ->
                    Payload = filter_account_payload(Account, Category),
                    reply_message(Req1, State, Payload, ID)
            end;
        {ok, Body, Req1} ->
            io:format("Invalid data received: ~p~n", [Body]),
            {false, Req1, State};
        {error, Req1} ->
            io:format("Failure.~n", []),
            {false, Req1, State}
    end.

filter_account(Req0, State) ->
    case aev_json_parse:parse_req_body(Req0) of
        {ok, #{<<"account">> := Account,
               <<"category">> := Category,
               <<"address">> := ID,
               <<"timestamp">> := Timestamp,
               <<"nonce">> := Nonce,
               <<"message_signature">> := Signature}, Req1} ->
            filter_account2(Req1, State, Account, Category, ID, Timestamp, Nonce,
                         Signature);
        {ok, Body, Req1} ->
            io:format("Invalid data received: ~p~n", [Body]),
            {false, Req1, State};
        {error, Req1} ->
            io:format("Failure.~n", []),
            {false, Req1, State}
    end.

filter_account2(Req0, State, Account, CategoryName, ID, Timestamp, Nonce, Signature) ->
    case aev_category_names:to_id(CategoryName) of
        {ok, Category} ->
            filter_account3(Req0, State, Account, CategoryName, Category, ID,
                         Timestamp, Nonce, Signature);
        error ->
            {false, Req0, State}
    end.

filter_account3(Req0, State, Account, CategoryName, Category, ID, Timestamp, Nonce, Signature) ->
    Message = filter_account_payload(Account, CategoryName),
    case aev_auth:verify_sig(Message, ID, Timestamp, Nonce, Signature) of
        ok ->
            poll_keeper:filter_account(Account, Category),
            Req1 = cowboy_req:set_resp_body("{}", Req0),
            {true, Req1, State};
        error ->
            {false, Req0, State}
    end.

filter_account_payload(Account, Category) ->
    Txt = io_lib:format("set category of account ~s to ~s", [Account, Category]),
    unicode:characters_to_list(Txt).

%%%%%%%%%%
% Common

reply_message(Req0, State, Payload, ID) ->
    {ok, Timestamp, Nonce, Message} = aev_auth:start_auth(Payload, ID, ttl()),
    Data = zj:encode(#{timestamp => Timestamp, nonce => Nonce, message => Message}),
    Req1 = cowboy_req:set_resp_body(Data, Req0),
    {true, Req1, State}.

ttl() ->
    60000.


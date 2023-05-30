-module(aev_json_client).
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

handle_post(Req, State = form_vote_tx) ->
    form_vote_tx(Req, State);
handle_post(Req, State = post_vote_tx) ->
    post_vote_tx(Req, State).

form_vote_tx(Req0, State) ->
    case aev_json_parse:parse_req_body(Req0) of
        {ok, #{<<"poll_id">> := Poll,
               <<"option_id">> := Option,
               <<"address">> := ID}, Req1} ->
            form_vote_tx2(Req1, State, ID, Poll, Option);
        {ok, Body, Req1} ->
            io:format("Invalid data received: ~p~n", [Body]),
            {false, Req1, State};
        {error, Req1} ->
            io:format("Failure.~n", []),
            {false, Req1, State}
    end.

form_vote_tx2(Req0, State, ID, Poll, Option) ->
    case client_ops:vote_tx(ID, Poll, Option) of
        {ok, TX} ->
            Data = zj:encode(#{tx => TX}),
            Req1 = cowboy_req:set_resp_body(Data, Req0),
            {true, Req1, State};
        {error, _} ->
            {false, Req0, State}
    end.

post_vote_tx(Req0, State) ->
    case aev_json_parse:parse_req_body(Req0) of
        {ok, #{<<"poll_id">> := Poll,
               <<"option_id">> := Option,
               <<"address">> := ID,
               <<"signed_tx">> := SignedTX}, Req1} ->
            post_vote_tx2(Req1, State, ID, Poll, Option, SignedTX);
        {ok, Body, Req1} ->
            io:format("Invalid data received: ~p~n", [Body]),
            {false, Req1, State};
        {error, Req1} ->
            io:format("Failure.~n", []),
            {false, Req1, State}
    end.

post_vote_tx2(Req0, State, ID, Poll, Option, SignedTX) ->
    case vanillae:post_tx(SignedTX) of
        {ok, #{"tx_hash" := TH}} ->
            poll_keeper:track_vote(ID, Poll, Option, TH),
            Data = zj:encode(#{}),
            Req1 = cowboy_req:set_resp_body(Data, Req0),
            {true, Req1, State};
        Error ->
            io:format("post_tx failed with ~p~n", [Error]),
            {false, Req0, State}
    end.

% Encode a list of all polls


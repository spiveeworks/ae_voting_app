-module(aev_json_counter).
-behavior(cowboy_handler).

-export([init/2, allowed_methods/2]).
-export([content_types_accepted/2, from_json/2]).
-export([content_types_provided/2, to_json/2]).

init(Req, State) ->
    {cowboy_rest, Req, State}.

allowed_methods(Req, State) ->
    {[<<"GET">>, <<"POST">>, <<"OPTIONS">>], Req, State}.

encode_counter() ->
    Counter = counter:read_counter(),
    zj:binary_encode(#{value => Counter}).

content_types_accepted(Req, State) ->
    Accepted = [
        {{<<"application">>, <<"json">>, '*'}, from_json}
    ],
    {Accepted, Req, State}.

from_json(Req0, State) ->
    Req1 = case aev_json_parse:parse_req_body(Req0) of
        {ok, #{<<"count">> := Count}, Req} ->
            counter:increase_counter(Count),
            Req;
        {ok, Body, Req} ->
            io:format("Invalid data received: ~p~n", [Body]),
            Req;
        {error, Req} ->
            io:format("Failure.~n", []),
            Req
    end,
    Data = encode_counter(),
    Req2 = cowboy_req:set_resp_body(Data, Req1),
    {true, Req2, State}.

content_types_provided(Req, State) ->
    Provided = [
        %{{<<"text">>, <<"html">>, '*'}, to_html},
        {{<<"application">>, <<"json">>, '*'}, to_json}
    ],
    {Provided, Req, State}.

to_json(Req, State) ->
    Data = encode_counter(),
    {Data, Req, State}.


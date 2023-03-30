-module(restful).
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

reverse_flatten_binaries(Binaries) ->
    lists:foldl(fun (It, Acc) -> <<It/binary, Acc/binary>> end, <<>>, Binaries).

read_req_body(Req) ->
    read_req_body(Req, []).
read_req_body(Req0, SoFar) ->
    case cowboy_req:read_body(Req0) of
        {ok, Next, Req1} ->
            Result = reverse_flatten_binaries([Next | SoFar]),
            {Result, Req1};
        {more, Next, Req1} ->
            read_req_body(Req1, [Next | SoFar])
    end.

parse_req_body(Req0) ->
    {JSON, Req1} = read_req_body(Req0),
    case zj:binary_decode(JSON) of
        {ok, Body} -> {ok, Body, Req1};
        {error, _, _} -> {error, Req1};
        {incomplete, _, _} -> {error, Req1}
    end.

from_json(Req0, State) ->
    Req1 = case parse_req_body(Req0) of
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


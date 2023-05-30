-module(aev_json_parse).

-export([read_req_body/1, parse_req_body/1]).

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

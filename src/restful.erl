-module(restful).
-behavior(cowboy_handler).

-export([init/2, allowed_methods/2]).
-export([content_types_accepted/2, from_json/2]).
-export([content_types_provided/2, to_html/2, to_json/2]).

init(Req, State) ->
    {cowboy_rest, Req, State}.

allowed_methods(Req, State) ->
    {[<<"GET">>, <<"POST">>, <<"OPTIONS">>], Req, State}.

encode_counter_as_body(Req0, _State) ->
    Data = zj:binary_encode(#{value => <<"hello">>}),
    Req1 = cowboy_req:set_resp_body(Data, Req0),
    Req1.

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
        {ok, Body, Req} ->
            io:format("Success: ~p~n", [Body]),
            Req;
        {error, Req} ->
            io:format("Failure.~n", []),
            Req
    end,
    Req2 = encode_counter_as_body(Req1, State),
    {true, Req2, State}.

content_types_provided(Req, State) ->
    Provided = [
        %{{<<"text">>, <<"html">>, '*'}, to_html},
        {{<<"application">>, <<"json">>, '*'}, to_json}
    ],
    {Provided, Req, State}.

to_html(Req0, State) ->
    Req1 = cowboy_req:reply(200,
        #{<<"content-type">> => <<"text/html">>},
         <<"<button type=\"button\">Click Me!</button>">>,
        Req0),
    {ok, Req1, State}.

to_json(Req0, State) ->
    Req1 = encode_counter_as_body(Req0, State),
    Req2 = cowboy_req:reply(200,
        #{<<"content-type">> => <<"application/json">>},
        % body given by encode_counter_as_body
        Req1),
    {ok, Req2, State}.


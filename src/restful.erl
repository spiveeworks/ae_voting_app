-module(restful).
-behavior(cowboy_handler).

-export([init/2, content_types_provided/2, to_html/2, to_json/2]).

init(Req, State) ->
    {cowboy_rest, Req, State}.

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
    Data = zj:binary_encode(#{value => <<"hello">>}),
    Req1 = cowboy_req:reply(200,
        #{<<"content-type">> => <<"application/json">>},
        Data,
        Req0),
    {ok, Req1, State}.


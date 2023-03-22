-module(hello_handler).
-behavior(cowboy_handler).

-export([init/2]).

init(Req0, State) ->
    Req1 = cowboy_req:reply(200,
        #{<<"content-type">> => <<"text/html">>},
         <<"<button type=\"button\">Click Me!</button>">>,
        Req0),
    {ok, Req1, State}.

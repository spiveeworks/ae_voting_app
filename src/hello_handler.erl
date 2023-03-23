-module(hello_handler).
-behavior(cowboy_handler).

-export([init/2]).

init(Req0, State) ->
    Req1 = cowboy_req:reply(200,
        #{<<"content-type">> => <<"text/html">>},
        <<"
            <!DOCTYPE html>
            <html>
            <body>

            <script>
                async function buttonClicked() {
                    json = await fetch(\"./api\");
                    object = await json.json();

                    text = document.getElementById(\"text\");
                    text.innerHTML = object.value;
                }
            </script>

            <p id=\"text\">text</p>
            <button onclick=\"buttonClicked()\">Click</button>

            </body>
            </html>
        ">>,
        Req0),
    {ok, Req1, State}.

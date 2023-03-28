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
                    text.innerHTML = \"failed?\";
                    json = await fetch(
                        \"./api/increaseCounter\",
                        {
                            method: \"post\",
                            headers: {
                                'Accept': 'application/json',
                                'Content-Type': 'application/json'
                            },
                            body: JSON.stringify({count: 1})
                        }
                    );
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

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
                function updateText(count) {
                    text = document.getElementById(\"text\");
                    text.innerHTML = \"The button has been clicked \"
                        + count + \" times.\";
                }

                async function buttonClicked() {
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
                    updateText(object.value);
                }

                async function updateCounter() {
                    json = await fetch(\"./api/increaseCounter\");
                    object = await json.json();
                    updateText(object.value);
                }

                window.onload = updateCounter;
            </script>

            <p id=\"text\">Loading...</p>
            <button onclick=\"buttonClicked()\">Click</button>

            </body>
            </html>
        ">>,
        Req0),
    {ok, Req1, State}.

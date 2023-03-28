-module(counter).
-behaviour(gen_server).

-export([start_link/1, increase_counter/1, read_counter/0]).
-export([init/1, handle_call/3, handle_cast/2]).

-spec start_link(Counter :: integer()) ->
    ReturnStatus :: gen_server:start_ret().

start_link(Counter) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Counter, []).

read_counter() ->
    gen_server:call(?MODULE, read_counter).

increase_counter(Value) ->
    gen_server:cast(?MODULE, {increase_counter, Value}).

init(Counter) ->
    {ok, Counter}.

handle_call(read_counter, _, Counter) ->
    {reply, Counter, Counter}.

handle_cast({increase_counter, Value}, Counter) ->
    {noreply, Counter + Value}.


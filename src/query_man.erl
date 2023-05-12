-module(query_man).
-behaviour(gen_server).

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2]).

-spec start_link() ->
    ReturnStatus :: gen_server:start_ret().

-record(qms, {}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, {}, []).

init({}) ->
    {ok, #qms{}}.

handle_call(_, _, State) ->
    {reply, ok, State}.

handle_cast(_, State) ->
    {noreply, State}.


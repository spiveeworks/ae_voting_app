-module(incubator).
-behaviour(gen_server).

-export([start_link/0, add_poll_hash/2, get_state/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2]).

%%
% State
%%

-record(pending_poll,
        {title :: string(),
         address :: {pending, vanillae:tx_hash()} | {created, vanillae:contract_id()},
         register_hash = none :: none | vanillae:tx_hash()}).

-type pending() :: [#pending_poll{}].

%%
% External Interface
%%

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, {}, []).

add_poll_hash(Title, TH) ->
    gen_server:cast(?MODULE, {add_poll_hash, Title, TH}).

get_state() ->
    gen_server:call(?MODULE, get_state).

%%
% Callbacks
%%

-spec init({}) -> {ok, pending()}.

init({}) ->
    {ok, []}.

handle_call(get_state, _, State) ->
    {reply, State, State}.

handle_cast({add_poll_hash, Title, TH}, State) ->
    NewState = do_add_poll_hash(Title, TH, State),
    {noreply, NewState}.

handle_info({subscribe_tx, {create_poll, Title, TH}, {ok, Contract}}, State) ->
    io:format("Poll created: ~p~n", [Contract]),
    NewState = do_poll_created(Title, TH, Contract, State),
    {noreply, NewState};
handle_info({subscribe_tx, {create_poll, _Title, TH}, {error, Reason}}, State) ->
    io:format("vote failed: ~p~n", [Reason]),
    NewState = remove_pending(State, TH),
    {noreply, NewState}.

%%
% Doers
%%

do_add_poll_hash(Title, TH, State) ->
    io:format("received hash: ~s~n", [TH]),
    query_man:subscribe_tx_contract(self(), {create_poll, Title, TH}, TH),
    NewPoll = #pending_poll{title = Title, address = {pending, TH}},
    [NewPoll | State].

do_poll_created(Title, TH, Contract, State) ->
    StateWithout = remove_pending(State, TH),
    NewPoll = #pending_poll{title = Title, address = {created, Contract}},
    [NewPoll | StateWithout].

remove_pending(State, TH) ->
    remove_pending(State, TH, []).

remove_pending([#pending_poll{address = {pending, TH}} | Remaining], TH, Acc) ->
    remove_pending(Remaining, TH, Acc);
remove_pending([Poll | Remaining], TH, Acc) ->
    remove_pending(Remaining, TH, [Poll | Acc]);
remove_pending([], _, Acc) ->
    lists:reverse(Acc).


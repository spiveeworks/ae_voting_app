-module(incubator).
-behaviour(gen_server).

-export([start_link/0, add_poll_hash/2, add_register_hash/3, get_state/0]).
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

add_register_hash(Title, Contract, TH) ->
    gen_server:cast(?MODULE, {add_register_hash, Title, Contract, TH}).

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
    {noreply, NewState};
handle_cast({add_register_hash, Title, Contract, TH}, State) ->
    NewState = do_add_register_hash(Title, Contract, TH, State),
    {noreply, NewState}.

handle_info({subscribe_tx, {create_poll, Title, TH}, {ok, Contract}}, State) ->
    io:format("Poll created: ~p~n", [Contract]),
    NewState = do_poll_created(Title, TH, Contract, State),
    {noreply, NewState};
handle_info({subscribe_tx, {create_poll, _Title, TH}, {error, Reason}}, State) ->
    io:format("poll creation failed: ~p~n", [Reason]),
    NewState = lists:keydelete({pending, TH}, #pending_poll.address, State),
    {noreply, NewState};
handle_info({subscribe_tx, {register_poll, Contract, _TH}, {ok, Index}}, State) ->
    io:format("Poll registered: ~p~n", [Index]),
    NewState = do_poll_registered(Contract, Index, State),
    {noreply, NewState};
handle_info({subscribe_tx, {register_poll, Contract, TH}, {error, Reason}}, State) ->
    io:format("poll registration failed: ~p~n", [Reason]),
    NewState = do_poll_not_registered(State, Contract, TH),
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
    StateWithout = lists:keydelete({pending, TH}, #pending_poll.address, State),
    NewPoll = #pending_poll{title = Title, address = {created, Contract}},
    [NewPoll | StateWithout].

do_add_register_hash(Title, Contract, TH, State) ->
    % TODO: check somewhere if this contract has been registered already?
    io:format("received hash: ~s~n", [TH]),
    case remove_unregistered(State, Contract) of
        pending ->
            State;
        NewState ->
            do_add_register_hash2(Title, Contract, TH, NewState)
    end.

do_add_register_hash2(Title, Contract, TH, State) ->
    IntegerType = {integer, already_normalized, integer},
    query_man:subscribe_tx_result(self(), {register_poll, Contract, TH}, IntegerType, TH),
    NewPoll = #pending_poll{title = Title,
                            address = {created, Contract},
                            register_hash = TH},
    [NewPoll | State].

remove_unregistered(State, Contract) ->
    remove_unregistered(State, Contract, []).

remove_unregistered([#pending_poll{address = {created, Contract}, register_hash = none} | Remaining], Contract, Acc) ->
    remove_unregistered(Remaining, Contract, Acc);
remove_unregistered([#pending_poll{address = {created, Contract}} | _], Contract, _) ->
    % register_hash wasn't none, so this contract is already being registered.
    % Refuse to post.
    pending;
remove_unregistered([Poll | Remaining], Contract, Acc) ->
    remove_unregistered(Remaining, Contract, [Poll | Acc]);
remove_unregistered([], _, Acc) ->
    lists:reverse(Acc).

do_poll_registered(Contract, Index, State) ->
    case ground_truth:get_poll_state(Contract) of
        {ok, PollState} ->
            % TODO: This fails because vanillae runs dry runs on key blocks
            % only, for some reason. See if dry run can run on microblocks too,
            % or else don't even try to retrieve individual state, and just let
            % the ground_truth module handle it.
            poll_keeper:add_poll(Index, PollState);
        {error, _} ->
            ok
    end,
    lists:keydelete({created, Contract}, #pending_poll.address, State).

do_poll_not_registered(State, Contract, TH) ->
    do_poll_not_registered(State, Contract, TH, []).

do_poll_not_registered([Poll | Remaining], Contract, TH, Acc) ->
    NewPoll = case Poll of
                  #pending_poll{address = {created, Contract},
                                register_hash = TH} ->
                      Poll#pending_poll{register_hash = none};
                  _ ->
                      Poll
              end,
    do_poll_not_registered(Remaining, Contract, TH, [NewPoll | Acc]);
do_poll_not_registered([], _, _, Acc) ->
    lists:reverse(Acc).


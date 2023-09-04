-module(poll_keeper).
-behaviour(gen_server).

-export([start_link/0, add_poll/2, get_polls/1, get_poll_titles/0, get_poll/1,
        get_user_status/2, get_registry_address/0, get_poll_address/1,
        track_vote/4, filter_poll/2, filter_poll_remove/1, filter_account/2,
        get_filters/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2]).

%%
% State
%%

-include("poll_state.hrl").

-type pending_votes() :: #{{pos_integer(), vanillae:account_id()} =>
                           {revoke | integer(), vanillae:tx_hash()}}.
-record(pks,
        {registry_id :: vanillae:contract_id(),
         polls = #{} :: #{pos_integer() => #poll{}},
         first_indices = #{} :: #{vanillae:contract_id() => pos_integer()},
         pending_votes = #{} :: pending_votes(),
         filters :: filters:poll_filter_set()}).

%%
% External Interface
%%

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, {}, []).

add_poll(Index, Poll) ->
    gen_server:cast(?MODULE, {add_poll, Index, Poll}).

get_polls(Category) ->
    gen_server:call(?MODULE, {get_polls, Category}).

get_poll_titles() ->
    gen_server:call(?MODULE, get_poll_titles).

get_poll(Id) ->
    gen_server:call(?MODULE, {get_poll, Id}).

get_user_status(PollIndex, ID) ->
    gen_server:call(?MODULE, {get_user_status, PollIndex, ID}).

get_registry_address() ->
    gen_server:call(?MODULE, get_registry_address).

get_poll_address(Id) ->
    gen_server:call(?MODULE, {get_poll_address, Id}).

track_vote(ID, PollIndex, Option, TH) ->
    gen_server:cast(?MODULE, {track_vote, ID, PollIndex, Option, TH}).

filter_poll(PollIndex, Category) ->
    gen_server:cast(?MODULE, {filter_poll, PollIndex, Category}).

filter_poll_remove(PollIndex) ->
    gen_server:cast(?MODULE, {filter_poll_remove, PollIndex}).

filter_account(ID, Category) ->
    gen_server:cast(?MODULE, {filter_account, ID, Category}).

get_filters() ->
    gen_server:call(?MODULE, get_filters).

%%
% Callbacks
%%

init({}) ->
    spawn(tests, run_tests, []),
    {ok, Registries} = file:consult("registry_id"),
    #{version := 3, chain_id := Reg} = lists:last(Registries),
    {ok, Filters} = filters:load("filters"),
    % Just make up some nonsense and use the full state once we have it.
    State = dummy_state(Reg, Filters),
    ground_truth:request_full_state(),
    io:format("Poll keeper created.~n", []),
    {ok, State}.

handle_call({get_polls, Category}, _, State) ->
    Polls = do_get_polls(Category, State),
    {reply, Polls, State};
handle_call(get_poll_titles, _, State) ->
    Titles = do_get_poll_titles(State#pks.polls),
    {reply, Titles, State};
handle_call({get_poll, Id}, _, State) ->
    Options = do_get_poll(Id, State),
    {reply, Options, State};
handle_call({get_user_status, PollIndex, ID}, _, State) ->
    Status = do_get_user_status(PollIndex, ID, State),
    {reply, Status, State};
handle_call(get_registry_address, _, State) ->
    Result = State#pks.registry_id,
    {reply, Result, State};
handle_call({get_poll_address, Id}, _, State) ->
    Options = do_get_poll_address(Id, State),
    {reply, Options, State};
handle_call(get_filters, _, State) ->
    Filters = do_get_filters(State),
    {reply, Filters, State}.

handle_cast({add_poll, Index, Poll}, State) ->
    NewState = do_add_poll(Index, Poll, State),
    {noreply, NewState};
handle_cast({track_vote, ID, PollIndex, Option, TH}, State) ->
    NewState = do_track_vote(ID, PollIndex, Option, TH, State),
    {noreply, NewState};
handle_cast({filter_poll, PollIndex, Category}, State) ->
    NewState = do_filter_poll(PollIndex, Category, State),
    {noreply, NewState};
handle_cast({filter_poll_remove, PollIndex}, State) ->
    NewState = do_filter_poll_remove(PollIndex, State),
    {noreply, NewState};
handle_cast({filter_account, ID, Category}, State) ->
    NewState = do_filter_account(ID, Category, State),
    {noreply, NewState}.

handle_info({subscribe_tx, {track_vote, PollIndex, ID, TH}, {ok, {}}}, State) ->
    NewState = do_track_vote_mined(PollIndex, ID, TH, State),
    {noreply, NewState};
handle_info({subscribe_tx, {track_vote, _PollIndex, _ID, _TH}, {error, Reason}}, State) ->
    io:format("vote failed: ~p~n", [Reason]),
    {noreply, State};
handle_info({ground_truth, Polls}, State) ->
    NewState = do_ground_truth(Polls, State),
    {noreply, NewState}.

%%
% Doers
%%

do_get_polls(Category, State) ->
    Pred = fun(_Id, Poll) ->
                   Poll#poll.category >= Category
           end,
    maps:filter(Pred, State#pks.polls).

do_get_poll_titles(Polls) ->
    maps:fold(fun(_, #poll{title = P}, Ps) -> [P | Ps] end, [], Polls).

do_get_poll(Id, State) ->
    case maps:find(Id, State#pks.polls) of
        {ok, Poll} ->
            {ok, Poll};
        error ->
            {error, not_found}
    end.

do_get_poll_address(Id, State) ->
    case maps:find(Id, State#pks.polls) of
        {ok, Poll} ->
            {ok, Poll#poll.chain_id};
        error ->
            {error, not_found}
    end.

do_get_user_status(PollIndex, IDRaw, State) ->
    ID = unicode:characters_to_list(IDRaw),
    case maps:find(PollIndex, State#pks.polls) of
        {ok, Poll} -> do_get_user_status2(PollIndex, Poll, ID, State);
        error -> {error, not_found}
    end.

do_get_user_status2(PollIndex, Poll, ID, State) ->
    Current = case poll_state:poll_lookup_user(ID, Poll) of
                  [] -> none;
                  [OptionIndex] -> OptionIndex;
                  [OptionIndex | _] ->
                      io:format("Warning: User ~p has voted for multiple"
                          "options in poll ~p. Ignoring.~n", [ID, PollIndex]),
                      OptionIndex
              end,
    Pending = case maps:find({PollIndex, ID}, State#pks.pending_votes) of
                  {ok, {PendingOptionIndex, _}} -> PendingOptionIndex;
                  error -> none
              end,
    {ok, Current, Pending}.

do_add_poll(Index, Poll, State) ->
    NewIndices = update_first_indices(Poll#poll.chain_id, Index,
                                      State#pks.first_indices),
    FilteredPoll = update_poll_category(State#pks.filters, Index, Poll,
                                        NewIndices),
    NewPolls = maps:put(Index, FilteredPoll, State#pks.polls),
    State#pks{polls = NewPolls, first_indices = NewIndices}.

do_track_vote(IDRaw, PollIndex, Option, TH, State) ->
    ID = unicode:characters_to_list(IDRaw),
    Unit = {{tuple, []}, already_normalized, {tuple, []}},
    query_man:subscribe_tx_result(self(), {track_vote, PollIndex, ID, TH}, Unit, TH),
    Pending = maps:put({PollIndex, ID}, {Option, TH}, State#pks.pending_votes),
    State#pks{pending_votes = Pending}.

do_track_vote_mined(PollIndex, ID, TH, State) ->
    PendingVotes = maps:remove({PollIndex, ID}, State#pks.pending_votes),
    State2 = State#pks{pending_votes = PendingVotes},
    case maps:find({PollIndex, ID}, State#pks.pending_votes) of
        {ok, {NewOption, TH}} ->
            NewPolls = poll_state:update_vote(PollIndex, ID, NewOption,
                                              State2#pks.polls),
            State2#pks{polls = NewPolls};
        _ ->
            io:format("Warning: Got subscribe_tx for a vote that was already "
                      "out of date.~n", []),
            State2
    end.

do_filter_poll(PollIndex, Category, State) ->
    case maps:find(PollIndex, State#pks.polls) of
        {ok, Poll} ->
            % add the filter
            NewFilters = filters:set_contract_category(State#pks.filters, PollIndex, Category),
            filters:store(NewFilters, "filters"),
            % work out what the category is now
            NewPollState = update_poll_category(NewFilters, PollIndex, Poll,
                                                State#pks.first_indices),
            NewPolls = maps:put(PollIndex, NewPollState, State#pks.polls),
            State#pks{polls = NewPolls, filters = NewFilters};
        error ->
            State
    end.

do_filter_poll_remove(PollIndex, State) ->
    case maps:find(PollIndex, State#pks.polls) of
        {ok, Poll} ->
            % remove the filter
            NewFilters = filters:reset_contract_category(State#pks.filters, PollIndex),
            filters:store(NewFilters, "filters"),
            % work out what the category is now
            NewPollState = update_poll_category(NewFilters, PollIndex, Poll,
                                                State#pks.first_indices),
            NewPolls = maps:put(PollIndex, NewPollState, State#pks.polls),
            State#pks{polls = NewPolls, filters = NewFilters};
        error ->
            State
    end.

do_filter_account(IDRaw, Category, State) ->
    ID = unicode:characters_to_list(IDRaw),
    % add the filter
    NewFilters = filters:set_account_category(State#pks.filters, ID, Category),
    filters:store(NewFilters, "filters"),
    % build the new state
    NewPolls = update_all_poll_categories(State#pks.polls, NewFilters, State#pks.first_indices),
    State#pks{filters = NewFilters, polls = NewPolls}.

do_get_filters(State) ->
    {poll_filter_set, AF, CF} = State#pks.filters,
    % for each poll, add an entry for it and its creator, if not already
    % present.
    maps:fold(fun add_default_category/3, {AF, CF}, State#pks.polls).

% fold function for building full filter lists for external rendering.
% These lists will still use indices rather than names, because names are
% really a part of the REST API, not really relevant to poll_keeper.
add_default_category(PollID, Poll, {AF, CF}) ->
    NewAF = maps:update_with(Poll#poll.creator_id, fun(Cat) -> Cat end, 1, AF),
    NewCF = maps:update_with(PollID, fun(Cat) -> Cat end, default, CF),
    {NewAF, NewCF}.

do_ground_truth(Polls, State) ->
    FirstIndices = calculate_first_indices(Polls),
    PollsFiltered = update_all_poll_categories(Polls, State#pks.filters, FirstIndices),
    State#pks{polls = PollsFiltered}.

%%
% Ground Truth
%%

calculate_first_indices(Polls) ->
    calculate_first_indices(maps:iterator(Polls), #{}).

calculate_first_indices(PollsIter, FirstIndices) ->
    case maps:next(PollsIter) of
        {Index, #poll{chain_id = Address}, Rest} ->
            NewIndices = update_first_indices(Address, Index, FirstIndices),
            calculate_first_indices(Rest, NewIndices);
        none ->
            FirstIndices
    end.

update_first_indices(Address, Index, FirstIndices) ->
    case is_first(Address, Index, FirstIndices) of
        true ->
            maps:put(Address, Index, FirstIndices);
        false ->
            FirstIndices
    end.

is_first(Address, Index, FirstIndices) ->
    Index =< maps:get(Address, FirstIndices, Index).

update_all_poll_categories(Polls, Filters, FirstIndices) ->
    ConvertPoll = fun(Index, Poll) ->
                          update_poll_category(Filters, Index, Poll, FirstIndices)
                  end,
    maps:map(ConvertPoll, Polls).

update_poll_category(Filters, Index, Poll, FirstIndices) ->
    IsFirst = is_first(Poll#poll.chain_id, Index, FirstIndices),
    Category = filters:category(Filters, Index, Poll#poll.creator_id, IsFirst),
    Poll#poll{category = Category}.


%%
% Initial State
%%

dummy_state(RegistryID, Filters) ->
    DummyPoll = #poll{chain_id = "n/a",
                      creator_id = "n/a",
                      category = 2,
                      title = "Fake Poll",
                      description = "Fake poll for testing the frontend. (Does not actually appear on chain)",
                      url = "example.com",
                      close_height = never_closes,
                      options = #{1 => #poll_option{name = "Option 1"},
                                  2 => #poll_option{name = "Option 2"}}},
    Polls = #{1 => DummyPoll},
    FirstIndices = calculate_first_indices(Polls),
    PollsFiltered = update_all_poll_categories(Polls, Filters, FirstIndices),
    #pks{registry_id = RegistryID,
         polls = PollsFiltered,
         first_indices = FirstIndices,
         filters = Filters}.


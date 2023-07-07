-module(poll_keeper).
-behaviour(gen_server).

-export([start_link/0, add_poll/2, get_polls/1, get_poll_titles/0, get_poll/1,
        get_user_status/2, get_registry_address/0, get_poll_address/1,
        track_vote/4, filter_poll/2, filter_poll_remove/1, filter_user/2,
        get_filters/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2]).

%%
% State
%%

-record(poll_vote,
        {id :: vanillae:account_id(),
         weight :: non_neg_integer()}).

-record(poll_option,
        {name :: string(),
         votes = [] :: [#poll_vote{}],
         vote_tally = 0 :: non_neg_integer()}).

% TODO: add fields for creator, curation status, etc.
-record(poll,
        {chain_id :: vanillae:contract_id(),
         creator_id :: vanillae:account_id(),
         category :: integer(),
         title :: string(),
         description :: string(),
         url = "" :: string(),
         % spec_ref = none :: none | vanillae:tx_hash(),
         close_height :: integer() | never_closes,
         options :: #{integer() => #poll_option{}}}).

-type pending_votes() :: #{{pos_integer(), vanillae:account_id()} =>
                           {revoke | integer(), vanillae:tx_hash()}}.
-record(pks,
        {registry_id :: vanillae:contract_id(),
         polls = #{} :: #{pos_integer() => #poll{}},
         pending_votes = #{} :: pending_votes(),
         filters :: filters:poll_filter_set()}).

%%
% External Interface
%%

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, {}, []).

add_poll(Title, Options) ->
    gen_server:cast(?MODULE, {add_poll, Title, Options}).

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

filter_user(ID, Category) ->
    gen_server:cast(?MODULE, {filter_user, ID, Category}).

get_filters() ->
    gen_server:call(?MODULE, get_filters).

%%
% Callbacks
%%

init({}) ->
    spawn(tests, run_tests, []),
    {ok, [RegistryID]} = file:consult("registry_id"),
    {ok, Filters} = filters:load("filters"),
    State = initial_state(RegistryID, Filters),
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

handle_cast({add_poll, Title, Options}, State) ->
    NewState = do_add_poll(Title, Options, State),
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
handle_cast({filter_user, ID, Category}, State) ->
    NewState = do_filter_user(ID, Category, State),
    {noreply, NewState}.

handle_info({subscribe_tx, {track_vote, PollIndex, ID, TH}, {ok, {}}}, State) ->
    NewState = do_track_vote_mined(PollIndex, ID, TH, State),
    {noreply, NewState};
handle_info({subscribe_tx, {track_vote, _PollIndex, _ID, _TH}, {error, Reason}}, State) ->
    io:format("vote failed: ~p~n", [Reason]),
    {noreply, State}.

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
    Current = case poll_lookup_user(ID, Poll) of
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

do_add_poll(_Title, _Options, State) ->
    io:print("Warning: do_add_poll is not yet implemented.~n", []),
    State.

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
            % TODO: let update_vote lookup the option itself, using
            % Poll.voted_option(ID)
            update_vote(PollIndex, ID, NewOption, State2);
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
            % build the new state
            NewPollState = Poll#poll{category = Category},
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
            Category = filters:category(NewFilters, PollIndex, Poll#poll.creator_id),
            % build the new state
            NewPollState = Poll#poll{category = Category},
            NewPolls = maps:put(PollIndex, NewPollState, State#pks.polls),
            State#pks{polls = NewPolls, filters = NewFilters};
        error ->
            State
    end.

do_filter_user(ID, Category, State) ->
    % add the filter
    NewFilters = filters:set_account_category(State#pks.filters, ID, Category),
    filters:store(NewFilters, "filters"),
    % build the new state
    UpdateCategory = fun(PollIndex, Poll) ->
                             NewCategory = filters:category(NewFilters,
                                                            PollIndex,
                                                            Poll#poll.creator_id),
                             Poll#poll{category = NewCategory}
                     end,
    NewPolls = maps:map(UpdateCategory, State#pks.polls),
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

%%
% Ground Truth
%%

initial_state(RegistryID, Filters) ->
    {ok, PollMap} = contract_man:query_polls(RegistryID),

    ConvertPoll = fun(_Index, PollInfo) ->
                          read_poll_from_registry(Filters, PollInfo)
                  end,
    Polls = maps:map(ConvertPoll, PollMap),
    #pks{registry_id = RegistryID, polls = Polls, filters = Filters}.

read_poll_from_registry(Filters, PollInfo) ->
    PollID = maps:get("poll", PollInfo),

    % FIXME: People could create any old contract that has a `vote` entrypoint,
    % so who knows if this state is valid.
    {ok, PollState} = contract_man:query_poll_state(PollID),

    Metadata = maps:get("metadata", PollState),
    Title = maps:get("title", Metadata),
    Description = maps:get("description", Metadata),
    URL = maps:get("link", Metadata),

    OptionNames = maps:get("vote_options", PollState),
    CloseHeight = case maps:get("close_height", PollState) of
                      {"None"} -> never_closes;
                      {"Some", Height} -> Height
                  end,
    VotesMap = maps:get("votes", PollState),

    OptionsNoVotes = maps:map(fun(_, Name) -> #poll_option{name = Name} end,
                              OptionNames),

    AddVote = fun(ID, OptionIndex, Options) ->
                      case maps:is_key(OptionIndex, Options) of
                          true ->
                              {ok, Weight} = contract_man:query_account_balance(ID),
                              options_add_vote(ID, OptionIndex, Weight,
                                               Options);
                          false ->
                              Options
                      end
              end,
    Options = maps:fold(AddVote, OptionsNoVotes, VotesMap),

    {ok, #{"owner_id" := CreatorID}} = vanillae:contract(PollID),
    Category = filters:category(Filters, PollID, CreatorID),

    #poll{chain_id = PollID,
          creator_id = CreatorID,
          category = Category,
          title = Title,
          description = Description,
          url = URL,
          close_height = CloseHeight,
          options = Options}.

%%
% Data Manipulation
%%

update_vote(PollIndex, ID, NewOption, State) ->
    Poll = maps:get(PollIndex, State#pks.polls),
    PollWithout = poll_remove_vote(ID, Poll),
    PollWith = case NewOption of
                   revoke ->
                       PollWithout;
                   _ ->
                       {ok, Weight} = contract_man:query_account_balance(ID),
                       Options = options_add_vote(ID, NewOption, Weight,
                                                  PollWithout#poll.options),
                       PollWithout#poll{options = Options}
               end,
    NewPolls = maps:put(PollIndex, PollWith, State#pks.polls),
    State#pks{polls = NewPolls}.

options_add_vote(ID, OptionIndex, Weight, Options) ->
    O = maps:get(OptionIndex, Options),
    {ok, Weight} = contract_man:query_account_balance(ID),
    Vote = #poll_vote{id = ID, weight = Weight},

    Votes = O#poll_option.votes,
    NewTally = O#poll_option.vote_tally + Weight,

    O2 = O#poll_option{votes = [Vote | Votes],
                       vote_tally = NewTally},
    maps:put(OptionIndex, O2, Options).

poll_remove_vote(ID, Poll) ->
    OptionRemoveVote = fun(_, O) ->
                               option_remove_vote(ID, O)
                       end,
    NewOptions = maps:map(OptionRemoveVote, Poll#poll.options),
    Poll#poll{options = NewOptions}.

option_remove_vote(ID, O) ->
    {Vs, T} = votes_remove(ID, O#poll_option.votes, [], 0),
    O#poll_option{votes = Vs, vote_tally = T}.

votes_remove(ID, [#poll_vote{id = ID} | Remaining], Vs, T) ->
    votes_remove(ID, Remaining, Vs, T);
votes_remove(ID, [Vote | Remaining], Vs, T) ->
    votes_remove(ID, Remaining, [Vote | Vs], T + Vote#poll_vote.weight);
votes_remove(_ID, [], Vs, T) ->
    {lists:reverse(Vs), T}.

poll_lookup_user(ID, Poll) ->
    OptionLookupUser = fun(OID, O, Acc) ->
                               Votes = O#poll_option.votes,
                               votes_lookup_user(ID, OID, Votes, Acc)
                       end,
    maps:fold(OptionLookupUser, [], Poll#poll.options).

votes_lookup_user(ID, OID, [#poll_vote{id = ID} | Remaining], Acc) ->
    votes_lookup_user(ID, OID, Remaining, [OID | Acc]);
votes_lookup_user(ID, OID, [_ | Remaining], Acc) ->
    votes_lookup_user(ID, OID, Remaining, Acc);
votes_lookup_user(_ID, _OID, [], Acc) ->
    % Should be empty or a singleton, so there is no need to reverse.
    % Also, Acc includes other options, so we better not anyway.
    Acc.


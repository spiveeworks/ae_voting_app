-module(poll_keeper).
-behaviour(gen_server).

-export([start_link/0, add_poll/2, get_polls/0, get_poll_titles/0, get_poll/1,
        get_registry_address/0, get_poll_address/1, track_vote/4]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2]).

%%
% State
%%

-record(poll_vote,
        {id :: term(),
         weight :: non_neg_integer()}).

-record(poll_option,
        {name :: string(),
         votes = [] :: [#poll_vote{}],
         vote_tally = 0 :: non_neg_integer()}).

% TODO: add fields for creator, curation status, etc.
-record(poll,
        {chain_id :: vanillae:contract_id(),
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
         pending_votes = #{} :: pending_votes()}).

%%
% External Interface
%%

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, {}, []).

add_poll(Title, Options) ->
    gen_server:cast(?MODULE, {add_poll, Title, Options}).

get_polls() ->
    gen_server:call(?MODULE, get_polls).

get_poll_titles() ->
    gen_server:call(?MODULE, get_poll_titles).

get_poll(Id) ->
    gen_server:call(?MODULE, {get_poll, Id}).

get_registry_address() ->
    gen_server:call(?MODULE, get_registry_address).

get_poll_address(Id) ->
    gen_server:call(?MODULE, {get_poll_address, Id}).

track_vote(ID, PollIndex, Option, TH) ->
    gen_server:cast(?MODULE, {track_vote, ID, PollIndex, Option, TH}).


%%
% Callbacks
%%

init({}) ->
    spawn(tests, run_tests, []),
    {ok, [RegistryID]} = file:consult("registry_id"),
    State = initial_state(RegistryID),
    {ok, State}.

handle_call(get_polls, _, State) ->
    {reply, State#pks.polls, State};
handle_call(get_poll_titles, _, State) ->
    Titles = do_get_poll_titles(State#pks.polls),
    {reply, Titles, State};
handle_call({get_poll, Id}, _, State) ->
    Options = do_get_poll(Id, State),
    {reply, Options, State};
handle_call(get_registry_address, _, State) ->
    Result = State#pks.registry_id,
    {reply, Result, State};
handle_call({get_poll_address, Id}, _, State) ->
    Options = do_get_poll_address(Id, State),
    {reply, Options, State}.

handle_cast({add_poll, Title, Options}, State) ->
    NewState = do_add_poll(Title, Options, State),
    {noreply, NewState};
handle_cast({track_vote, ID, PollIndex, Option, TH}, State) ->
    NewState = do_track_vote(ID, PollIndex, Option, TH, State),
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

do_add_poll(_Title, _Options, State) ->
    io:print("Warning: do_add_poll is not yet implemented.~n", []),
    State.

do_track_vote(ID, PollIndex, Option, TH, State) ->
    Unit = {{tuple, []}, already_normalized, {tuple, []}},
    query_man:subscribe_tx_result(self(), {track_vote, PollIndex, ID, TH}, Unit, TH),
    Pending = maps:put({PollIndex, ID}, {Option, TH}, State#pks.pending_votes),
    State#pks{pending_votes = Pending}.

do_track_vote_mined(PollIndex, ID, TH, State) ->
    PendingVotes = maps:remove({PollIndex, ID}, State#pks.pending_votes),
    State2 = State#pks{pending_votes = PendingVotes},
    case maps:find({PollIndex, ID}, State#pks.pending_votes) of
        {ok, {NewOption, TH}} ->
            update_vote(PollIndex, ID, NewOption, State2);
        _ ->
            io:format("Warning: Got subscribe_tx for a vote that was already "
                      "out of date.~n", []),
            State2
    end.

%%
% Ground Truth
%%

initial_state(RegistryID) ->
    {ok, PollMap} = contract_man:query_polls(RegistryID),

    Polls = maps:map(fun read_poll_from_registry/2, PollMap),
    #pks{registry_id = RegistryID, polls = Polls}.

read_poll_from_registry(_Index, PollInfo) ->
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

    #poll{chain_id = PollID,
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


-module(poll_keeper).
-behaviour(gen_server).

-export([start_link/0, add_poll/2, get_polls/0, get_poll_titles/0, get_poll_options/1]).
-export([init/1, handle_call/3, handle_cast/2]).

%%
% State
%%

-record(poll_vote,
        {address :: term(), % what should this be? Some Vanillae thing?
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
         close_height :: integer() | endless,
         options :: #{integer() => #poll_option{}}}).

-record(pks,
        {registry_id :: vanillae:contract_id(),
         polls = #{} :: #{pos_integer() => #poll{}}}).

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

get_poll_options(Id) ->
    gen_server:call(?MODULE, {get_poll_options, Id}).


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
handle_call({get_poll_options, Id}, _, State) ->
    Options = do_get_poll_options(Id, State),
    {reply, Options, State}.

handle_cast({add_poll, Title, Options}, State) ->
    NewState = do_add_poll(Title, Options, State),
    {noreply, NewState}.

%%
% Doers
%%

do_get_poll_titles(Polls) ->
    maps:fold(fun(_, #poll{title = P}, Ps) -> [P | Ps] end, [], Polls).

do_get_poll_options(Id, State) ->
    case mapse:find(Id, State#pks.polls) of
        #poll{options = Options} ->
            Names = [Name || #poll_option{name = Name} <- Options],
            {ok, Names};
        error ->
            {err, not_found}
    end.

do_add_poll(_Title, _Options, State) ->
    io:print("Warning: do_add_poll is not yet implemented.~n", []),
    State.

%%
% Ground Truth
%%

initial_state(RegistryID) ->
    {ok, PollMap} = contract_man:query_polls(RegistryID),

    Polls = maps:map(fun read_poll_from_registry/2, PollMap),
    #pks{registry_id = RegistryID, polls = Polls}.

read_poll_from_registry(_Index, PollInfo) ->
    PollID = maps:get("poll", PollInfo),

    {ok, PollState} = contract_man:query_poll_state(PollID),

    Metadata = maps:get("metadata", PollState),
    Title = maps:get("title", Metadata),
    Description = maps:get("description", Metadata),
    URL = maps:get("link", Metadata),

    OptionNames = maps:get("vote_options", PollState),
    CloseHeight = case maps:get("close_height", PollState) of
                      {"None"} -> endless;
                      {"Some", Height} -> Height
                  end,
    VotesMap = maps:get("votes", PollState),

    OptionsNoVotes = maps:map(fun(_, Name) -> #poll_option{name = Name} end,
                              OptionNames),

    AddVote = fun(ID, OptionIndex, Options) ->
                      case maps:find(OptionIndex, Options) of
                          {ok, O} ->
                              Weight = contract_man:query_account_balance(),
                              Vote = #poll_vote{address = ID, weight = Weight},

                              Votes = O#poll_option.votes,
                              NewTally = O#poll_option.vote_tally + Weight,

                              O2 = O#poll_option{votes = [Vote | Votes],
                                                 vote_tally = NewTally},
                              maps:put(OptionIndex, O2, Options);
                          error ->
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


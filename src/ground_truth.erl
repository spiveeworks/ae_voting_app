-module(ground_truth).
-behaviour(gen_server).

-export([start_link/0, get_poll_state/1, get_full_state/0, request_full_state/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2]).

%%
% State
%%

-include("poll_state.hrl").

-record(gts, {registries :: [#registry{}],
              timer = none :: none | reference()}).

%%
% External Interface
%%

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, {}, []).

get_poll_state(Contract) ->
    gen_server:call(?MODULE, {get_poll_state, Contract}).

get_full_state() ->
    gen_server:call(?MODULE, get_full_state).

request_full_state() ->
    gen_server:cast(?MODULE, request_full_state).

%%
% Callbacks
%%

init({}) ->
    {ok, Registries} = poll_state:load_registries("registry_id"),
    % Start a timer straight away. We are starting either because the
    % application as a whole just started, or because we died and the
    % supervisor is restarting us. In the first case the poll_keeper will
    % override this timer with a request_full_state(), but in the second case
    % we want to restart the timer, so we do here. In theory the poll_keeper
    % might also have gone down, in which case it has a minute to restart. ;)
    Ref = erlang:start_timer(60000, self(), update_state),
    {ok, #gts{registries = Registries, timer = Ref}}.

handle_call({get_poll_state, Contract}, _, State) ->
    Result = do_get_poll_state(Contract),
    {reply, Result, State};
handle_call(get_full_state, _, State) ->
    Result = do_get_full_state(State#gts.registries),
    {reply, Result, State}.

handle_cast(request_full_state, State) ->
    case State#gts.timer of
        none -> ok;
        OldRef -> erlang:cancel_timer(OldRef)
    end,
    NewState = do_send_full_state(State#gts.registries),
    {noreply, NewState}.

handle_info({timeout, Ref, update_state}, State) ->
    NewState = case State#gts.timer of
                   Ref ->
                       do_send_full_state(State#gts.registries);
                   _ ->
                       % do nothing
                       State
               end,
    {noreply, NewState}.

%%
% Doers
%%

do_send_full_state(Registries) ->
    case do_get_full_state(Registries) of
        {ok, Polls} ->
            poll_keeper ! {ground_truth, Polls};
        {error, _} ->
            ok
    end,
    Ref = erlang:start_timer(60000, self(), update_state),
    #gts{registries = Registries, timer = Ref}.

do_get_full_state([Reg6, Reg7]) ->
    case poll_state:load_poll_list([Reg6, Reg7]) of
        {ok, PollMap} ->
            do_get_full_state2(PollMap);
        {error, Error} ->
            {error, Error}
    end.

do_get_full_state2(PollMap) ->
    case vanillae:top_height() of
        {ok, CurrentHeight} -> do_get_full_state3(PollMap, CurrentHeight);
        Error -> Error
    end.

do_get_full_state3(PollMap, CurrentHeight) ->
    ConvertPoll = fun(_Index, PollID) ->
                          case do_get_poll_state2(PollID, CurrentHeight) of
                              {ok, Poll} -> {true, Poll};
                              {error, _} -> false
                          end
                  end,
    Polls = maps:filtermap(ConvertPoll, PollMap),
    {ok, Polls}.

do_get_poll_state(PollID) ->
    case vanillae:top_height() of
        {ok, CurrentHeight} -> do_get_poll_state2(PollID, CurrentHeight);
        Error -> Error
    end.

do_get_poll_state2(PollID, CurrentHeight) ->
    case vanillae:contract(PollID) of
        {ok, #{"owner_id" := CreatorID}} ->
            do_get_poll_state3(PollID, CurrentHeight, CreatorID);
        {ok, Result} ->
            {error, {badresult, Result}};
        {error, Reason} ->
            {error, Reason}
    end.

do_get_poll_state3(PollID, CurrentHeight, CreatorID) ->
    case contract_man:query_poll_state(PollID) of
        {ok, PollState} ->
            do_get_poll_state4(PollID, CurrentHeight, CreatorID, PollState);
        {error, Error} ->
            {error, Error}
    end.

do_get_poll_state4(PollID, CurrentHeight, CreatorID, PollState) ->
    % FIXME: People could create any old contract that has a `vote` entrypoint,
    % so who knows if this state is valid.
    {ok, PollState} = contract_man:query_poll_state(PollID),

    Metadata = maps:get("metadata", PollState),
    Title = maps:get("title", Metadata),
    Description = maps:get("description", Metadata),
    URL = maps:get("link", Metadata),

    OptionNames = maps:get("vote_options", PollState),
    {CloseHeight, Closed} = case maps:get("close_height", PollState) of
                                {"None"} ->
                                    {never_closes, false};
                                {"Some", Height} ->
                                    {Height, CurrentHeight >= Height}
                            end,
    PollHeight = case Closed of
                     false -> CurrentHeight;
                     true -> CloseHeight
                 end,
    VotesMap = maps:get("votes", PollState),

    OptionsNoVotes = maps:map(fun(_, Name) -> #poll_option{name = Name} end,
                              OptionNames),

    AddVote = fun(ID, OptionIndex, Options) ->
                      case maps:is_key(OptionIndex, Options) of
                          true ->
                              options_add_vote(ID, PollHeight, OptionIndex, Options);
                          false ->
                              Options
                      end
              end,
    Options = maps:fold(AddVote, OptionsNoVotes, VotesMap),


    Poll = #poll{chain_id = PollID,
                 creator_id = CreatorID,
                 title = Title,
                 description = Description,
                 url = URL,
                 close_height = CloseHeight,
                 closed = Closed,
                 options = Options},
    {ok, Poll}.

%%
% Data Manipulation
%%

options_add_vote(ID, PollHeight, OptionIndex, Options) ->
    O = maps:get(OptionIndex, Options),
    {ok, Weight} = contract_man:query_account_balance(ID, PollHeight),
    Vote = #poll_vote{id = ID, weight = Weight},

    Votes = O#poll_option.votes,
    NewTally = O#poll_option.vote_tally + Weight,

    O2 = O#poll_option{votes = [Vote | Votes],
                       vote_tally = NewTally},
    maps:put(OptionIndex, O2, Options).


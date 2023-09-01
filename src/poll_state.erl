-module(poll_state).

-export([load_registries/1, load_poll_list/1, update_vote/4, poll_lookup_user/2]).

% TODO: move some ground state functions to here?

%%
% State
%%

-include("poll_state.hrl").

%%
% Data Manipulation
%%

load_registries(Path) ->
    case file:consult(Path) of
        {ok, Terms} ->
            form_registry_records(Terms, []);
        Error ->
            Error
    end.

form_registry_records([#{version := Version, chain_id := ID} | Rest], Acc) when is_integer(Version) ->
    Reg = #registry{version = Version, chain_id = ID},
    form_registry_records(Rest, [Reg | Acc]);
form_registry_records([T | _], _) ->
    {error, {bad_registry, T}};
form_registry_records([], Acc) ->
    {ok, lists:reverse(Acc)}.

% Uses the list of registries from load_registries/1
load_poll_list(Registries) ->
    load_poll_list(Registries, #{}, 0).

load_poll_list([Registry | Rest], PollAcc, NextIndex) ->
    case contract_man:query_polls(Registry) of
        {ok, Polls} ->
            io:format("Added ~p polls, starting at index ~p.~n", [maps:size(Polls), NextIndex]),
            {NewPolls, NewIndex} = combine_polls(PollAcc, NextIndex, Polls),
            load_poll_list(Rest, NewPolls, NewIndex);
        {error, Error} ->
            {error, Error}
    end;
load_poll_list([], Polls, _) ->
    {ok, Polls}.

combine_polls(PollAcc, NextIndex, NextPolls) when NextPolls == #{} ->
    % short circuit to avoid calculating min and max of an empty list
    {PollAcc, NextIndex};
combine_polls(PollAcc, NextIndex, NextPolls) ->
    Indices = maps:keys(NextPolls),
    Min = lists:min(Indices),
    Max = lists:max(Indices),
    Offset = NextIndex - Min,
    NewMax = Max + Offset,
    AddPoll = fun(Index, Poll, Acc) ->
                      maps:put(Index + Offset, Poll, Acc)
              end,
    NewPolls = maps:fold(AddPoll, PollAcc, NextPolls),
    {NewPolls, NewMax + 1}.

update_vote(PollIndex, ID, NewOption, Polls) ->
    Poll = maps:get(PollIndex, Polls),
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
    maps:put(PollIndex, PollWith, Polls).

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


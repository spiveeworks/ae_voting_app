-module(poll_state).

-export([update_vote/4, poll_lookup_user/2]).

%%
% State
%%

-include("poll_state.hrl").

%%
% Data Manipulation
%%

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


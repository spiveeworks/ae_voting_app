-module(client_ops).

-export([vote_tx/3, post_vote_sig/4]).

%%% Client operations module.
%%% This module provides all the abstract operations that HTTP requests will
%%% need to call into, in order to read and modify the state of the backend
%%% correctly.
%%%
%%% The four things that the frontend needs to be able to do are
%%% 1. read a summary of all polls
%%% 2. read detailed information about a specific poll
%%% 3. vote on a poll
%%% 4. revoke a vote
%%%
%%% For the read operations, we just need to ask the pollkeeper what its state
%%% is. For the vote operations, we need to follow a typical transaction back
%%% and forth:
%%% 1. The backend shares what it thinks the vote transaction should look like
%%% 2. The frontent shares a signature, which the backend then posts to the
%%%    chain.
%%% 3. The frontent asks what the state of its vote is, really this is a part
%%%    of "read detailed information about a specific poll".

% serializes a transaction along with its signature
attach_signature(TX, Sig) ->
    SignedTXTemplate = [{signatures, [binary]}, {transaction, binary}],
    Fields = [{signatures, [Sig]}, {transaction, TX}],
    aeser_chain_objects:serialize(signed_tx, 1, SignedTXTemplate, Fields).

attach_signature_base58(EncodedTX, EncodedSig) ->
    {transaction, TX} = aeser_api_encoder:decode(EncodedTX),
    {signature, Sig} = aeser_api_encoder:decode(EncodedSig),
    SignedTX = attach_signature(TX, Sig),
    aeser_api_encoder:encode(transaction, SignedTX).

vote_tx(ID, PollIndex, Option) ->
    {ok, PollID} = poll_keeper:get_poll_address(PollIndex),
    contract_man:vote_tx(ID, PollID, Option).

post_vote_sig(ID, PollIndex, Option, Signature) ->
    {ok, PollID} = poll_keeper:get_poll_address(PollIndex),
    {ok, TX} = contract_man:vote_tx(ID, PollID, Option),
    SignedTX = attach_signature_base58(TX, Signature),
    {ok, #{"tx_hash" := TH}} = vanillae:post_tx(SignedTX),
    ok = poll_keeper:track_vote(ID, PollIndex, Option, TH),
    {ok, TH}.

% revoke_vote_just_sig(ID, PollIndex, Signature) -> ok.


-module(tests).

-export([run_tests/0, get_pubkey/0]).

-compile([export_all]).

-record(keypair, {public :: string(), private :: binary()}).

make_keypair() ->
    #{ public := Pub, secret := Priv } = enacl:sign_keypair(),
    PubBin = aeser_api_encoder:encode(account_pubkey, Pub),
    PubStr = unicode:characters_to_list(PubBin),
    #keypair{public = PubStr, private = Priv}.

store_keypair(Path, KeyPair) ->
    Data = io_lib:format("~p.~n", [KeyPair]),
    ok = file:write_file(Path, Data).

load_keypair(Path) ->
    {ok, [Data]} = file:consult(Path),
    Data.

% returns the signature by itself
make_transaction_signature(Priv, TX) ->
    Id = list_to_binary(vanillae:network_id()),
    Blob = <<Id/binary, TX/binary>>,
    enacl:sign_detached(Blob, Priv).

% serializes a transaction along with its signature
sign_transaction(Priv, TX) ->
    Sig = make_transaction_signature(Priv, TX),
    SignedTXTemplate = [{signatures, [binary]}, {transaction, binary}],
    Fields = [{signatures, [Sig]}, {transaction, TX}],
    aeser_chain_objects:serialize(signed_tx, 1, SignedTXTemplate, Fields).

% returns the signature by itself, base58 encoded
make_transaction_signature_base58(Priv, EncodedTX) ->
    {transaction, TX} = aeser_api_encoder:decode(EncodedTX),
    Sig = make_transaction_signature(Priv, TX),
    aeser_api_encoder:encode(signature, Sig).

sign_transaction_base58(Priv, EncodedTX) ->
    {transaction, TX} = aeser_api_encoder:decode(EncodedTX),
    SignedTX = sign_transaction(Priv, TX),
    aeser_api_encoder:encode(transaction, SignedTX).

get_key() ->
    load_keypair("keypair").

get_pubkey() ->
    K = load_keypair("dryrun_keypair"),
    K#keypair.public.


create_poll_registry() ->
    Key = get_key(),
    CreatorID = Key#keypair.public,

    {ok, CreateTX} = contract_man:create_registry(CreatorID),

    SignedTX = sign_transaction_base58(Key#keypair.private, CreateTX),

    query_man:post_tx_contract(self(), "create poll registry", SignedTX),
    {ok, Contract} = receive
                         {subscribe_tx, "create poll registry", A} -> A
                     end,

    io:format("Contract: ~s~n", [Contract]),

    Contract.


registry_id() ->
    "ct_4ddJuw5ekkgg6SvkX6F3k3Vs42a6CCfHgAEbidhCiYyM5k7sw".

create_poll_contract() ->
    Key = get_key(),
    ID = Key#keypair.public,

    Description = "Dummy poll for testing the poll contract",
    Options = #{1 => "option 1", 2 => "option 2"},
    {ok, CreateTX} = contract_man:create_poll(ID, "Test Poll", Description,
                                              "example.com", none, Options,
                                              never_closes),

    SignedTX = sign_transaction_base58(Key#keypair.private, CreateTX),

    {ok, Result} = vanillae:post_tx(SignedTX),
    #{"tx_hash" := Hash} = Result,
    {ok, Hash}.

add_poll_to_registry(RegistryContract, PollContract) ->
    Key = get_key(),
    ID = Key#keypair.public,

    {ok, {ResultType, TX}} = contract_man:register_poll(ID, RegistryContract,
                                                        PollContract, true),
    io:format("Result type: ~p~n", [ResultType]),

    SignedTX = sign_transaction_base58(Key#keypair.private, TX),

    {ok, Result} = vanillae:post_tx(SignedTX),
    #{"tx_hash" := Hash} = Result,

    incubator:add_register_hash("Test Poll", PollContract, Hash),

    ok.

adt_test() ->
    Key = get_key(),
    ID = Key#keypair.public,

    Args = [#{"x" => ["NoInts", {"OneInt", 100},
                      {"TwoInts", 10, 20},
                      {"NoInts"}],
              "y" => {5, 6}}],
    {ok, _CreateTX} = vanillae:contract_create(ID, "contracts/ADT_Test.aes", Args),

    ok.

create_and_add_poll(RegistryID) ->
    {ok, Hash} = create_poll_contract(),
    io:format("~nTransaction hash: ~n~s~n", [Hash]),

    incubator:add_poll_hash("Test Poll", Hash),
    io:format("Incubator state: ~p~n", [incubator:get_state()]),

    query_man:subscribe_tx_contract(self(), "create poll", Hash),
    {ok, PollID} = receive
                       {subscribe_tx, "create poll", A} -> A
                   end,
    io:format("Contract id: ~s~n", [PollID]),
    {ok, PollInfo} = vanillae:contract(PollID),
    io:format("Poll info:~n~p~n", [PollInfo]),

    {ok, RegistryInfo} = vanillae:contract(RegistryID),
    io:format("Registry info:~n~p~n", [RegistryInfo]),

    add_poll_to_registry(RegistryID, PollID),
    io:format("Incubator state: ~p~n", [incubator:get_state()]),

    ok.

create_registry_and_poll_parallel() ->
    Key = load_keypair("dryrun_keypair"),
    CreatorID = Key#keypair.public,

    {ok, CreateTX} = contract_man:create_registry(CreatorID),

    SignedTX = sign_transaction_base58(Key#keypair.private, CreateTX),

    query_man:post_tx_contract(self(), "create poll registry", SignedTX),
    {ok, Hash} = create_poll_contract(),
    io:format("~nTransaction hash: ~n~s~n", [Hash]),
    query_man:subscribe_tx_contract(self(), "create poll", Hash),

    {ok, RegistryID} = receive
                           {subscribe_tx, "create poll registry", A} -> A
                       end,

    {ok, PollID} = receive
                       {subscribe_tx, "create poll", B} -> B
                   end,

    add_poll_to_registry(RegistryID, PollID),

    RegistryID.

vote_poll(PollIndex, Option) ->
    Key = get_key(),
    ID = Key#keypair.public,

    {ok, TX} = client_ops:vote_tx(ID, PollIndex, Option),
    Sig = make_transaction_signature_base58(Key#keypair.private, TX),
    {ok, TH} = client_ops:post_vote_sig(ID, PollIndex, Option, Sig),
    TH.

vote_poll_wait(PollIndex, Option) ->
    TH = vote_poll(PollIndex, Option),

    query_man:subscribe_tx_info(self(), "add vote", TH),
    receive
        {subscribe_tx, "add vote", _} -> ok
    end,

    ok.

run_tests() ->
    %adt_test(),

    %RegistryID = create_registry_and_poll_parallel(),
    %create_and_add_poll(RegistryID),

    %RegistryID = poll_keeper:get_registry_address(),

    %create_and_add_poll(RegistryID),

    %PollID = 9,

    %vote_poll_wait(PollID, 1),
    %{ok, PollAfter1} = poll_keeper:get_poll(PollID),
    %io:format("Poll ~p: ~p~n", [PollID, element(7, PollAfter1)]),

    %vote_poll_wait(PollID, revoke),
    %{ok, PollAfter2} = poll_keeper:get_poll(PollID),
    %io:format("Poll ~p: ~p~n", [PollID, element(7, PollAfter2)]),

    ok.


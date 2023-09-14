-module(tests).

-export([run_tests/0, get_pubkey/0]).

-compile([export_all]).

-record(keypair, {public :: string(), private :: binary()}).

-include("poll_state.hrl").

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


create_poll_registry(Version) ->
    Key = get_key(),
    CreatorID = Key#keypair.public,

    {ok, [Reg | _]} = poll_state:load_registries("registry_id"),
    {ok, #{0 := Prototype}} = poll_state:load_poll_list([Reg]),

    {ok, CreateTX} = contract_man:create_registry(CreatorID, Version, Prototype),

    SignedTX = sign_transaction_base58(Key#keypair.private, CreateTX),

    query_man:post_tx_contract(self(), "create poll registry", SignedTX),
    {ok, Contract} = receive
                         {subscribe_tx, "create poll registry", A} -> A
                     end,

    io:format("Contract: ~s~n", [Contract]),

    Contract.


registry_id() ->
    "ct_4ddJuw5ekkgg6SvkX6F3k3Vs42a6CCfHgAEbidhCiYyM5k7sw".

create_poll_contract(Registry) ->
    Key = get_key(),
    ID = Key#keypair.public,

    Description = "Dummy poll for testing the poll contract",
    Options = #{1 => "option 1", 2 => "option 2"},
    {ok, CreateTX} = contract_man:create_poll(ID, Registry, "Test Poll", Description,
                                              "example.com", none, Options,
                                              never_closes),

    SignedTX = sign_transaction_base58(Key#keypair.private, CreateTX),

    {ok, Result} = vanillae:post_tx(SignedTX),
    #{"tx_hash" := Hash} = Result,
    {ok, Hash}.

adt_test() ->
    Key = get_key(),
    ID = Key#keypair.public,

    Args = [#{"x" => ["NoInts",
                      {"OneInt", 100},
                      {"TwoInts", 20, true},
                      {"NoInts"}, "OtherNumberOfInts",
                      {"ManyInts", #{0 => 0,
                                     1 => "thirty",
                                     "two" => 2,
                                     "three" => "fifty"}},
                      false],
              "y" => {5, true}}],
    {ok, _CreateTX} = vanillae:contract_create(ID, "contracts/ADT_Test.aes", Args),

    ok.

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

add_poll_registry(Version) ->
    Contract = create_poll_registry(Version),

    {ok, OldRegistries} = poll_state:load_registries("registry_id"),

    Registries = OldRegistries ++ [#registry{version = Version,
                                             chain_id = Contract}],

    poll_state:store_registries("registry_id", Registries),

    Contract.

create_poll_and_registry() ->
    Registry = add_poll_registry(3),
    {ok, TH} = create_poll_contract(Registry),

    query_man:subscribe_tx_info(self(), "create poll", TH),
    receive
        {subscribe_tx, "create poll", Info} ->
            io:format("Info: ~p~n", [Info])
    end,

    ok.


run_tests() ->
    %adt_test(),

    %Registry = create_registry_and_poll_parallel(7),
    %create_and_add_poll(Registry),

    %{ok, [_Reg6, Reg7]} = poll_state:load_registries("registry_id"),

    %RegistryID = create_poll_registry(7),

    %create_and_add_poll(Reg7),

    %PollID = 9,

    %vote_poll_wait(PollID, 1),
    %{ok, PollAfter1} = poll_keeper:get_poll(PollID),
    %io:format("Poll ~p: ~p~n", [PollID, element(7, PollAfter1)]),

    %vote_poll_wait(PollID, revoke),
    %{ok, PollAfter2} = poll_keeper:get_poll(PollID),
    %io:format("Poll ~p: ~p~n", [PollID, element(7, PollAfter2)]),

    % create_poll_and_registry(),

    ok.

compile_fatecode(Path, OutputPath) ->
    case aeso_compiler:file(Path) of
        {ok, #{fate_code := FC}} ->
            Data = io_lib:format("~p.~n", [FC]),
            file:write_file(OutputPath, Data),
            io:format("Compiled ~s to ~s~n", [Path, OutputPath]);
        {error, Reason} ->
            io:format("Error in file ~s: ~p~n", [Path, Reason])
    end.

compile_all_fatecode() ->
    compile_fatecode("contracts/ADT_Test.aes", "ADTs.fate"),
    compile_fatecode("contracts/Poll_v2.aes", "poll.fate"),
    compile_fatecode("contracts/Registry_v3.aes", "registry.fate"),
    halt().


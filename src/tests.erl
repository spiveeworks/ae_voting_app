-module(tests).

-export([run_tests/0]).

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

sign_transaction_base58(Priv, EncodedTX) ->
    {transaction, TX} = aeser_api_encoder:decode(EncodedTX),
    SignedTX = sign_transaction(Priv, TX),
    aeser_api_encoder:encode(transaction, SignedTX).

get_key() ->
    load_keypair("keypair").

tx_contract(TH) ->
    case vanillae:tx_info(TH) of
        {ok, #{"call_info" := #{"contract_id" := Contract}}} ->
            {ok, Contract};
        {error, Reason} -> {error, Reason}
    end.

sleep_until_result_mined(AACI, Fun, TH) ->
    sleep_until_thunk_mined(fun() -> vanillae:tx_result(AACI, Fun, TH) end).

sleep_until_contract_mined(TH) ->
    sleep_until_thunk_mined(fun() -> tx_contract(TH) end).

sleep_until_thunk_mined(F) ->
    case F() of
        {error, "Tx not mined"} ->
            io:format("Tx not mined. Sleeping...~n", []),
            timer:sleep(1000),
            sleep_until_thunk_mined(F);
        {error, "Transaction not found"} ->
            io:format("Tx not found. Sleeping...~n", []),
            timer:sleep(1000),
            sleep_until_thunk_mined(F);
        {error, timeout} ->
            io:format("Node timed out. Sleeping...~n", []),
            timer:sleep(1000),
            sleep_until_thunk_mined(F);
        {ok, Result} ->
            {ok, Result}
    end.


% TODO: Make this run asynchronously, and poll for when the contract is
% created?
create_poll_registry() ->
    Key = get_key(),
    CreatorID = Key#keypair.public,
    Path = "contracts/Registry_Compiler_v6.aes",

    {ok, CreateTX} = vanillae:contract_create(CreatorID, Path, []),

    SignedTX = sign_transaction_base58(Key#keypair.private, CreateTX),

    {ok, Result} = vanillae:post_tx(SignedTX),
    #{"tx_hash" := Hash} = Result,
    io:format("~nTransaction hash: ~s~n", [Hash]),

    {ok, Contract} = sleep_until_contract_mined(Hash),

    io:format("Contract: ~s~n", [Contract]),

    Contract.


registry_id() ->
    "ct_4ddJuw5ekkgg6SvkX6F3k3Vs42a6CCfHgAEbidhCiYyM5k7sw".

fetch_polls() ->
    {ok, AACI} = vanillae:prepare_contract("contracts/Registry_Compiler_v6.aes"),

    Key = get_key(),
    CallerID = Key#keypair.public,
    ContractID = registry_id(),
    {ok, TX} = vanillae:contract_call(CallerID, 100000, AACI, ContractID, "polls", []),

    {ok, Result} = vanillae:dry_run_result(AACI, "polls", TX),
    Result.

fetch_polls_gas() ->
    ContractID = registry_id(),

    {ok, AACI} = vanillae:prepare_contract("contracts/Registry_Compiler_v6.aes"),

    Key = get_key(),
    CallerID = Key#keypair.public,
    {ok, TX} = vanillae:contract_call(CallerID, 100000, AACI, ContractID, "polls", []),

    SignedTX = sign_transaction_base58(Key#keypair.private, TX),

    {ok, #{"tx_hash" := Hash}} = vanillae:post_tx(SignedTX),

    io:format("polls() transaction hash: ~s~n", [Hash]),

    {ok, Result} = sleep_until_result_mined(AACI, "polls", Hash),
    Result.

create_poll_contract() ->
    Key = get_key(),
    ID = Key#keypair.public,
    PollPath = "contracts/Poll_Iris.aes",

    PollMetadata = #{"title" => "Test Poll",
                     "description" => "Dummy poll for testing the poll contract.",
                     "link" => "example.com",
                     "spec_ref" => "None"},

    Options = #{1 => "option 1", 2 => "option 2"},
    {ok, TopHeight} = vanillae:top_height(),
    CloseHeight = TopHeight + 100,
    PollArgs = [PollMetadata, Options, {"Some", CloseHeight}],

    {ok, CreateTX} = vanillae:contract_create(ID, PollPath, PollArgs),
    io:format("Tx: ~s~n", [CreateTX]),

    SignedTX = sign_transaction_base58(Key#keypair.private, CreateTX),

    {ok, Result} = vanillae:post_tx(SignedTX),
    #{"tx_hash" := Hash} = Result,
    {ok, Hash}.

add_poll_to_registry(RegistryContract, PollContract) ->
    Key = get_key(),
    ID = Key#keypair.public,
    {ok, AACI} = vanillae:prepare_contract("contracts/Registry_Compiler_v6.aes"),
    {ok, TX} = vanillae:contract_call(ID, AACI, RegistryContract, "add_poll", [PollContract, true]),

    SignedTX = sign_transaction_base58(Key#keypair.private, TX),

    {ok, Result} = vanillae:post_tx(SignedTX),
    #{"tx_hash" := Hash} = Result,

    {ok, PollIndex} = sleep_until_result_mined(AACI, "add_poll", Hash),
    io:format("Poll index ~p~n", [PollIndex]),

    PollIndex.

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
    {ok, PollID} = sleep_until_contract_mined(Hash),
    io:format("Contract id: ~s~n", [PollID]),
    {ok, PollInfo} = vanillae:contract(PollID),
    io:format("Poll info:~n~p~n", [PollInfo]),

    {ok, RegistryInfo} = vanillae:contract(RegistryID),
    io:format("Registry info:~n~p~n", [RegistryInfo]),

    add_poll_to_registry(RegistryID, PollID),

    ok.


run_tests() ->
    %adt_test(),

    %RegistryID = create_poll_registry(),

    %create_and_add_poll(RegistryID),

    Polls = fetch_polls_gas(),
    io:format("Polls: ~p~n", [Polls]),

    ok.


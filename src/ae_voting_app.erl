-module(ae_voting_app).
-behaviour(application).

-export([start/2]).
-export([stop/1]).

-export([make_keypair/0, store_keypair/2, load_keypair/1]).

-export([create_poll_registry/0]).

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

dry_run(TX) ->
    case vanillae:dry_run(TX) of
        {ok, #{"results" := [#{"call_obj" := Obj}]}} ->
            #{"return_value" := EncodedStr} = Obj,
            Encoded = unicode:characters_to_binary(EncodedStr),
            {contract_bytearray, Binary} = aeser_api_encoder:decode(Encoded),
            Object = aeb_fate_encoding:deserialize(Binary),
            {ok, Object};
        _ -> error
    end.

start(_Type, _Args) ->
    Dispatch = cowboy_router:compile([
        {'_', [
            {"/", hello_handler, []},
            {"/api/increaseCounter", restful, increase_counter}
        ]}
    ]),
    {ok, _} = cowboy:start_clear(my_http_listener,
        [{port, 8080}],
        #{env => #{dispatch => Dispatch}}
    ),
    {ok, Sup} = ae_voting_app_sup:start_link(),
    vanillae:ae_nodes([{"localhost",3013}]),
    vanillae:network_id("ae_uat"),
    %create_poll_registry(),
    Polls = fetch_polls(),
    io:format("Polls: ~p~n", [Polls]),
    {ok, Sup}.

% TODO: Make this run asynchronously, and poll for when the contract is
% created?
create_poll_registry() ->
    Key = get_key(),
    CreatorID = Key#keypair.public,
    io:format("Account: ~s~n", [CreatorID]),
    Path = "contracts/Registry_Compiler_v6.aes",

    {ok, CreateTX} = vanillae:contract_create(CreatorID, Path, []),
    io:format("~nCreate TX:~n~p~n", [CreateTX]),

    SignedTX = sign_transaction_base58(Key#keypair.private, CreateTX),
    io:format("~nSigned transaction: ~n~p~n", [SignedTX]),

    {ok, Result} = vanillae:post_tx(SignedTX),
    #{"tx_hash" := Hash} = Result,
    io:format("~nTransaction hash: ~n~s~n", [Hash]),

    ok.

fetch_polls() ->
    {ok, AACI} = vanillae:prepare_contract("contracts/Registry_Compiler_v6.aes"),

    Key = get_key(),
    CallerID = Key#keypair.public,
    ContractID = "ct_NNTKcrryzc6VNpuKZpvztCGo4Uha4614y5iUih1A12iJfAS7S",
    {ok, TX} = vanillae:contract_call(CallerID, AACI, ContractID, "polls", []),

    {ok, Result} = dry_run(TX),
    Result.

stop(_State) ->
    ok.

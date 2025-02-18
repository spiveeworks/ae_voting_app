-module(tests).

-export([run_tests/0, get_pubkey/0]).

-compile([export_all]).

-record(keypair, {public :: string(), private :: binary()}).

-include("poll_state.hrl").

%%%%%%%%%%%%%%%%%%%%%%%%
% Keypair Manipulation

make_keypair() ->
    #{ public := Pub, secret := Priv } = ecu_eddsa:sign_keypair(),
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


%%%%%%%%%%%%%%%%%%
% contract_man functionality

% This is just contract_man turned into a library instead of a gen_server.
% cms = 'Contract Manager State', i.e. the compiled contracts, and a key that
% can be used for dry runs.

-record(ci, {path, aaci}).

-record(cms, {registry_info :: #{integer() => #ci{}},
              poll_info :: #ci{},
              dry_run_id :: hz:pubkey()}).

load_contract_info(Path) ->
    {ok, AACI} = hz:prepare_contract(Path),
    #ci{path = Path, aaci = AACI}.

compile_cms() ->
    PollInfo = load_contract_info("contracts/Poll_v2.aes"),
    Registries = #{2 => load_contract_info("contracts/Registry_v2.aes"),
                   3 => load_contract_info("contracts/Registry_v3.aes")},

    % TODO: Put this keypair stuff in a devoted key handling module? Stop
    %       storing a private key in the backend?
    ID = tests:get_pubkey(),

    CMS = #cms{registry_info = Registries,
                 poll_info = PollInfo,
                 dry_run_id = ID},

    {ok, CMS}.

create_registry_tx(CMS, ID, Version, Prototype) ->
    case maps:find(Version, CMS#cms.registry_info) of
        {ok, #ci{path = Path}} ->
            hz:contract_create(ID, Path, [Prototype]);
        error ->
            {error, unknown_version}
    end.

query_polls_tx(CMS, ID, #registry{chain_id = RegistryID, version = Version}) ->
    Info = maps:get(Version, CMS#cms.registry_info),
    AACI = Info#ci.aaci,
    case hz:contract_call(ID, 1000000, AACI, RegistryID, "polls", []) of
        {ok, TX} ->
            {ok, {_, PollsType}} = hz:aaci_lookup_spec(AACI, "polls"),
            {ok, {PollsType, TX}};
        Error = {error, _} -> Error
    end.

query_polls(CMS, Registry) ->
    ID = CMS#cms.dry_run_id,

    case query_polls_tx(CMS, ID, Registry) of
        {ok, {PollsType, TX}} ->
            dry_run(PollsType, TX);
        Error ->
            Error
    end.

create_poll_tx(CMS, ID, RegistryID, Title, Description, Link, SpecRef, Options, Age) ->
    RegistryInfo = maps:get(3, CMS#cms.registry_info),
    AACI = RegistryInfo#ci.aaci,

    PollMetadata = #{"title" => Title,
                     "description" => Description,
                     "link" => Link,
                     "spec_ref" => option(SpecRef)},

    CloseHeight = case Age of
                      never_closes ->
                          "None";
                      _ ->
                          case hz:top_height() of
                              {ok, TopHeight} -> {"Some", TopHeight + Age};
                              _ -> error
                          end
                  end,

    PollArgs = [PollMetadata, Options, CloseHeight],

    case CloseHeight of
        error ->
            {error, top_height};
        _ ->
            hz:contract_call(ID, AACI, RegistryID, "create_poll", PollArgs)
    end.

option(none) -> "None";
option(A) -> {"Some", A}.

register_poll_tx(CMS, ID, #registry{chain_id = RegistryID, version = Version}, PollID, Listed) ->
    RegistryInfo = maps:get(Version, CMS#cms.registry_info),
    RegistryAACI = RegistryInfo#ci.aaci,

    FormationResult = hz:contract_call(ID, RegistryAACI, RegistryID,
                                             "add_poll", [PollID, Listed]),
    case FormationResult of
        {ok, TX} ->
            {ok, {_, T}} = hz:aaci_lookup_spec(RegistryAACI, "add_poll"),
            {ok, {T, TX}};
        Error = {error, _} -> Error
    end.

query_poll_state_tx(CMS, ID, PollID) ->
    AACI = CMS#cms.poll_info#ci.aaci,
    case hz:contract_call(ID, 1000000, AACI, PollID, "get_state", []) of
        {ok, TX} ->
            {ok, {_, Type}} = hz:aaci_lookup_spec(AACI, "get_state"),
            {ok, {Type, TX}};
        Error = {error, _} -> Error
    end.

query_poll_state(CMS, PollID) ->
    ID = CMS#cms.dry_run_id,

    case query_poll_state_tx(CMS, ID, PollID) of
        {ok, {StateType, TX}} ->
            dry_run(StateType, TX);
        Error ->
            Error
    end.

vote_tx(CMS, ID, PollID, revoke) ->
    AACI = CMS#cms.poll_info#ci.aaci,
    % FIXME what should this gas amount be? The vote call should have a pretty
    % consistent gas cost, right?
    hz:contract_call(ID, AACI, PollID, "revoke_vote", []);
vote_tx(CMS, ID, PollID, Option) ->
    AACI = CMS#cms.poll_info#ci.aaci,
    % FIXME what should this gas amount be? The vote call should have a pretty
    % consistent gas cost, right?
    hz:contract_call(ID, AACI, PollID, "vote", [Option]).

dry_run(Type, TX) ->
    case hz:dry_run(TX) of
        {ok, #{"results" := [#{"call_obj" := #{"return_value" := EncodedStr}}]}} ->
            hz:decode_bytearray(Type, EncodedStr);
        {ok, #{"results" := [#{"reason" := Message}]}} ->
            {error, Message};
        {error, Reason} ->
            {error, Reason}
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Contract Manipulations

% These functions combine the contract_man operations above with query_man
% operations to create and manipulate contracts.

create_poll_registry(CMS, Version) ->
    Key = get_key(),
    CreatorID = Key#keypair.public,

    {ok, [Reg | _]} = poll_state:load_registries("registry_id"),
    {ok, #{0 := Prototype}} = poll_state:load_poll_list([Reg]),

    {ok, CreateTX} = create_registry_tx(CMS, CreatorID, Version, Prototype),

    SignedTX = sign_transaction_base58(Key#keypair.private, CreateTX),

    query_man:post_tx_contract(self(), "create poll registry", SignedTX),
    {ok, Contract} = receive
                         {subscribe_tx, "create poll registry", A} -> A
                     end,

    io:format("Contract: ~s~n", [Contract]),

    Contract.

create_poll_contract(CMS, Registry) ->
    Key = get_key(),
    ID = Key#keypair.public,

    Description = "Dummy poll for testing the poll contract",
    Options = #{1 => "option 1", 2 => "option 2"},
    {ok, CreateTX} = create_poll_tx(CMS, ID, Registry, "Test Poll", Description,
                                              "example.com", none, Options,
                                              never_closes),

    SignedTX = sign_transaction_base58(Key#keypair.private, CreateTX),

    {ok, Result} = vanillae:post_tx(SignedTX),
    #{"tx_hash" := Hash} = Result,
    {ok, Hash}.

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

add_poll_registry(CMS, Version) ->
    Contract = create_poll_registry(CMS, Version),

    {ok, OldRegistries} = poll_state:load_registries("registry_id"),

    Registries = OldRegistries ++ [#registry{version = Version,
                                             chain_id = Contract}],

    poll_state:store_registries("registry_id", Registries),

    Contract.

create_poll_and_registry(CMS) ->
    Registry = add_poll_registry(CMS, 3),
    {ok, TH} = create_poll_contract(CMS, Registry),

    query_man:subscribe_tx_info(self(), "create poll", TH),
    receive
        {subscribe_tx, "create poll", Info} ->
            io:format("Info: ~p~n", [Info])
    end,

    ok.


run_tests() ->
    CMS = compile_cms(),

    %Registry = create_registry_and_poll_parallel(7),
    %create_and_add_poll(Registry),

    %{ok, [_Reg6, Reg7]} = poll_state:load_registries("registry_id"),

    %RegistryID = create_poll_registry(CMS, 7),

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


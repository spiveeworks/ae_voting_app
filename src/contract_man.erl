-module(contract_man).
-behaviour(gen_server).

-export([start_link/0]).
-export([create_registry/3, query_polls_tx/2, query_polls/1, create_poll/8,
         register_poll/4, query_poll_state/1, vote_tx/3,
         query_account_balance/1, query_account_balance/2]).
-export([init/1, handle_call/3, handle_cast/2]).

-include("poll_state.hrl").

-spec start_link() ->
    ReturnStatus :: gen_server:start_ret().

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, {}, []).

%%%%%%%%%%%%%
% Interface

create_registry(ID, Version, Prototype) ->
    gen_server:call(?MODULE, {create_registry, ID, Version, Prototype}).

query_polls_tx(ID, Registry) ->
    gen_server:call(?MODULE, {query_polls_tx, ID, Registry}).

query_polls(Registry) ->
    gen_server:call(?MODULE, {query_polls, Registry}).

create_poll(ID, RegistryID, Title, Description, Link, SpecRef, Options, Age) ->
    gen_server:call(?MODULE, {create_poll, ID, RegistryID, Title, Description,
                              Link, SpecRef, Options, Age}).

register_poll(ID, Registry, PollID, Listed) ->
    gen_server:call(?MODULE, {register_poll, ID, Registry, PollID, Listed}).

query_poll_state(PollID) ->
    gen_server:call(?MODULE, {query_poll_state, PollID}).

vote_tx(ID, PollID, Option) ->
    gen_server:call(?MODULE, {vote_tx, ID, PollID, Option}).

% TODO: move to poll_state.erl? 
query_account_balance(ID) ->
    case vanillae:acc(ID) of
        {ok, #{"balance" := Balance}} -> {ok, Balance};
        {error, Reason} -> {error, Reason}
    end.

query_account_balance(ID, PollHeight) ->
    case vanillae:acc_at_height(ID, PollHeight) of
        {ok, #{"balance" := Balance}} -> {ok, Balance};
        {error, Reason} -> {error, Reason}
    end.

%%%%%%%%%%%%%%%%%%
% Implementation

-record(ci, {path, aaci}).

-record(cms, {registry_info :: #{integer() => #ci{}},
              poll_info :: #ci{},
              dry_run_id :: vanillae:pubkey()}).

load_contract_info(Path) ->
    {ok, AACI} = vanillae:prepare_contract(Path),
    #ci{path = Path, aaci = AACI}.

init({}) ->
    PollInfo = load_contract_info("contracts/Poll_v2.aes"),
    Registries = #{2 => load_contract_info("contracts/Registry_v2.aes"),
                   3 => load_contract_info("contracts/Registry_v3.aes")},

    % TODO: Put this keypair stuff in a devoted key handling module? Stop
    %       storing a private key in the backend?
    ID = tests:get_pubkey(),

    State = #cms{registry_info = Registries,
                 poll_info = PollInfo,
                 dry_run_id = ID},

    {ok, State}.

handle_call({create_registry, ID, Version, Prototype}, _, State) ->
    Result = do_create_registry(State, ID, Version, Prototype),
    {reply, Result, State};
handle_call({query_polls_tx, ID, Registry}, _, State) ->
    Result = do_query_polls_tx(State, ID, Registry),
    {reply, Result, State};
handle_call({query_polls, Registry}, _, State) ->
    Result = do_query_polls(State, Registry),
    {reply, Result, State};
handle_call({create_poll, ID, Registry, Title, Description, Link, SpecRef, Options, Age}, _, State) ->
    Result = do_create_poll(State, ID, Registry, Title, Description, Link,
                            SpecRef, Options, Age),
    {reply, Result, State};
handle_call({register_poll, ID, Registry, PollID, Listed}, _, State) ->
    Result = do_register_poll(State, ID, Registry, PollID, Listed),
    {reply, Result, State};
handle_call({query_poll_state, PollID}, _, State) ->
    Result = do_query_poll_state(State, PollID),
    {reply, Result, State};
handle_call({vote_tx, ID, PollID, Option}, _, State) ->
    Result = do_vote_tx(State, ID, PollID, Option),
    {reply, Result, State}.

handle_cast(_, State) ->
    {noreply, State}.

do_create_registry(State, ID, Version, Prototype) ->
    case maps:find(Version, State#cms.registry_info) of
        {ok, #ci{path = Path}} ->
            vanillae:contract_create(ID, Path, [Prototype]);
        error ->
            {error, unknown_version}
    end.

do_query_polls_tx(State, ID, #registry{chain_id = RegistryID, version = Version}) ->
    Info = maps:get(Version, State#cms.registry_info),
    AACI = Info#ci.aaci,
    case vanillae:contract_call(ID, 1000000, AACI, RegistryID, "polls", []) of
        {ok, TX} ->
            {ok, {_, PollsType}} = vanillae:aaci_lookup_spec(AACI, "polls"),
            {ok, {PollsType, TX}};
        Error = {error, _} -> Error
    end.

do_query_polls(State, Registry) ->
    ID = State#cms.dry_run_id,

    case do_query_polls_tx(State, ID, Registry) of
        {ok, {PollsType, TX}} ->
            dry_run(PollsType, TX);
        Error ->
            Error
    end.

do_create_poll(State, ID, RegistryID, Title, Description, Link, SpecRef, Options, Age) ->
    RegistryInfo = maps:get(3, State#cms.registry_info),
    AACI = RegistryInfo#ci.aaci,

    PollMetadata = #{"title" => Title,
                     "description" => Description,
                     "link" => Link,
                     "spec_ref" => option(SpecRef)},

    CloseHeight = case Age of
                      never_closes ->
                          "None";
                      _ ->
                          case vanillae:top_height() of
                              {ok, TopHeight} -> {"Some", TopHeight + Age};
                              _ -> error
                          end
                  end,

    PollArgs = [PollMetadata, Options, CloseHeight],

    case CloseHeight of
        error ->
            {error, top_height};
        _ ->
            vanillae:contract_call(ID, AACI, RegistryID, "create_poll", PollArgs)
    end.

do_register_poll(State, ID, #registry{chain_id = RegistryID, version = Version}, PollID, Listed) ->
    RegistryInfo = maps:get(Version, State#cms.registry_info),
    RegistryAACI = RegistryInfo#ci.aaci,

    FormationResult = vanillae:contract_call(ID, RegistryAACI, RegistryID,
                                             "add_poll", [PollID, Listed]),
    case FormationResult of
        {ok, TX} ->
            {ok, {_, T}} = vanillae:aaci_lookup_spec(RegistryAACI, "add_poll"),
            {ok, {T, TX}};
        Error = {error, _} -> Error
    end.

do_query_poll_state_tx(State, ID, PollID) ->
    AACI = State#cms.poll_info#ci.aaci,
    case vanillae:contract_call(ID, 1000000, AACI, PollID, "get_state", []) of
        {ok, TX} ->
            {ok, {_, Type}} = vanillae:aaci_lookup_spec(AACI, "get_state"),
            {ok, {Type, TX}};
        Error = {error, _} -> Error
    end.

do_query_poll_state(State, PollID) ->
    ID = State#cms.dry_run_id,

    case do_query_poll_state_tx(State, ID, PollID) of
        {ok, {StateType, TX}} ->
            dry_run(StateType, TX);
        Error ->
            Error
    end.

do_vote_tx(State, ID, PollID, revoke) ->
    AACI = State#cms.poll_info#ci.aaci,
    % FIXME what should this gas amount be? The vote call should have a pretty
    % consistent gas cost, right?
    vanillae:contract_call(ID, AACI, PollID, "revoke_vote", []);
do_vote_tx(State, ID, PollID, Option) ->
    AACI = State#cms.poll_info#ci.aaci,
    % FIXME what should this gas amount be? The vote call should have a pretty
    % consistent gas cost, right?
    vanillae:contract_call(ID, AACI, PollID, "vote", [Option]).

option(none) -> "None";
option(A) -> {"Some", A}.

dry_run(Type, TX) ->
    case vanillae:dry_run(TX) of
        {ok, #{"results" := [#{"call_obj" := #{"return_value" := EncodedStr}}]}} ->
            vanillae:decode_bytearray(Type, EncodedStr);
        {ok, #{"results" := [#{"reason" := Message}]}} ->
            {error, Message};
        {error, Reason} ->
            {error, Reason}
    end.


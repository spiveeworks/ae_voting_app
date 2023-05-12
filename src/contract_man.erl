-module(contract_man).
-behaviour(gen_server).

-export([start_link/0]).
-export([create_registry/1, query_polls_tx/2, query_polls/1, create_poll/7,
         register_poll/4]).
-export([init/1, handle_call/3, handle_cast/2]).
-export([tx_info_convert_result/2]). % TODO: move this somewhere

-spec start_link() ->
    ReturnStatus :: gen_server:start_ret().

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, {}, []).

%%%%%%%%%%%%%
% Interface

create_registry(ID) ->
    gen_server:call(?MODULE, {create_registry, ID}).

query_polls_tx(ID, RegistryID) ->
    gen_server:call(?MODULE, {query_polls_tx, ID, RegistryID}).

query_polls(RegistryID) ->
    gen_server:call(?MODULE, {query_polls, RegistryID}).

create_poll(ID, Title, Description, Link, SpecRef, Options, Age) ->
    gen_server:call(?MODULE, {create_poll, ID, Title, Description, Link,
                              SpecRef, Options, Age}).

register_poll(ID, RegistryID, PollID, Listed) ->
    gen_server:call(?MODULE, {register_poll, ID, RegistryID, PollID, Listed}).

-record(cms, {registry_path, registry_aaci, poll_path, poll_aaci, dry_run_id}).

init({}) ->
    RegistryPath = "contracts/Registry_Compiler_v6.aes",
    {ok, RegistryAACI} = vanillae:prepare_contract(RegistryPath),
    PollPath = "contracts/Poll_Iris.aes",
    {ok, PollAACI} = vanillae:prepare_contract(PollPath),

    % TODO: Put this keypair stuff in a devoted key handling module? Stop
    %       storing a private key in the backend?
    ID = tests:get_pubkey(),

    State = #cms{registry_path = RegistryPath,
                 registry_aaci = RegistryAACI,
                 poll_path = PollPath,
                 poll_aaci = PollAACI,
                 dry_run_id = ID},

    {ok, State}.

handle_call({create_registry, ID}, _, State) ->
    Result = do_create_registry(State, ID),
    {reply, Result, State};
handle_call({query_polls_tx, ID, RegistryID}, _, State) ->
    Result = do_query_polls_tx(State, ID, RegistryID),
    {reply, Result, State};
handle_call({query_polls, RegistryID}, _, State) ->
    Result = do_query_polls(State, RegistryID),
    {reply, Result, State};
handle_call({create_poll, ID, Title, Description, Link, SpecRef, Options, Age}, _, State) ->
    Result = do_create_poll(State, ID, Title, Description, Link, SpecRef,
                            Options, Age),
    {reply, Result, State};
handle_call({register_poll, ID, RegistryID, PollID, Listed}, _, State) ->
    Result = do_register_poll(State, ID, RegistryID, PollID, Listed),
    {reply, Result, State}.

handle_cast(_, State) ->
    {noreply, State}.

do_create_registry(State, ID) ->
    Path = State#cms.registry_path,

    vanillae:contract_create(ID, Path, []).

do_query_polls_tx(State, ID, RegistryID) ->
    AACI = State#cms.registry_aaci,
    case vanillae:contract_call(ID, 1000000, AACI, RegistryID, "polls", []) of
        {ok, TX} ->
            {ok, {_, PollsType}} = vanillae:aaci_lookup_spec(AACI, "polls"),
            {ok, {PollsType, TX}};
        Error = {error, _} -> Error
    end.

do_query_polls(State, RegistryID) ->
    ID = State#cms.dry_run_id,

    case do_query_polls_tx(State, ID, RegistryID) of
        {ok, {PollsType, TX}} ->
            Result = vanillae:dry_run(TX),
            dry_run_convert_result(PollsType, Result);
        Error ->
            Error
    end.

do_create_poll(State, ID, Title, Description, Link, SpecRef, Options, Age) ->
    Path = State#cms.poll_path,

    PollMetadata = #{"title" => Title,
                     "description" => Description,
                     "link" => Link,
                     "spec_ref" => option(SpecRef)},

    CloseHeight = case Age of
                      infinite ->
                          "None";
                      _ ->
                          case vanillae:top_height() of
                              {ok, TopHeight} -> {"Some", TopHeight + Age};
                              _ -> error
                          end
                  end,

    PollArgs = [PollMetadata, Options, CloseHeight],

    case CloseHeight of
        error -> {error, top_height};
        _ -> vanillae:contract_create(ID, Path, PollArgs)
    end.

do_register_poll(State, ID, RegistryID, PollID, Listed) ->
    RegistryAACI = State#cms.registry_aaci,

    vanillae:contract_call(ID, RegistryAACI, RegistryID, "add_poll", [PollID, Listed]).

option(none) -> "None";
option(A) -> {"Some", A}.

dry_run_convert_result(ResultDef, {ok, Result}) ->
    case Result of
        #{"results" := [#{"call_obj" := #{"return_value" := EncodedStr}}]} ->
            vanillae:decode_bytearray(ResultDef, EncodedStr);
        #{"results" := [#{"reason" := Message}]} ->
            {error, Message}
    end.

tx_info_convert_result(ResultDef, Result) ->
    case Result of
        {ok, #{"call_info" := #{"return_value" := Encoded}}} ->
            vanillae:decode_bytearray(ResultDef, Encoded);
        {error, Reason} -> {error, Reason}
    end.


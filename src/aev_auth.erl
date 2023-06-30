-module(aev_auth).
-behaviour(gen_server).

-export([start_link/0, start_auth/3, verify_sig/5]).
-export([init/1, handle_call/3, handle_cast/2]).

-record(auth_ident, {payload :: string(),
                     address :: vanillae:account_id(),
                     timestamp :: integer(),
                     nonce :: integer()}).
-record(auth_state, {expires :: reference()}).

-type auth_states() :: #{#auth_ident{} => #auth_state{}}.

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, {}, []).

start_auth(Payload, Author, TTL) ->
    gen_server:call(?MODULE, {start_auth, Payload, Author, TTL}).

verify_sig(Payload, ID, Timestamp, Nonce, Signature) ->
    gen_server:call(?MODULE, {verify_sig, Payload, ID, Timestamp, Nonce, Signature}).

init({}) ->
    {ok, #{}}.

handle_call({start_auth, Payload, Author, TTL}, _, State) ->
    {Result, NewState} = do_start_auth(Payload, Author, TTL, State),
    {reply, Result, NewState};
handle_call({verify_sig, Payload, ID, Timestamp, Nonce, Signature}, _, State) ->
    {Result, NewState} = do_verify_sig(Payload, ID, Timestamp, Nonce, Signature, State),
    {reply, Result, NewState}.

handle_cast(_, State) ->
    {noreply, State}.

-spec do_start_auth(string(), vanillae:account_id(), integer(), auth_states())
    -> {Result, auth_states()} when
      Result :: {ok, integer(), integer(), string()} | error.

do_start_auth(Payload, Author, TTL, States) ->
    Timestamp = erlang:system_time(millisecond),
    Nonce = rand:uniform(16#10000000000000000) - 1,
    AID = #auth_ident{payload = Payload,
                      address = Author,
                      timestamp = Timestamp,
                      nonce = Nonce},

    TRef = erlang:start_timer(TTL, self(), {auth_expired, AID}),
    AS = #auth_state{expires = TRef},

    NewStates = maps:put(AID, AS, States),

    Message = form_message(AID),

    {{ok, Timestamp, Nonce, Message}, NewStates}.

do_verify_sig(Payload, ID, Timestamp, Nonce, Signature, States) ->
    AID = #auth_ident{payload = Payload,
                      address = ID,
                      timestamp = Timestamp,
                      nonce = Nonce},
    case maps:is_key(AID, States) of
        true ->
            do_verify_sig2(AID, Signature, States);
        false ->
            {error, States}
    end.

do_verify_sig2(AID, Signature, States) ->
    Message = form_message(AID),
    case vanillae:verify_signature(Signature, Message, AID#auth_ident.address) of
        {ok, true} ->
            NewStates = maps:remove(AID, States),
            {ok, NewStates};
        {ok, X} ->
            io:format("Signature verification result: ~p~n", [X]),
            {error, States};
        _ ->
            {error, States}
    end.

form_message(#auth_ident{payload = Payload, timestamp = Timestamp, nonce = Nonce}) ->
    io_lib:format("Governance app action. Timestamp: ~p. RNG: ~p. Operation: ~s",
                            [Timestamp, Nonce, Payload]).


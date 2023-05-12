-module(query_man).
-behaviour(gen_server).

-export([start_link/0]).
-export([tx_info/1, tx_result/2, tx_contract/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2]).

-spec start_link() ->
    ReturnStatus :: gen_server:start_ret().

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, {}, []).

%%%%%%%%%%%%%
% Interface

tx_info(ID) ->
    gen_server:call(?MODULE, {tx_info, ID}).

tx_result(Type, ID) ->
    gen_server:call(?MODULE, {tx_result, Type, ID}).

tx_contract(ID) ->
    gen_server:call(?MODULE, {tx_contract, ID}).

%%%%%%%%%%%%%%%%%%
% Implementation

-record(query, {block_until :: timer:tref()}).
          %subsciptions : [#subscription{}]

%-record(subscription, {pid :: pid(),
%                       label = no_label :: term(),
%                       type = raw :: raw | contract | {result, term()},

-type state() :: #{string() => #query{}}.

init({}) ->
    {ok, #{}}.

handle_call({tx_info, ID}, _, State) ->
    {Result, NewState} = do_tx_info(State, ID),
    {reply, Result, NewState};
handle_call({tx_result, Type, ID}, _, State) ->
    {Result, NewState} = do_tx_result(State, Type, ID),
    {reply, Result, NewState};
handle_call({tx_contract, ID}, _, State) ->
    {Result, NewState} = do_tx_contract(State, ID),
    {reply, Result, NewState}.

handle_cast(_, State) ->
    {noreply, State}.

handle_info({timeout, _, {stop_blocking, ID}}, RecentQueries) ->
    NewRecentQueries = maps:remove(ID, RecentQueries),
    {noreply, NewRecentQueries}.

-spec do_tx_info(RecentQueries, IDRaw) -> {ok, Result} | {error, Reason}
    when RecentQueries :: state(),
         IDRaw         :: unicode:char_list(),
         Result        :: term(),
         Reason        :: term().

do_tx_info(RecentQueries, IDRaw) ->
    case unicode:characters_to_list(IDRaw) of
        {error, _, _} -> {{error, bad_pubkey}, RecentQueries};
        {incomplete, _, _} -> {{error, bad_pubkey}, RecentQueries};
        ID ->
            case maps:is_key(ID, RecentQueries) of
                true -> {{error, queried_recently}, RecentQueries};
                false -> do_tx_info2(RecentQueries, ID)
            end
    end.

do_tx_info2(RecentQueries, ID) ->
    Result = vanillae:tx_info(ID),
    ShouldBlock = case Result of
                      {error, "Tx not mined"} -> true;
                      % what about "Transaction not found"?
                      _ -> false
                  end,
    NewRecentQueries = case ShouldBlock of
                           true ->
                               TRef = erlang:start_timer(1000, self(),
                                                         {stop_blocking, ID}),
                               NewQuery = #query{block_until = TRef},
                               maps:put(ID, NewQuery, RecentQueries);
                           false ->
                               RecentQueries
                       end,
    {Result, NewRecentQueries}.


do_tx_result(State, ResultType, ID) ->
    {Info, NewState} = do_tx_info(State, ID),
    Result = tx_info_convert_result(ResultType, Info),
    {Result, NewState}.

do_tx_contract(State, ID) ->
    {Info, NewState} = do_tx_info(State, ID),
    Result = tx_info_get_contract(Info),
    {Result, NewState}.


tx_info_convert_result(ResultDef, Result) ->
    case Result of
        {ok, #{"call_info" := #{"return_value" := Encoded}}} ->
            vanillae:decode_bytearray(ResultDef, Encoded);
        {error, Reason} -> {error, Reason}
    end.

tx_info_get_contract(Result) ->
    case Result of
        {ok, #{"call_info" := #{"contract_id" := Contract}}} ->
            {ok, Contract};
        {error, Reason} -> {error, Reason}
    end.


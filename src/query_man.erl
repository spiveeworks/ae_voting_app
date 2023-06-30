-module(query_man).
-behaviour(gen_server).

-export([start_link/0]).
-export([tx_info/1, tx_result/2, tx_contract/1]).
-export([subscribe_tx_info/3, subscribe_tx_result/4, subscribe_tx_contract/3]).
-export([post_tx_info/3, post_tx_result/4, post_tx_contract/3]).
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

subscribe_tx_info(PID, Label, ID) ->
    gen_server:cast(?MODULE, {subscribe_tx_info, PID, Label, ID}).

subscribe_tx_result(PID, Label, Type, ID) ->
    gen_server:cast(?MODULE, {subscribe_tx_result, PID, Label, Type, ID}).

subscribe_tx_contract(PID, Label, ID) ->
    gen_server:cast(?MODULE, {subscribe_tx_contract, PID, Label, ID}).

post_tx_info(PID, Label, TX) ->
    gen_server:cast(?MODULE, {post_tx_info, PID, Label, TX}).

post_tx_result(PID, Label, Type, TX) ->
    gen_server:cast(?MODULE, {post_tx_result, PID, Label, Type, TX}).

post_tx_contract(PID, Label, TX) ->
    gen_server:cast(?MODULE, {post_tx_contract, PID, Label, TX}).


%%%%%%%%%%%%%%%%%%
% Implementation

-record(query, {block_until :: reference()}).

-record(sub, {pid :: pid(),
              label = no_label :: {label, term()},
              type = raw :: raw | {result, term()} | contract}).

-type query_table() :: #{vanillae:tx_hash() => #query{}}.
-type subscription_table() :: [{vanillae:tx_hash(), [#sub{}]}].
-type state() :: {qms, query_table(), subscription_table()}.

-spec init({}) -> state().

init({}) ->
    erlang:start_timer(1000, self(), check_subscriptions),
    {ok, {qms, #{}, []}}.

handle_call({tx_info, ID}, _, {qms, QT, ST}) ->
    {Result, NewQT} = do_tx_info(QT, ID),
    {reply, Result, {qms, NewQT, ST}};
handle_call({tx_result, Type, ID}, _, {qms, QT, ST}) ->
    {Result, NewQT} = do_tx_result(QT, Type, ID),
    {reply, Result, {qms, NewQT, ST}};
handle_call({tx_contract, ID}, _, {qms, QT, ST}) ->
    {Result, NewQT} = do_tx_contract(QT, ID),
    {reply, Result, {qms, NewQT, ST}}.

handle_cast({subscribe_tx_info, PID, Label, ID}, {qms, QT, ST}) ->
    NewST = do_subscribe_tx(ST, PID, Label, raw, ID),
    {noreply, {qms, QT, NewST}};
handle_cast({subscribe_tx_result, PID, Label, Type, ID}, {qms, QT, ST}) ->
    NewST = do_subscribe_tx(ST, PID, Label, {result, Type}, ID),
    {noreply, {qms, QT, NewST}};
handle_cast({subscribe_tx_contract, PID, Label, ID}, {qms, QT, ST}) ->
    NewST = do_subscribe_tx(ST, PID, Label, contract, ID),
    {noreply, {qms, QT, NewST}};
handle_cast({post_tx_info, PID, Label, TX}, {qms, QT, ST}) ->
    NewST = do_post_tx(ST, PID, Label, raw, TX),
    {noreply, {qms, QT, NewST}};
handle_cast({post_tx_result, PID, Label, Type, TX}, {qms, QT, ST}) ->
    NewST = do_post_tx(ST, PID, Label, {result, Type}, TX),
    {noreply, {qms, QT, NewST}};
handle_cast({post_tx_contract, PID, Label, TX}, {qms, QT, ST}) ->
    NewST = do_post_tx(ST, PID, Label, contract, TX),
    {noreply, {qms, QT, NewST}}.

handle_info({timeout, _, {stop_blocking, ID}}, {qms, QT, ST}) ->
    NewQT = maps:remove(ID, QT),
    {noreply, {qms, NewQT, ST}};
handle_info({timeout, _, check_subscriptions}, State) ->
    NewState = check_subscriptions(State),
    erlang:start_timer(1000, self(), check_subscriptions),
    {noreply, NewState}.

-spec do_tx_info(RecentQueries, IDRaw) -> {{ok, Result} | {error, Reason},
                                           NewQueries}
    when RecentQueries :: query_table(),
         IDRaw         :: unicode:char_list(),
         Result        :: term(),
         Reason        :: term(),
         NewQueries    :: query_table().

do_tx_info(RecentQueries, IDRaw) ->
    case unicode:characters_to_list(IDRaw) of
        {error, _, _} -> {{error, bad_pubkey}, RecentQueries};
        {incomplete, _, _} -> {{error, bad_pubkey}, RecentQueries};
        ID -> do_tx_info2(RecentQueries, ID)
    end.

do_tx_info2(RecentQueries, ID) ->
    case maps:is_key(ID, RecentQueries) of
        true -> {{error, queried_recently}, RecentQueries};
        false -> do_tx_info3(RecentQueries, ID)
    end.

do_tx_info3(RecentQueries, ID) ->
    % TODO: if the query succeeded, then we should also check if there were any
    % subscriptions to this ID, and reply to those.
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

-spec do_subscribe_tx(ST, PID, Label, Type, IDRaw) -> NewState
    when ST       :: subscription_table(),
         PID      :: pid(),
         Label    :: term(),
         Type     :: raw | {result, term()} | contract,
         IDRaw    :: unicode:char_list(),
         NewState :: state().

do_subscribe_tx(ST, PID, Label, Type, IDRaw) ->
    case unicode:characters_to_list(IDRaw) of
        {error, _, _} ->
            PID ! {subscribe_tx, Label, {error, bad_pubkey}},
            ST;
        {incomplete, _, _} ->
            PID ! {subscribe_tx, Label, {error, bad_pubkey}},
            ST;
        ID ->
            NewSub = #sub{pid = PID, label = Label, type = Type},
            add_subcription(ID, NewSub, ST)
    end.

-spec do_post_tx(ST, PID, Label, Type, TX) -> NewState
    when ST       :: subscription_table(),
         PID      :: pid(),
         Label    :: term(),
         Type     :: raw | {result, term()} | contract,
         TX       :: string(),
         NewState :: subscription_table().

do_post_tx(ST, PID, Label, Type, TX) ->
    case vanillae:post_tx(TX) of
        {ok, #{"tx_hash" := ID}} ->
            do_subscribe_tx(ST, PID, Label, Type, ID);
        {error, Reason} ->
            PID ! {subscribe_tx, Label, {error, Reason}},
            ST
    end.

add_subcription(ID, NewSub, ST) ->
    add_subcription(ID, NewSub, ST, []).

add_subcription(ID, NewSub, [{ID, Subs} | Rest], Checked) ->
    lists:reverse(Checked, [{ID, [NewSub | Subs]} | Rest]);
add_subcription(ID, NewSub, [Next | Rest], Checked) ->
    add_subcription(ID, NewSub, Rest, [Next | Checked]);
add_subcription(ID, NewSub, [], Checked) ->
    % Put the new subscription at the end.
    lists:reverse(Checked, [{ID, [NewSub]}]).

check_subscriptions({qms, QT, ST}) ->
    check_subscriptions({qms, QT, ST}, []).

check_subscriptions({qms, QT, [{ID, Subs} | Rest]}, Checked) ->
    % Use do_tx_info2 since ID is already a list.
    case do_tx_info2(QT, ID) of
        {{error, queried_recently}, NewQT} ->
            check_subscriptions({qms, NewQT, Rest}, [{ID, Subs} | Checked]);
        {{error, "Tx not mined"}, NewQT} ->
            io:format("Tx not mined... ~s~n", [ID]),
            NewST = Rest ++ lists:reverse(Checked, [{ID, Subs}]),
            {qms, NewQT, NewST};
         {{error, "Transaction not found"}, NewQT} ->
            % TODO: Count how many queries have occured without this
            % transaction being found, and cancel it eventually.
            io:format("Tx not found... ~s~n", [ID]),
            NewST = Rest ++ lists:reverse(Checked, [{ID, Subs}]),
            {qms, NewQT, NewST};
        {{error, timeout}, NewQT} ->
            io:format("Node timed out...~n", []),
            NewST = [{ID, Subs} | Rest] ++ lists:reverse(Checked),
            {qms, NewQT, NewST};
        {Result, NewQT} ->
            send_results(Subs, Result),
            NewST = Rest ++ lists:reverse(Checked),
            {qms, NewQT, NewST}
    end;
check_subscriptions({qms, QT, []}, Checked) ->
    NewST = lists:reverse(Checked),
    {qms, QT, NewST}.

send_results([], _) -> ok;
send_results([#sub{pid = PID, label = Label, type = T} | Rest], RawResult) ->
    Result = case T of
                 raw ->
                     RawResult;
                 {result, ResultDef} ->
                     tx_info_convert_result(ResultDef, RawResult);
                 contract ->
                     tx_info_get_contract(RawResult)
             end,
    PID ! {subscribe_tx, Label, Result},
    send_results(Rest, RawResult).


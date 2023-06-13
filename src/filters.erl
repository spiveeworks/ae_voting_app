-module(filters).

-export([load/1, category/3]).

-export_type([poll_filter_set/0]).

-record(poll_filter_set, {account_filters :: #{vanillae:account_id() => integer()},
                          contract_filters :: #{vanillae:contract_id() => integer()}}).

-type poll_filter_set() :: #poll_filter_set{}.

load(Path) ->
    {ok, [#{account_filters := AF, contract_filters := CF}]} = file:consult(Path),
    {ok, #poll_filter_set{account_filters = AF, contract_filters = CF}}.

category(#poll_filter_set{account_filters = AF, contract_filters = CF}, Contract, Creator) ->
    case maps:find(Contract, CF) of
        {ok, Category} -> Category;
        error ->
            case maps:find(Creator, AF) of
                {ok, Category} -> Category;
                error -> 1
            end
    end.

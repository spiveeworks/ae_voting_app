-module(filters).

-export([load/1, store/2, category/3, set_account_category/3,
         set_contract_category/3, reset_contract_category/2]).

-export_type([poll_filter_set/0]).

-record(poll_filter_set, {account_filters :: #{vanillae:account_id() => integer()},
                          contract_filters :: #{vanillae:contract_id() => integer()}}).

-type poll_filter_set() :: #poll_filter_set{}.

load(Path) ->
    {ok, [#{account_filters := AF, contract_filters := CF}]} = file:consult(Path),
    {ok, #poll_filter_set{account_filters = AF, contract_filters = CF}}.

store(#poll_filter_set{account_filters = AF, contract_filters = CF}, Path) ->
    FilterTerm = #{account_filters => AF, contract_filters => CF},
    FilterBin = io_lib:format("~p.", [FilterTerm]),
    % No backups right now, just write it to file. It should be fine...
    file:write_file(Path, FilterBin).

category(#poll_filter_set{account_filters = AF, contract_filters = CF}, Contract, Creator) ->
    case maps:find(Contract, CF) of
        {ok, Category} -> Category;
        error ->
            case maps:find(Creator, AF) of
                {ok, Category} -> Category;
                error -> 1
            end
    end.

set_account_category(Filters = #poll_filter_set{account_filters = AF}, Account, Category) ->
    NewAF = case Category of
                1 -> maps:remove(Account, AF);
                _ -> maps:put(Account, Category, AF)
            end,
    Filters#poll_filter_set{account_filters = NewAF}.

set_contract_category(Filters = #poll_filter_set{contract_filters = CF}, Contract, Category) ->
    NewCF = maps:put(Contract, Category, CF),
    Filters#poll_filter_set{contract_filters = NewCF}.

reset_contract_category(Filters = #poll_filter_set{contract_filters = CF}, Contract) ->
    NewCF = maps:remove(Contract, CF),
    Filters#poll_filter_set{contract_filters = NewCF}.


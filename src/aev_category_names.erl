-module(aev_category_names).

-export([from_id/1, to_id/1, permissions_from_id/1, permissions_to_id/1]).

from_id(0) -> "hidden";
from_id(1) -> "all";
from_id(2) -> "approved";
from_id(3) -> "official".

to_id(<<"hidden">>) -> {ok, 0};
to_id(<<"all">>) -> {ok, 1};
to_id(<<"approved">>) -> {ok, 2};
to_id(<<"official">>) -> {ok, 3};
to_id(_) -> error.

permissions_from_id(0) -> "none";
permissions_from_id(1) -> "can_create_polls";
permissions_from_id(2) -> "can_set_categories";
permissions_from_id(3) -> "can_change_permissions".

permissions_to_id(<<"none">>) -> {ok, 0};
permissions_to_id(<<"can_create_polls">>) -> {ok, 1};
permissions_to_id(<<"can_set_categories">>) -> {ok, 2};
permissions_to_id(<<"can_change_permissions">>) -> {ok, 3};
permissions_to_id(_) -> error.


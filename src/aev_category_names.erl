-module(aev_category_names).

-export([from_id/1, to_id/1]).

from_id(0) -> "hidden";
from_id(1) -> "all";
from_id(2) -> "approved";
from_id(3) -> "official".

to_id(<<"hidden">>) -> {ok, 0};
to_id(<<"all">>) -> {ok, 1};
to_id(<<"approved">>) -> {ok, 2};
to_id(<<"official">>) -> {ok, 3};
to_id(_) -> error.


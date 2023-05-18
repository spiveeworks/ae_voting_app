-module(aev_json_polls).
-behavior(cowboy_handler).

-export([init/2, allowed_methods/2]).
-export([content_types_provided/2, to_json/2]).

init(Req, State) ->
    {cowboy_rest, Req, State}.

allowed_methods(Req, State) ->
    {[<<"GET">>, <<"OPTIONS">>], Req, State}.

content_types_provided(Req, State) ->
    Provided = [
        %{{<<"text">>, <<"html">>, '*'}, to_html},
        {{<<"application">>, <<"json">>, '*'}, to_json}
    ],
    {Provided, Req, State}.

to_json(Req, State) ->
    Data = encode_polls(),
    {Data, Req, State}.

encode_polls() ->
    PollMap = poll_keeper:get_polls(),
    Polls = maps:fold(fun add_poll/3, [], PollMap),
    PollsSorted = lists:sort(fun order_polls/2, Polls),
    zj:encode(#{polls => PollsSorted}).

add_poll(ID, {poll, _, Title, _, _, CloseHeight, OptionMap}, Acc) ->
    Options = maps:fold(fun add_score/3, [], OptionMap),
    OptionsSorted = lists:sort(fun order_options/2, Options),
    Poll = #{id => ID,
             title => Title,
             close_height => CloseHeight,
             scores => OptionsSorted},
    [Poll | Acc].

add_score(ID, {poll_option, Name, _, Tally}, Acc) ->
    Option = #{id => ID, name => Name, score => Tally},
    [Option | Acc].

order_options(#{score := ScoreA, id := IDA}, #{score := ScoreB, id := IDB}) ->
    if
        ScoreA <  ScoreB -> false;
        ScoreA == ScoreB -> IDA =< IDB;
        ScoreA >  ScoreB -> true
    end.

order_polls(#{id := IDA}, #{id := IDB}) ->
    IDA >= IDB.

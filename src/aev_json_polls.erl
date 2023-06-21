-module(aev_json_polls).
-behavior(cowboy_handler).

-export([init/2, allowed_methods/2]).
-export([resource_exists/2, malformed_request/2, content_types_provided/2, to_json/2]).

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

% parse query string, report whether it succeeded, and store the result.
malformed_request(Req, get_polls) ->
    case cowboy_req:parse_qs(Req) of
        [{<<"category">>, Category}] ->
            case category_id(Category) of
                error -> {true, Req, get_polls};
                {ok, ID} -> {false, Req, {get_polls, ID}}
            end;
        [] ->
            {false, Req, {get_polls, 1}};
        _ ->
            {true, Req, get_polls}
    end;
malformed_request(Req, State) ->
    % Nothing else takes a query string, so proceed.
    {false, Req, State}.

resource_exists(Req, get_poll_info) ->
     PollID = cowboy_req:binding(id, Req),
     case poll_keeper:get_poll(PollID) of
         {ok, Poll} -> {true, Req, {get_poll_info, PollID, Poll}};
         {error, not_found} -> {false, Req, get_poll_info}
     end;
resource_exists(Req, get_user_status) ->
     PollID = cowboy_req:binding(poll, Req),
     UserID = cowboy_req:binding(user, Req),
     case poll_keeper:get_user_status(PollID, UserID) of
         {ok, Current, Pending} -> {true, Req, {get_user_status, PollID, UserID, Current, Pending}};
         {error, not_found} -> {false, Req, get_user_status}
     end;
resource_exists(Req, State) ->
    % Everything else is just a static endpoint; assume that since it got
    % routed at all, it probably exists.
    {true, Req, State}.

to_json(Req, State) ->
    Data = case State of
               {get_polls, Category} ->
                   encode_polls(Category);
               {get_poll_info, PollID, Poll} ->
                   encode_poll_info(PollID, Poll);
               {get_user_status, PollID, UserID, Current, Pending} ->
                   encode_user_status(PollID, UserID, Current, Pending)
           end,
    {Data, Req, State}.

% Encode a list of all polls

encode_polls(Category) ->
    PollMap = poll_keeper:get_polls(Category),
    Polls = maps:fold(fun add_poll/3, [], PollMap),
    PollsSorted = lists:sort(fun order_polls/2, Polls),
    zj:encode(#{polls => PollsSorted}).

add_poll(ID, {poll, _, _, Category, Title, _, _, CloseHeight, OptionMap}, Acc) ->
    Options = format_options(OptionMap),
    Poll = #{id => ID,
             category => category_name(Category),
             title => Title,
             close_height => CloseHeight,
             scores => Options},
    [Poll | Acc].

format_options(OptionMap) ->
    Options = maps:fold(fun add_score/3, [], OptionMap),
    lists:sort(fun order_options/2, Options).

add_score(ID, {poll_option, Name, _, Tally}, Acc) ->
    Option = #{id => ID, name => Name, score => Tally},
    [Option | Acc].

order_options(#{id := IDA}, #{id := IDB}) ->
    IDA =< IDB.

order_polls(#{id := IDA}, #{id := IDB}) ->
    IDA >= IDB.

% Encode info about one poll

encode_poll_info(PollID, Poll) ->
    {poll, _, Title, Description, URL, CloseHeight, OptionMap} = Poll,
    Options = format_options(OptionMap),
    zj:encode(#{id => PollID,
                title => Title,
                description => Description,
                url => URL,
                close_height => CloseHeight,
                options => Options}).

encode_user_status(_PollID, _UserID, Current, Pending) ->
    zj:encode(#{current_vote => Current,
                pending_vote => Pending}).

% Poll categories

category_name(0) -> "hidden";
category_name(1) -> "all";
category_name(2) -> "approved";
category_name(3) -> "official".

category_id(<<"hidden">>) -> {ok, 0};
category_id(<<"all">>) -> {ok, 1};
category_id(<<"approved">>) -> {ok, 2};
category_id(<<"official">>) -> {ok, 3};
category_id(_) -> error.


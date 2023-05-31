-module(aev_json_polls).
-behavior(cowboy_handler).

-export([init/2, allowed_methods/2]).
-export([resource_exists/2, content_types_provided/2, to_json/2]).

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

resource_exists(Req, get_polls) ->
    {true, Req, get_polls};
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
     end.

to_json(Req, State) ->
    Data = case State of
               get_polls ->
                   encode_polls();
               {get_poll_info, PollID, Poll} ->
                   encode_poll_info(PollID, Poll);
               {get_user_status, PollID, UserID, Current, Pending} ->
                   encode_user_status(PollID, UserID, Current, Pending)
           end,
    {Data, Req, State}.

% Encode a list of all polls

encode_polls() ->
    PollMap = poll_keeper:get_polls(),
    Polls = maps:fold(fun add_poll/3, [], PollMap),
    PollsSorted = lists:sort(fun order_polls/2, Polls),
    zj:encode(#{polls => PollsSorted}).

add_poll(ID, {poll, _, Title, _, _, CloseHeight, OptionMap}, Acc) ->
    Options = format_options(OptionMap),
    Poll = #{id => ID,
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


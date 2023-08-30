-module(aev_json_polls).
-behavior(cowboy_handler).

-export([init/2, allowed_methods/2]).
-export([resource_exists/2, malformed_request/2, content_types_provided/2, to_json/2]).

-include("poll_state.hrl").

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
            case aev_category_names:to_id(Category) of
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
resource_exists(Req, get_option_info) ->
     PollID = cowboy_req:binding(poll, Req),
     OptionID = cowboy_req:binding(option, Req),
     case poll_keeper:get_poll(PollID) of
         {ok, Poll} ->
             case maps:find(OptionID, Poll#poll.options) of
                 {ok, Option} ->
                     {true, Req, {get_option_info, PollID, OptionID, Option}};
                 error -> {false, Req, get_option_info}
             end;
         {error, not_found} -> {false, Req, get_option_info}
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
                   encode_user_status(PollID, UserID, Current, Pending);
               {get_option_info, PollID, OptionID, Option} ->
                   encode_option_info(PollID, OptionID, Option)
           end,
    {Data, Req, State}.

% Encode a list of all polls

encode_polls(Category) ->
    PollMap = poll_keeper:get_polls(Category),
    Polls = maps:fold(fun add_poll/3, [], PollMap),
    PollsSorted = lists:sort(fun order_polls/2, Polls),
    zj:encode(#{polls => PollsSorted}).

add_poll(ID, #poll{category = Category,
                   title = Title,
                   close_height = CloseHeight,
                   closed = Closed,
                   options = OptionMap}, Acc) ->
    Options = format_options(OptionMap),
    Poll = #{id => ID,
             category => aev_category_names:from_id(Category),
             title => Title,
             close_height => CloseHeight,
             closed => Closed,
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
    #poll{title = Title,
          description = Description,
          url = URL,
          close_height = CloseHeight,
          closed = Closed,
          options = OptionMap} = Poll,
    Options = format_options(OptionMap),
    zj:encode(#{id => PollID,
                title => Title,
                description => Description,
                url => URL,
                close_height => CloseHeight,
                closed => Closed,
                options => Options}).

encode_user_status(_PollID, _UserID, Current, Pending) ->
    zj:encode(#{current_vote => Current,
                pending_vote => Pending}).

% Encode a detailed view of a single option in a poll

encode_option_info(_PollID, _OptionID, {poll_option, Name, Votes, Tally}) ->
    NameBin = unicode:characters_to_binary(Name),
    VotesEncoded = lists:map(fun encode_vote/1, Votes),
    VotesSorted = lists:sort(fun order_votes/2, VotesEncoded),
    zj:binary_encode(#{name => NameBin,
                       votes => VotesSorted,
                       score => Tally}).

encode_vote({poll_vote, ID, Weight}) ->
    IDBin = unicode:characters_to_binary(ID),
    #{voter_address => IDBin, weight => Weight}.

order_votes(#{weight := Weight1}, #{weight := Weight2}) ->
    Weight1 >= Weight2.

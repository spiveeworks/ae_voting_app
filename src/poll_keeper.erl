-module(poll_keeper).
-behaviour(gen_server).

-export([start_link/0, add_poll/2, get_polls/0, get_poll_titles/0, get_poll_options/1]).
-export([init/1, handle_call/3, handle_cast/2]).

%%
% State
%%

-record(poll_vote,
        {address :: term(), % what should this be? Some Vanillae thing?
         weight :: pos_integer()}).

-record(poll_option,
        {name :: string(),
         votes = [] :: [#poll_vote{}],
         vote_tally = 0 :: non_neg_integer()}).

% TODO: add fields for creator, curation status, etc.
-record(poll,
        {id :: pos_integer(),
         title :: string(),
         options :: [#poll_option{}, ...]}).

-record(pks,
        {polls = [] :: [#poll{}],
         next_poll_id = 1 :: pos_integer()}).

%%
% External Interface
%%

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, empty, []).

add_poll(Title, Options) ->
    gen_server:cast(?MODULE, {add_poll, Title, Options}).

get_polls() ->
    gen_server:call(?MODULE, get_polls).

get_poll_titles() ->
    gen_server:call(?MODULE, get_poll_titles).

get_poll_options(Id) ->
    gen_server:call(?MODULE, {get_poll_options, Id}).


%%
% Callbacks
%%

init(empty) ->
    {ok, #pks{}}.

handle_call(get_polls, _, State) ->
    {reply, State#pks.polls, State};
handle_call(get_poll_titles, _, State) ->
    Titles = do_get_poll_titles(State),
    {reply, Titles, State};
handle_call({get_poll_options, Id}, _, State) ->
    Options = do_get_poll_options(Id, State),
    {reply, Options, State}.

handle_cast({add_poll, Title, Options}, State) ->
    NewState = do_add_poll(Title, Options, State),
    {noreply, NewState}.

%%
% Doers
%%

lookup_poll(Id, State) ->
    lists:keyfind(Id, #poll.id, State#pks.polls).

do_get_poll_titles(State) ->
    [Title || #poll{title = Title} <- State#pks.polls].

do_get_poll_options(Id, State) ->
    case lookup_poll(Id, State) of
        #poll{options = Options} ->
            Names = [Name || #poll_option{name = Name} <- Options],
            {ok, Names};
        false ->
            {err, not_found}
    end.

create_poll(Id, Title, OptionNames) ->
    Options = [#poll_option{name = Name} || Name <- OptionNames],
    #poll{id = Id, title = Title, options = Options}.

do_add_poll(Title, Options, State) ->
    Id = State#pks.next_poll_id,
    NewPoll = create_poll(Id, Title, Options),
    State#pks{polls = [NewPoll | State#pks.polls], next_poll_id = Id + 1}.


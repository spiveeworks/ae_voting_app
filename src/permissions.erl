-module(permissions).
-behaviour(gen_server).

-export([start_link/0, can_create_polls/1, can_set_categories/1,
         can_change_permissions/1, set/2, get_all/0]).
-export([init/1, handle_call/3, handle_cast/2]).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, {}, []).

can_create_polls(ID) ->
    gen_server:call(?MODULE, {check, 1, ID}).

can_set_categories(ID) ->
    gen_server:call(?MODULE, {check, 2, ID}).

can_change_permissions(ID) ->
    gen_server:call(?MODULE, {check, 3, ID}).

set(ID, Level) ->
    gen_server:cast(?MODULE, {set, Level, ID}).

get_all() ->
    gen_server:call(?MODULE, get_all).

% callbacks

init({}) ->
    {ok, [Permissions]} = file:consult("permissions"),
    {ok, Permissions}.

handle_call({check, Level, ID}, _, State) ->
    Result = do_check(Level, ID, State),
    {reply, Result, State};
handle_call(get_all, _, State) ->
    {reply, State, State}.

handle_cast({set, Level, ID}, State) ->
    NewState = do_set(Level, ID, State),
    {noreply, NewState}.

% implementations

do_check(Level, IDRaw, Permissions) ->
    ID = unicode:characters_to_list(IDRaw),
    Actual = maps:get(ID, Permissions, 1),
    Actual >= Level.

do_set(1, IDRaw, Permissions) ->
    ID = unicode:characters_to_list(IDRaw),
    NewPermissions = maps:remove(ID, Permissions),
    save_permissions(NewPermissions),
    NewPermissions;
do_set(Level, IDRaw, Permissions) ->
    ID = unicode:characters_to_list(IDRaw),
    NewPermissions = maps:put(ID, Level, Permissions),
    save_permissions(NewPermissions),
    NewPermissions.

save_permissions(Permissions) ->
    Bin = io_lib:format("~p.", [Permissions]),
    % No backups right now, just write it to file. It should be fine...
    file:write_file("permissions", Bin).


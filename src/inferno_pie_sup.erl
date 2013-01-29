%% There is a supervisor per application.
-module(inferno_pie_sup).
-behaviour(supervisor).

-export([start_link/1, add_pie/4, add_watcher/2]). %% API.
-export([init/1]). %% supervisor.
%% Private callbacks.
-export([start_pie/2, start_watcher/1]).

-define(SUPERVISOR, ?MODULE).
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ----------------------------------------------------------------------
%% API.
%% ----------------------------------------------------------------------

-spec start_link(Server::pid()) -> {ok, Pid::pid()}.
start_link(Server) ->
	supervisor:start_link(?MODULE, [Server]).

%% child_spec() =
%%  {Id :: child_id(),
%%   StartFunc :: mfargs(),
%%   Restart :: restart(),
%%   Shutdown :: shutdown(),
%%   Type :: worker(),
%%   Modules :: modules()}
add_pie(SupRef, Type, Re, KeyMaker) ->
    Id = {pie, Type},
    ChildSpec = {Id, {?MODULE, start_pie, [Re, KeyMaker]},
                      transient, shutdown, worker, []},
    supervisor:start_child(SupRef, ChildSpec).


add_watcher(SupRef, AppDir) ->
    Id = {watcher, AppDir},
    ChildSpec = {Id, {?MODULE, start_watcher, [AppDir]},
                      transient, shutdown, worker, []},
    supervisor:start_child(SupRef, ChildSpec).


%% ----------------------------------------------------------------------
%% Behaviour's callbacks.
%% ----------------------------------------------------------------------

init([Server]) ->
    put(inferno_server, Server),
	{ok, {{one_for_one, 10, 10}, []}}.


%% ----------------------------------------------------------------------
%% Other callbacks.
%% ----------------------------------------------------------------------

start_pie(Re, KeyMaker) ->
    {ok, Pie} = dirmon_pie:start_link(Re, KeyMaker),
    dirmon_pie:monitor_async(Pie, get(server)),
    save_pie(Pie),
    %% Connect each alive watcher with this pie.
    [dirmon_pie:add_watcher_async(Pie, Watcher) || Watcher <- get_watchers()],
    {ok, Pie}.


start_watcher(AppDir) ->
    {ok, Watcher} = dirmon_watcher:start_link(AppDir),
    save_watcher(Watcher),
    [dirmon_pie:add_watcher_async(Pie, Watcher) || Pie <- get_pies()],
    {ok, Watcher}.


%% ----------------------------------------------------------------------
%% Helpers.
%% ----------------------------------------------------------------------

%% You cannot call `supervisor:which_children/1', than emulate it, using
%% the process dictionary (sorry).
get_pies() ->
    case get(inferno_pies) of
        undefined -> [];
        Pies -> [Pie || Pie <- Pies, is_process_alive(Pie)]
    end.

save_pie(Pie) ->
    put(inferno_pies, [Pie|get_pies()]).


get_watchers() ->
    case get(inferno_watchers) of
        undefined -> [];
        Ws -> [W || W <- Ws, is_process_alive(W)]
    end.

save_watcher(Watcher) ->
    put(inferno_watchers, [Watcher|get_watchers()]).

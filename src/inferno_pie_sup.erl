%% There is a supervisor per application.
-module(inferno_pie_sup).
-behaviour(supervisor).

-export([start_link/1, add_pie/4, add_watcher/3]). %% API.
-export([init/1]). %% supervisor.
%% Private callbacks.
-export([start_pie/3, start_watcher/2]).

-define(SUPERVISOR, ?MODULE).
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ----------------------------------------------------------------------
%% API.
%% ----------------------------------------------------------------------

-spec start_link(Server::pid()) -> {ok, Pid::pid()}.
start_link(Server) when is_pid(Server) ->
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
    ChildSpec = {Id, {?MODULE, start_pie, [Type, Re, KeyMaker]},
                      transient, 1000, worker, []},
    supervisor:start_child(SupRef, ChildSpec).


add_watcher(SupRef, AppDir, Types) ->
    Id = {watcher, AppDir},
    ChildSpec = {Id, {?MODULE, start_watcher, [AppDir, Types]},
                      transient, 1000, worker, []},
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

start_pie(Type, Re, KeyMaker) ->
    %% Type of pie
    {ok, Pie} = dirmon_pie:start_link(Re, KeyMaker),
    dirmon_pie:monitor_async(Pie, get(inferno_server)),
    save_pie(Pie, Type),
    %% Connect each alive watcher of suitable type with this pie.
    [dirmon_pie:add_watcher_async(Pie, Watcher)
     || {Watcher, Ts} <- get_watchers(),
        T <- Ts, T =:= Type],
    {ok, Pie}.


start_watcher(AppDir, Types) ->
    {ok, Watcher} = dirmon_watcher:start_link(AppDir),
    save_watcher(Watcher, Types),
    [dirmon_pie:add_watcher_async(Pie, Watcher)
     || {Pie, Type} <- get_pies(),
        T <- Types, T =:= Type],
    {ok, Watcher}.


%% ----------------------------------------------------------------------
%% Helpers.
%% ----------------------------------------------------------------------

%% You cannot call `supervisor:which_children/1', than emulate it, using
%% the process dictionary (sorry).
get_pies() ->
    case get(inferno_pies) of
        undefined -> [];
        Pies -> [{Pie, Type} || {Pie, Type} <- Pies, is_process_alive(Pie)]
    end.

save_pie(Pie, Type) ->
    put(inferno_pies, [{Pie, Type}|get_pies()]).


get_watchers() ->
    case get(inferno_watchers) of
        undefined -> [];
        Ws -> [{W, Types} || {W, Types} <- Ws, is_process_alive(W)]
    end.

save_watcher(Watcher, Types) ->
    put(inferno_watchers, [{Watcher, Types}|get_watchers()]).

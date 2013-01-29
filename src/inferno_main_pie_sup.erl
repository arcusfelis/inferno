%% There is a supervisor per each inferno server.
-module(inferno_main_pie_sup).
-behaviour(supervisor).

-export([start_link/1, add_application/2]). %% API.
-export([init/1]). %% supervisor.

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
add_application(SupRef, AppName) ->
    Id = {app_sup, AppName},
    ChildSpec = {Id, {inferno_pie_sup, start_link, [get(inferno_server)]},
                      transient, shutdown, supervisor, []},
    supervisor:start_child(SupRef, ChildSpec).


%% ----------------------------------------------------------------------
%% Behaviour's callbacks.
%% ----------------------------------------------------------------------

init([Server]) ->
    put(inferno_server, Server),
	{ok, {{one_for_one, 10, 10}, []}}.

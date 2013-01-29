%% There is a supervisor per each inferno server.
-module(inferno_main_pie_sup).
-behaviour(supervisor).

-export([start_link/1, add_application/2, get_application_supervisor/2]). %% API.
-export([init/1]). %% supervisor.
%% Private callbacks.
-export([start_application_supervisor/0]).

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
add_application(SupRef, AppName) ->
    Id = {app_sup, AppName},
    ChildSpec = {Id, {?MODULE, start_application_supervisor, []},
                      transient, 1000, supervisor, []},
    supervisor:start_child(SupRef, ChildSpec).


get_application_supervisor(SupRef, AppName) ->
    %% [{Id, Child, Type, Modules}]
    Id = {app_sup, AppName},
    Children = supervisor:which_children(SupRef),
    case lists:keyfind(Id, 1, Children) of
        {Id, Child, _Type, _Modules} when is_pid(Child) -> {ok, Child};
        false -> {error, no_such_app}
    end.


%% ----------------------------------------------------------------------
%% Behaviour's callbacks.
%% ----------------------------------------------------------------------

init([Server]) ->
    put(inferno_server, Server),
	{ok, {{one_for_one, 10, 10}, []}}.


%% ----------------------------------------------------------------------
%% Other callbacks.
%% ----------------------------------------------------------------------

%% @doc Start a sub-supervisor.
start_application_supervisor() ->
    inferno_pie_sup:start_link(get(inferno_server)).

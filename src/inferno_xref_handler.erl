-module(inferno_xref_handler).
-behaviour(gen_event).
-export([init/1,
         handle_event/2]).

-record(xref_state, {xref}).

init(Xref) ->
    State = #xref_state{xref = Xref},
    {ok, State}.


handle_event({add_application, AppName, AppDir},
             State=#xref_state{xref=Xref}) ->
    spawn_link(fun() ->
        xref:add_application(Xref, AppName, AppDir)
        end),
    {ok, State};
handle_event({remove_application, AppName},
             State=#xref_state{xref=Xref}) ->
    spawn_link(fun() ->
        xref:remove_application(Xref, AppName)
        end),
    {ok, State};
handle_event({update_application, AppName, _OldDir, NewDir},
             State=#xref_state{xref=Xref}) ->
    spawn_link(fun() ->
        xref:replace_application(Xref, AppName, NewDir)
        end),
    {ok, State};

handle_event(_, State) ->
    {ok, State}.

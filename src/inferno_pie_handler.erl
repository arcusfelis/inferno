-module(inferno_pie_handler).
-behaviour(gen_event).
-export([init/1,
         handle_event/2]).

-record(state, {server, main_sup}).

init(Server) ->
    {ok, MainSup} = inferno_main_pie_sup:start_link(Server),
    State = #state{server=Server, main_sup=MainSup},
    {ok, State}.


handle_event({add_application, AppName, _AppDir},
             State=#state{main_sup=MainSup}) ->
    {ok, Sup} = inferno_main_pie_sup:add_application(MainSup, AppName),
    %% Create new application:
    %% - Create pie-servers.
    {ok, _SrcPie} = 
        inferno_pie_sup:add_pie(Sup,
            src, "\\.erl$",  erl_key_maker(AppName)),
    {ok, _CmpPie} = 
        inferno_pie_sup:add_pie(Sup,
            cmp, "\\.beam$", beam_key_maker(AppName)),
    {ok, _ManPie} = 
        inferno_pie_sup:add_pie(Sup,
            man, "\\.xml$",  refman_key_maker(AppName)),
    {ok, _AppPie} = 
        inferno_pie_sup:add_pie(Sup,
            app, "\\.app$",  app_key_maker(AppName)),
    {ok, State};

handle_event({add_application_directory, AppName, AppDir},
             State=#state{main_sup=MainSup}) ->
    {ok, Sup} = inferno_main_pie_sup:
                get_application_supervisor(MainSup, AppName),
    %% Register the directory.
    SrcDir  = filename:join(AppDir, "src"),
    EbinDir = filename:join(AppDir, "ebin"),
    ManDir  = filename:join([AppDir, "doc", "src"]),
    {ok, _SrcWatcher} =
        inferno_pie_sup:add_watcher(Sup,
            SrcDir, [src]),
    {ok, _EbinWatcher} =
        inferno_pie_sup:add_watcher(Sup,
            EbinDir, [cmp, app]),
    {ok, _ManWatcher} =
        inferno_pie_sup:add_watcher(Sup,
            ManDir, [man]),
    {ok, State};

handle_event(_, State) ->
    {ok, State}.



erl_key_maker(AppName) ->
    fun(FileName) -> {erl, AppName, filename_to_module_name(FileName)} end.

beam_key_maker(AppName) ->
    fun(FileName) -> {beam, AppName, filename_to_module_name(FileName)} end.

refman_key_maker(AppName) ->
    fun(FileName) -> {refman, AppName, filename_to_module_name(FileName)} end.

app_key_maker(AppName) ->
    fun(FileName) -> {app, AppName, filename_to_module_name(FileName)} end.

filename_to_module_name(FileName) ->
    list_to_atom(filename:rootname(filename:basename(FileName))).

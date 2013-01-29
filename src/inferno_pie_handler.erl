-module(inferno_pie_handler).
-behaviour(gen_event).
-export([init/1,
         handle_event/2]).

-record(state, {server, main_sup}).

init(Server) ->
    {ok, Sup} = inferno_main_pie_sup:start_link(Server),
    State = #state{server=Server, main_sup=Sup},
    {ok, State}.


handle_event({add_application, AppName},
             State=#state{main_sup=MainSup}) ->
    {ok, Sup} = supervisor:add_application(MainSup),
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
             State=#state{server=Server, sup=Sup}) ->
    %% Register the directory.
    SrcDir  = filename:join(AppDir, "src"),
    EbinDir = filename:join(AppDir, "ebin"),
    ManDir  = filename:join([AppDir, "doc", "src"]),
    {ok, SrcSrv}  = dirmon_watcher:start_link(SrcDir),
    {ok, EbinSrv} = dirmon_watcher:start_link(EbinDir),
    {ok, ManSrv}  = dirmon_watcher:start_link(ManDir),
    dirmon_pie:add_watcher(SrcPie, SrcSrv),
    dirmon_pie:add_watcher(CmpPie, EbinSrv),
    dirmon_pie:add_watcher(AppPie, EbinSrv),
    dirmon_pie:add_watcher(ManPie, ManSrv),
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

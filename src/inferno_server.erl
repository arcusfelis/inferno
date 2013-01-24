%%% @doc This module is a `gen_server' that stores information about a 
%%%      single application.
%%% @end
-module(inferno_server).
-behaviour(gen_server).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/1,
         start/1,
         module_info/3,
         function_info/3,
         add_application/3,
         module_names_to_compiled_filenames/2,
         application_names_to_directories/2]).


%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, 
         handle_call/3, 
         handle_cast/2, 
         handle_info/2,
         terminate/2, 
         code_change/3]).


%% ------------------------------------------------------------------
%% Import 
%% ------------------------------------------------------------------



%% ------------------------------------------------------------------
%% Macro Definitions
%% ------------------------------------------------------------------

%% Used in handlers
-define(SERVER, ?MODULE).

%% Used for testing, then can be moved to an another file
-define(SRV, ?MODULE).
-define(APP, inferno).


%% ------------------------------------------------------------------
%% Records' Definitions
%% ------------------------------------------------------------------

-record(state, {
        module2source_filename :: dict(),
        module2compiled_filename :: dict(),
        module2doc_filename :: dict(),
        app2dir_filename :: dict(),
        module2app_name :: dict(),
        fun_tbl :: ets:tid(),
        mod_tbl :: ets:tid(),
        app_tbl :: ets:tid(),
        mod_rec_f2p :: dict(),
        fun_rec_f2p :: dict(),
        app_rec_f2p :: dict(),
        cache :: inferno_cache:server()
}).


-record(function_info, {
        mfa,
        fields
}).

-record(module_info, {
        module_name,
        fields
}).

-record(module_names_to_compiled_filenames, {
        module_names
}).

-record(application_names_to_directories, {
        app_names
}).

-record(add_application, {
        name,
        directory
}).

%% ------------------------------------------------------------------
%% Import code
%% ------------------------------------------------------------------

-include_lib("inferno/include/inferno.hrl").
-include_lib("eunit/include/eunit.hrl").


%% ------------------------------------------------------------------
%% Declare parse transforms
%% ------------------------------------------------------------------

-compile({parse_transform, seqbind}).
-compile({parse_transform, do}).
-compile({parse_transform, rum}).
-compile({parse_transform, gin}).


%% ------------------------------------------------------------------
%% Import external types
%% ------------------------------------------------------------------

-type x_server() :: inferno_type:x_server().


%% ------------------------------------------------------------------
%% Internal types
%% ------------------------------------------------------------------


%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------


%% @doc Start a linked server without supervision.
-spec start_link([term()]) -> {ok, x_server()}.

start_link(Params) ->
    Args = [Params],
    case proplists:get_value(name, Params) of
        undefined ->
            gen_server:start_link(?MODULE, Args, []);
        A when is_atom(A) ->
            gen_server:start_link({local, A}, ?MODULE, Args, []);
        Name ->
            gen_server:start_link(Name, ?MODULE, Args, [])
    end.


%% @doc This function is for testing only.
-spec start([term()]) -> {ok, x_server()}.

start(Params) ->
    Args = [Params],
    gen_server:start_link(?MODULE, Args, []).


function_info(Server, MFA, Fields) ->
    call(Server, #function_info{mfa = MFA, fields = Fields}).

module_info(Server, ModuleName, Fields) ->
    call(Server, #module_info{module_name = ModuleName, fields = Fields}).


%% @doc Returns filenames of modules. 
%% `MaybeFilename' is undefined, when the module is not found.
-spec module_names_to_compiled_filenames(Server, ModuleNames) -> M2F when
    Server :: x_server(),
    ModuleNames :: [ModuleName],
    M2F :: [{ModuleName, MaybeFilename}],
    ModuleName :: atom(),
    MaybeFilename :: filename:filename() | undefined. 

module_names_to_compiled_filenames(_Server, []) ->
    [];
module_names_to_compiled_filenames(Server, ModuleNames) ->
    call(Server, #module_names_to_compiled_filenames{module_names = ModuleNames}).


-spec application_names_to_directories(Server, AppNames) -> A2D when
    Server :: x_server(),
    AppNames :: [AppName],
    A2D :: [{AppName, MaybeDirName}],
    AppName :: atom(),
    MaybeDirName :: filename:dirname() | undefined. 

application_names_to_directories(_Server, []) ->
    [];
application_names_to_directories(Server, AppNames) ->
    call(Server, #application_names_to_directories{app_names = AppNames}).


-spec add_application(x_server(), atom(), filename:directory()) -> ok.

add_application(Server, AppName, AppDir) ->
    call(Server, #add_application{name = AppName, directory = AppDir}).

 
%% ------------------------------------------------------------------
%% gen_server Client Helpers
%% ------------------------------------------------------------------

-spec call(x_server(), term()) -> term().

call(Server, Params) ->
    client_error_handler(gen_server:call(Server, Params)).


client_error_handler({ok, Result}) -> 
    Result;

client_error_handler({error, Reason}) -> 
    erlang:error(Reason).


%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

%% @private
init([Params]) ->
    AppTable = ets:new(inferno_application_table, [{keypos, #info_application.name}]),
    ModTbl = ets:new(inferno_module_table, [{keypos, #info_module.name}]),
    FunTbl = ets:new(inferno_function_table, [{keypos, #info_function.mfa}]),
    %% Collect info about positions of the record.
    AppRecF2P = fields_to_position_dict(record_info(fields, info_application)),
    ModRecF2P = fields_to_position_dict(record_info(fields, info_module)),
    FunRecF2P = fields_to_position_dict(record_info(fields, info_function)),
    CacheFileName = proplists:get_value(cache_file, Params, "cache.dets"),
    {ok, Cache} = inferno_cache:start_link(CacheFileName),
    State = #state{
        fun_tbl = FunTbl,
        mod_tbl = ModTbl,
        app_tbl = AppTable,
        mod_rec_f2p = ModRecF2P,
        fun_rec_f2p = FunRecF2P,
        app_rec_f2p = AppRecF2P,
        cache = Cache
    },
    {ok, State}.

 
%% @private
handle_call(#function_info{mfa = MFA, fields = FieldNames}, _From, State) ->
    Reply = 
    do([error_m || 
        _ModuleStatus <- check_module(mfa_to_module_name(MFA), State),
        Info <- lookup_fields(State#state.fun_tbl,      %% table
                              MFA,                      %% key
                              FieldNames,               %% atoms
                              State#state.fun_rec_f2p), %% meta info
        return(Info)
       ]),
    {reply, Reply, State};

handle_call(#module_info{module_name = ModuleName, fields = FieldNames},
            _From, State) ->
    Reply = 
    do([error_m || 
        _ModuleStatus <- check_module(ModuleName, State),
        Info <- lookup_fields(State#state.mod_tbl,      %% table
                              ModuleName,               %% key
                              FieldNames,               %% atoms
                              State#state.mod_rec_f2p), %% meta info
        return(Info)
       ]),
    {reply, Reply, State};

handle_call(#module_names_to_compiled_filenames{module_names = ModuleNames},
            _From, State=#state{mod_tbl = ModTbl}) ->
    Reply = [{ModuleName, maybe_lookup_element(ModTbl, ModuleName, 
                                               #info_module.compiled_filename)}
            || ModuleName <- ModuleNames],
    {reply, {ok, Reply}, State};

handle_call(#application_names_to_directories{app_names = AppNames},
            _From, State=#state{app_tbl = AppTbl}) ->
    Reply = [{AppName, maybe_lookup_element(AppTbl, AppName, 
                                            #info_application.directories)}
            || AppName <- AppNames],
    {reply, {ok, Reply}, State};

handle_call(#add_application{name = AppName, directory = AppDir},
            _From, State=#state{app_tbl = AppTbl}) ->
    %% Select the application with AppName from AppTbl (or create new app).
    A = 
    #info_application{source_pie = SrcPie, 
                      compiled_pie = CmpPie,
                      refman_pie = ManPie} =
    case ets:lookup(AppTbl, AppName) of
        [] ->
            new_application(AppName);
        [Ai] ->
            Ai
    end,
    %% Register the directory.
    SrcDir  = filename:join(AppDir, "src"),
    EbinDir = filename:join(AppDir, "ebin"),
    ManDir  = filename:join([AppDir, "doc", "src"]),
    {ok, SrcSrv}  = dirmon_watcher:start_link(SrcDir),
    {ok, EbinSrv} = dirmon_watcher:start_link(EbinDir),
    {ok, ManSrv}  = dirmon_watcher:start_link(ManDir),
    dirmon_pie:add_watcher(SrcPie, SrcSrv),
    dirmon_pie:add_watcher(CmpPie, EbinSrv),
    dirmon_pie:add_watcher(ManPie, ManSrv),
    ets:insert(AppTbl, A),
    {reply, {ok, ok}, State}.

%% @private
handle_cast(_Mess, State) ->
    {noreply, State}.


%% @private
handle_info({pie,Ref,Events}, State) ->
    {noreply, lists:foldl(fun handle_pie_event/2, State, Events)};

handle_info(Mess, State) ->
    io:format(user, "UNHANDLED MESSAGE: ~p~n", [Mess]),
    {noreply, State}.


%% @private
terminate(_Reason, #state{}) ->
    ok.


%% @private
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.




%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------


%% ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%% Data loading
%% ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

check_module(ModuleName, State) ->
    #state{mod_tbl = ModTbl, fun_tbl = FunTbl, cache = Cache} = State,
    case ets:lookup(ModTbl, ModuleName) of
        [M=#info_module{is_analysed = true}] ->
            {ok, M};
        [M@=#info_module{is_analysed = false}] ->
            M@ = M@#info_module{functions = []},
            M@ = inferno_edoc_slow_reader:fill(M@, Cache),
            M@ = inferno_refman_slow_reader:fill(M@, Cache),
            #info_module{functions = Funs} =
            M@ = inferno_pos_reader:fill(M@, Cache),
            M@ = M@#info_module{functions = [], is_analysed=true},
            ets:insert(ModTbl, M@),
            ets:insert(FunTbl, Funs),
            {ok, M@};
        [] ->
            %% Put the empty module, if an error.
            %% It helps to escape of file reading again.
            M = #info_module{name = ModuleName},
            ets:insert(ModTbl, M),
            {ok, M}
    end.

%% ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%% Helpers
%% ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

mfa_to_module_name({M, _F, _A}) -> M.


lookup_fields(Table, Key, FieldNames, N2P) ->
    case ets:lookup(Table, Key) of
        [] -> {error, {no_such_key, Key}};
        [Elem] -> 
            %% For each field, extract its value from `Elem'.
            try
                [element(dict:fetch(FieldName, N2P), Elem)
                 || FieldName <- FieldNames]

            of Values -> {ok, Values}
            catch error:Reason ->
                {error, {no_such_field, Reason}}
            end
    end.

maybe_lookup_element(Tbl, Key, Pos) ->
    try ets:lookup_element(Tbl, Key, Pos)
    catch error:badarg ->
            undefined
    end.


fields_to_position_dict(Fields) ->
    dict:from_list(fields_to_position_pl(Fields)).


fields_to_position_pl(Fields) ->
    fields_to_position_pl(Fields, 2).

fields_to_position_pl([H|T], N) ->
    [{H, N} | fields_to_position_pl(T, N+1)];
fields_to_position_pl([], _N) ->
    [].



%% ------------------------------------------------------------------
%% Tests
%% ------------------------------------------------------------------

function_info_test() ->
    AppDir = code:lib_dir(inferno),
    {ok, S} = ?SRV:start_link([]),
    ?SRV:add_application(S, inferno, AppDir),
    Info = ?SRV:function_info(S, {?SRV, start_link, 1},
                              [description, title, position]),
    ?assertMatch([_, _, _], Info),
    io:format(user, "function_info: ~p", [Info]),
    ok.


module_info_test() ->
    AppDir = code:lib_dir(inferno),
    {ok, S} = ?SRV:start_link([]),
    ?SRV:add_application(S, inferno, AppDir),
    Info = ?SRV:module_info(S, ?SRV, [description, title]),
    ?assertMatch([_, _], Info),
    io:format(user, "module_info: ~p", [Info]),
    ok.

erl_key_maker(AppName) ->
    fun(FileName) -> {erl, AppName, filename_to_module_name(FileName)} end.

beam_key_maker(AppName) ->
    fun(FileName) -> {beam, AppName, filename_to_module_name(FileName)} end.

refman_key_maker(AppName) ->
    fun(FileName) -> {refman, AppName, filename_to_module_name(FileName)} end.

filename_to_module_name(FileName) ->
    list_to_atom(filename:rootname(filename:basename(FileName))).


new_application(AppName) ->
    %% Create new application:
    %% - Create pie-servers.
    {ok, SrcPie} = dirmon_pie:start_link("\\.erl$",  erl_key_maker(AppName)),
    {ok, CmpPie} = dirmon_pie:start_link("\\.beam$", beam_key_maker(AppName)),
    {ok, ManPie} = dirmon_pie:start_link("\\.xml$",  refman_key_maker(AppName)),
    dirmon_pie:monitor(SrcPie),
    dirmon_pie:monitor(CmpPie),
    dirmon_pie:monitor(ManPie),
    #info_application{source_pie = SrcPie,
                      compiled_pie = CmpPie,
                      refman_pie = ManPie}.


handle_pie_event({EventType,{beam,AppName,ModName},FileName}, State
                 =#state{mod_tbl=ModTbl}) 
        when in(EventType, [added, modified]) ->
    M = get_or_create_module(ModTbl, ModName, AppName),
    ets:insert(ModTbl, M#info_module{compiled_filename=FileName}),
    State;
handle_pie_event({deleted,{beam,_AppName,ModName},_FileName}, State
                 =#state{mod_tbl=ModTbl}) ->
    ets:update_element(ModTbl, ModName,
                       {#info_module.compiled_filename, undefined}),
    State;

handle_pie_event({EventType,{erl,AppName,ModName},FileName}, State
                 =#state{mod_tbl=ModTbl, fun_tbl=FunTbl}) 
        when in(EventType, [added, modified]) ->
    M@ = get_or_create_module(ModTbl, ModName, AppName),
    M@ = M@#info_module{source_filename=FileName, is_analysed=false},
    clear_functions(ModName, FunTbl),
    ets:insert(ModTbl, M@),
    State;
handle_pie_event({deleted,{erl,_AppName,ModName},_FileName}, State
                 =#state{mod_tbl=ModTbl}) ->
    ets:update_element(ModTbl, ModName,
                       {#info_module.source_filename, undefined}),
    State;

handle_pie_event({EventType,{refman,AppName,ModName},FileName}, State
                 =#state{mod_tbl=ModTbl, fun_tbl=FunTbl, cache=Cache})
        when in(EventType, [added, modified]) ->
    M@ = get_or_create_module(ModTbl, ModName, AppName),
    M@ = M@#info_module{refman_filename=FileName, is_analysed=false},
    clear_functions(ModName, FunTbl),
    ets:insert(ModTbl, M@),
    State;
handle_pie_event({deleted,{refman,_AppName,ModName},_FileName}, State
                 =#state{mod_tbl=ModTbl}) ->
    ets:update_element(ModTbl, ModName,
                       {#info_module.refman_filename, undefined}),
    State.



get_or_create_module(ModTbl, ModName, AppName) ->
    case ets:lookup(ModTbl, ModName) of
        []  -> #info_module{name=ModName, application_name=AppName};
        [M] -> M
    end.



clear_functions(ModName, FunTbl) ->
    ets:match_delete(FunTbl, #info_function{_='_', module_name=ModName}).

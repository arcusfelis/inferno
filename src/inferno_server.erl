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
        function_tbl :: ets:tid(),
        module_tbl :: ets:tid(),
        mod_rec_f2p :: dict(),
        fun_rec_f2p :: dict()
}).


-record(function_info, {
        mfa,
        fields
}).

-record(module_info, {
        module_name,
        fields
}).

-record(add_application, {
        name,
        directory
}).

-record(module_names_to_compiled_filenames, {
        module_names
}).

-record(application_names_to_directories, {
        app_names
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
init([_Params]) ->
    FunTable = ets:new(inferno_function_table, [{keypos, #info_function.mfa}]),
    ModTable = ets:new(inferno_module_table, [{keypos, #info_module.name}]),
    %% Collect info about positions of the record.
    ModRecF2P = fields_to_position_dict(record_info(fields, info_module)),
    FunRecF2P = fields_to_position_dict(record_info(fields, info_function)),
    State = #state{
            module2source_filename = dict:new(),
            module2compiled_filename = dict:new(),
            module2doc_filename = dict:new(),
            app2dir_filename = dict:new(),
            function_tbl = FunTable,
            module_tbl = ModTable,
            mod_rec_f2p = ModRecF2P,
            fun_rec_f2p = FunRecF2P
    },
    {ok, State}.

 
%% @private
handle_call(#function_info{mfa = MFA, fields = FieldNames}, _From, State) ->
    Reply = 
    do([error_m || 
        _ModuleStatus <- check_module(mfa_to_module_name(MFA), State),
        Info <- lookup_fields(State#state.function_tbl, %% table
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
        Info <- lookup_fields(State#state.module_tbl,   %% table
                              ModuleName,               %% key
                              FieldNames,               %% atoms
                              State#state.mod_rec_f2p), %% meta info
        return(Info)
       ]),
    {reply, Reply, State};

handle_call(#add_application{name = AppName, directory = AppDir},
            _From, State) ->
    M2SF = dict:from_list(inferno_lib:source_files(AppDir)),
    M2CF = dict:from_list(inferno_lib:compiled_files(AppDir)),
    M2DF = dict:from_list(inferno_lib:doc_files(AppDir)),
    State2 = State#state{ module2source_filename   = patch_dict(old(), M2SF),
                          module2compiled_filename = patch_dict(old(), M2CF),
                          module2doc_filename      = patch_dict(old(), M2DF),
                          app2dir_filename         = dict:store(AppName, AppDir, old())},
    {reply, {ok, ok}, State2};

handle_call(#module_names_to_compiled_filenames{module_names = ModuleNames},
            _From, State=#state{module2compiled_filename = M2CF}) ->
    Reply = [{ModuleName, maybe_name_to_filename(ModuleName, M2CF)}
            || ModuleName <- ModuleNames],
    {reply, {ok, Reply}, State};

handle_call(#application_names_to_directories{app_names = AppNames},
            _From, State=#state{app2dir_filename = A2D}) ->
    Reply = [{AppName, maybe_name_to_filename(AppName, A2D)}
            || AppName <- AppNames],
    {reply, {ok, Reply}, State}.


%% @private
handle_cast(_Mess, State) ->
    {noreply, State}.


%% @private
handle_info(_Mess, State) ->
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
    #state{module2source_filename = M2F,
           module2compiled_filename = M2CF,
           module2doc_filename = M2DF,
           module_tbl = ModTable, 
           function_tbl = FunTable} = State,
    case ets:member(ModTable, ModuleName) of
        false ->
            Result = analyse_module(ModuleName, M2F, M2CF, M2DF, 
                                    ModTable, FunTable),
            %% Put the empty module, if an error.
            %% It helps to escape of file reading again.
            [ets:insert(ModTable, #info_module{name = ModuleName})
             || error =:= element(1, Result)],
            Result;
        true ->
            {ok, already_loaded}
    end.


-spec analyse_module(ModuleName, M2F, M2CF, M2DF, ModTable, FunTable) -> 
    {ok, Status} | {error, Error} when
    ModuleName :: atom(),
    M2F :: dict(),
    M2CF :: dict(),
    M2DF :: dict(),
    ModTable :: ets:tid(),
    FunTable :: ets:tid(),
    Status :: loaded,
    Error :: atom().

analyse_module(ModuleName, M2F, M2CF, M2DF, ModTable, FunTable) ->
    do([error_m ||
        FileName <- name_to_filename(ModuleName, M2F),
        XML <- inferno_edoc_xml_reader:filename_to_edoc_xml(FileName),
        MFA2PosDict <- inferno_lib:filename_to_function_positions(FileName),
        %% src/Module.erl
        SrcIM = inferno_edoc_xml_reader:handle_module(XML),
        %% doc/src/Module.xml
        %% DocIM is undefined, if there is no the xml file.
        DocIM = read_module_documentation(ModuleName, M2DF),
        IM1 = merge_modules(SrcIM, DocIM),
        IM2 = #info_module{functions = Funs} = 
            inferno_lib:set_positions(IM1, MFA2PosDict),
        BeamFN = maybe_name_to_filename(ModuleName, M2CF),
        IM3 = IM2#info_module{functions = undefined, 
                              compiled_filename = BeamFN},
        true = ets:insert(ModTable, IM3),
        true = ets:insert(FunTable, Funs),
        return(loaded)
       ]).


name_to_filename(Name, N2F) ->
    case dict:find(Name, N2F) of
        {ok, _FileName} = X -> X;
        error -> {error, module_not_found}
    end.

maybe_name_to_filename(Name, N2F) ->
    case dict:find(Name, N2F) of
        {ok, FileName} -> FileName;
        error -> undefined
    end.


%% ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%% merge_modules
%% ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

%% @doc Update IM1 with values of IM2.
-spec merge_modules(IM, IM) -> IM when
    IM :: #info_module{} | undefined.

merge_modules(undefined, undefined) -> erlang:error(badarg);
merge_modules(IM, undefined) -> IM;
merge_modules(undefined, IM) -> IM;
merge_modules(IM1, IM2) ->
    Fields = record_info(fields, info_module),
    Vals1 = record_values(IM1),
    Vals2 = record_values(IM2),
    Vals3 = lists:zipwith3(fun merge_module_fields/3, Fields, Vals1, Vals2),
    list_to_tuple([info_module|Vals3]).


-spec merge_module_fields(FieldName, Val, Val) -> Val when
    FieldName :: atom(),
    Val :: term().

merge_module_fields(_FieldName, X, undefined) -> X;
merge_module_fields(_FieldName, undefined, Y) -> Y;
merge_module_fields(functions, X, Y) -> 
    %% For each element of the list X, update it with values of the element 
    %% from Y.
    lists2:ordkeymerge_with(#info_function.mfa, fun merge_functions/2, X, Y);
merge_module_fields(_FieldName, _X, Y) -> Y.



-spec merge_functions(IF, IF) -> IF when
    IF :: #info_function{} | undefined.

merge_functions(undefined, undefined) -> erlang:error(badarg);
merge_functions(IF, undefined) -> IF;
merge_functions(undefined, IF) -> IF;
merge_functions(IF1, IF2) ->
    Fields = record_info(fields, info_function),
    Vals1 = record_values(IF1),
    Vals2 = record_values(IF2),
    Vals3 = lists:zipwith3(fun merge_function_fields/3, Fields, Vals1, Vals2),
    list_to_tuple([info_function|Vals3]).


-spec merge_function_fields(FieldName, Val, Val) -> Val when
    FieldName :: atom(),
    Val :: term().

merge_function_fields(_FieldName, X, undefined) -> X;
merge_function_fields(_FieldName, _X, Y) -> Y.


record_values(Rec) ->
    tl(tuple_to_list(Rec)).


%% ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%% read_module_documentation
%% ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

read_module_documentation(ModuleName, M2DF) ->
    case dict:find(ModuleName, M2DF) of
        {ok, FileName} -> 
            case inferno_refman_xml_reader:filename_to_xml(FileName) of
                {ok, XML} -> inferno_refman_xml_reader:handle_module(XML);
                {error, Reason} -> 
                    error_logger:error_msg("inferno_refman_xml_reader "
                        "returns ~p. Ignore and continue.~n", [Reason]),
                    undefined
            end;
        error -> undefined
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


fields_to_position_dict(Fields) ->
    dict:from_list(fields_to_position_pl(Fields)).


fields_to_position_pl(Fields) ->
    fields_to_position_pl(Fields, 2).

fields_to_position_pl([H|T], N) ->
    [{H, N} | fields_to_position_pl(T, N+1)];
fields_to_position_pl([], _N) ->
    [].


%% New elements will be added, old ones will be replaced.
patch_dict(OldDict, NewDict) ->
    dict:merge(fun(_K,_Old,New) -> New end, OldDict, NewDict).


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



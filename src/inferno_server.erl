%%% @doc This module is a `gen_server' that stores information about a 
%%%      single application.
%%% @end
-module(inferno_server).
-behaviour(gen_server).
-compile({parse_transform, rum}).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/1,
         start/1,
         module_info/3,
         function_info/3,
         add_application/2]).


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


-spec add_application(x_server(), filename:directory()) -> ok.

add_application(Server, AppDir) ->
    call(Server, #add_application{directory = AppDir}).

 
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

handle_call(#add_application{directory = AppDir},
            _From, State) ->
    M2SF = dict:from_list(inferno_lib:source_files(AppDir)),
    M2CF = dict:from_list(inferno_lib:compiled_files(AppDir)),
    State2 = State#state{ module2source_filename   = patch_dict(old(), M2SF),
                          module2compiled_filename = patch_dict(old(), M2CF)},
    {reply, {ok, ok}, State2}.

%% New elements will be added, old ones will be replaced.
patch_dict(OldDict, NewDict) ->
    dict:merge(fun(_K,_Old,New) -> New end, OldDict, NewDict).

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
           module_tbl = ModTable, 
           function_tbl = FunTable} = State,
    case ets:member(ModTable, ModuleName) of
        false ->
            Result = analyse_module(ModuleName, M2F, ModTable, FunTable),
            %% Put the empty module, if an error.
            %% It helps to escape of file reading again.
            [ets:insert(ModTable, #info_module{name = ModuleName})
             || error =:= element(1, Result)],
            Result;
        true ->
            {ok, already_loaded}
    end.


-spec analyse_module(ModuleName, M2F, ModTable, FunTable) -> 
    {ok, Status} | {error, Error} when
    ModuleName :: atom(),
    M2F :: dict(),
    ModTable :: ets:tid(),
    FunTable :: ets:tid(),
    Status :: loaded,
    Error :: atom().

analyse_module(ModuleName, M2F, ModTable, FunTable) ->
    do([error_m ||
        FileName <- module_name_to_filename(ModuleName, M2F),
        XML <- inferno_lib:filename_to_edoc_xml(FileName),
        MFA2PosDict <- inferno_lib:filename_to_function_positions(FileName),
        IM = inferno_lib:handle_module(XML),
        IM2 = #info_module{functions = Funs} = 
            inferno_lib:set_positions(IM, MFA2PosDict),
        IM3 = IM2#info_module{functions = undefined},
        true = ets:insert(ModTable, IM3),
        true = ets:insert(FunTable, Funs),
        return(loaded)
       ]).


module_name_to_filename(ModuleName, M2F) ->
    case dict:find(ModuleName, M2F) of
        {ok, _FileName} = X -> X;
        error -> {error, module_not_found}
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

%% ------------------------------------------------------------------
%% Tests
%% ------------------------------------------------------------------

function_info_test() ->
    AppDir = code:lib_dir(inferno),
    {ok, S} = ?SRV:start_link([]),
    ?SRV:add_application(S, AppDir),
    Info = ?SRV:function_info(S, {?SRV, start_link, 1},
                              [description, title, position]),
    ?assertMatch([_, _, _], Info),
    io:format(user, "function_info: ~p", [Info]),
    ok.


module_info_test() ->
    AppDir = code:lib_dir(inferno),
    {ok, S} = ?SRV:start_link([]),
    ?SRV:add_application(S, AppDir),
    Info = ?SRV:module_info(S, ?SRV, [description, title]),
    ?assertMatch([_, _], Info),
    io:format(user, "module_info: ~p", [Info]),
    ok.



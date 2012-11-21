%%% @doc This module is a `gen_server' that stores information about a 
%%%      single application.
%%% @end
-module(inferno_server).
-behaviour(gen_server).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/2,
         mfa_to_description/2,
         module_name_to_description/2]).



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
        module2filename :: dict(),
        function_tbl :: ets:tid(),
        module_tbl :: ets:tid()
}).


-record(mfa_to_description, {
        mfa
}).

-record(module_name_to_description, {
        module_name
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
-spec start_link(filename:directory(), [term()]) -> {ok, x_server()}.

start_link(Path, Params) ->
    Args = [Path, Params],
    case proplists:get_value(name, Params) of
        undefined ->
            gen_server:start_link(?MODULE, Args, []);
        A when is_atom(A) ->
            gen_server:start_link({local, A}, ?MODULE, Args, []);
        Name ->
            gen_server:start_link(Name, ?MODULE, Args, [])
    end.


mfa_to_description(Server, MFA) ->
    call(Server, #mfa_to_description{mfa = MFA}).

module_name_to_description(Server, ModuleName) ->
    call(Server, #module_name_to_description{module_name = ModuleName}).

 
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
init([AppDir, _Params]) ->
    FunTable = ets:new(inferno_function_table, [{keypos, #info_function.mfa}]),
    ModTable = ets:new(inferno_module_table, [{keypos, #info_module.name}]),
    State = #state{
            module2filename = dict:from_list(inferno_lib:source_files(AppDir)),
            function_tbl = FunTable,
            module_tbl = ModTable
    },
    {ok, State}.

 
%% @private
handle_call(#mfa_to_description{mfa = MFA}, _From, State) ->
    Reply = 
    do([error_m || 
        _ModuleStatus <- check_module(mfa_to_module_name(MFA), State),
        Description <- lookup_element(State#state.function_tbl, 
                                      MFA, 
                                      #info_function.description),
        return(Description)
       ]),
    {reply, Reply, State};

handle_call(#module_name_to_description{module_name = ModuleName}, 
            _From, State) ->
    Reply = 
    do([error_m || 
        _ModuleStatus <- check_module(ModuleName, State),
        Description <- lookup_element(State#state.module_tbl, 
                                      ModuleName, 
                                      #info_module.description),
        return(Description)
       ]),
    {reply, Reply, State}.


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
    #state{module2filename = M2F,
           module_tbl = ModTable, 
           function_tbl = FunTable} = State,
    case ets:member(ModTable, ModuleName) of
        false ->
            analyse_module(ModuleName, M2F, ModTable, FunTable);
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
        IM = #info_module{functions = Funs} = inferno_lib:handle_module(XML),
        IM2 = IM#info_module{functions = undefined},
        true = ets:insert(ModTable, IM2),
        true = ets:insert(FunTable, Funs),
        return(loaded)
       ]).


module_name_to_filename(ModuleName, M2F) ->
    case dict:find(ModuleName, M2F) of
        {ok, _FileName} = X -> X;
        error -> {error, module_not_found}
    end.




%% ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%% Data extraction
%% ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

lookup_element(Tab, Key, Pos) ->
    try
        {ok, ets:lookup_element(Tab, Key, Pos)}
    catch error:badarg ->
        {error, elem_not_found}
    end.


%% ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%% Helpers
%% ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

mfa_to_module_name({M, _F, _A}) -> M.


%% ------------------------------------------------------------------
%% Tests
%% ------------------------------------------------------------------

mfa_to_description_test() ->
    AppDir = code:lib_dir(inferno),
    {ok, S} = ?SRV:start_link(AppDir, []),
    Desc = ?SRV:mfa_to_description(S, {?SRV, start_link, 2}),
    io:format(user, "mfa_to_description: ~ts", [Desc]),
    ok.


module_name_to_description_test() ->
    AppDir = code:lib_dir(inferno),
    {ok, S} = ?SRV:start_link(AppDir, []),
    Desc = ?SRV:module_name_to_description(S, ?SRV),
    io:format(user, "module_name_to_description: ~ts", [Desc]),
    ok.

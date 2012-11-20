%%% @doc This module is a `gen_server' that handles a single port connection.
-module(inferno_server).
-behaviour(gen_server).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/2,
         mfa_to_description/2]).



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


%% ------------------------------------------------------------------
%% Import code
%% ------------------------------------------------------------------

-include_lib("inferno/include/inferno.hrl").


%% ------------------------------------------------------------------
%% Declare parse transforms
%% ------------------------------------------------------------------

-compile({parse_transform, seqbind}).


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
            module2filename = dict:from_list(include_lib:source_files(AppDir)),
            function_tbl = FunTable,
            module_tbl = ModTable
    },
    {ok, State}.

 
%% @private
handle_call(#mfa_to_description{mfa = MFA}, _From, State) ->
    {Module, Function, Arity} = MFA,
    check_module(Module, State),
    Reply = ok,
    {reply, Reply, State}.


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

check_module(Module, State) ->
    ok.


analyse_module(ModuleName, M2F) ->
    XML = inferno_lib:filename_to_edoc_xml(
            module_name_to_filename(ModuleName, M2F)),
    IM = #info_module{functions = Funs} = include_lib:handle_module(XML),
    ok.


module_name_to_filename(ModuleName, M2F) ->
    dict:fetch(ModuleName, M2F).

-module(inferno_pos_reader).
-export([fill/2]).
-include_lib("inferno/include/inferno.hrl").
-include_lib("eunit/include/eunit.hrl").

fill(InM=#info_module{source_filename = FileName}, Cache) ->
    Hash = [inferno_lib:file_hash(FileName),
            inferno_lib:module_ast_hash(?MODULE)],
    Key = {pos, FileName},
    case inferno_cache:get(Cache, Key) of
        {Hash, Fun2Pos} ->
            OutM = set_positions(InM, Fun2Pos),
            inferno_lib:merge_modules(InM, OutM);

        %% undefined or {_OtherHash, _OtherFun2Pos}
        _Other ->
            case filename_to_function_positions(FileName) of
                {ok, Fun2Pos} ->
                    inferno_cache:put(Cache, Key, FileName, {Hash, Fun2Pos}),
                    OutM = set_positions(InM, Fun2Pos),
                    inferno_lib:merge_modules(InM, OutM);
                {error, Reason} ->
                    lager:error("Parser error: ~p~n", [Reason]),
                    InM
            end
    end.

%% ------------------------------------------------------------------
%% These functions allows to extract function positions from AST
%% ------------------------------------------------------------------

set_positions(M=#info_module{functions = Funs}, Fun2Pos) ->
    NewFuns = [set_function_position(F, Fun2Pos) || F <- Funs],
    M#info_module{functions = NewFuns}.
    
set_function_position(F=#info_function{mfa = MFA}, Fun2Pos) ->
    try
        dict:fetch(MFA, Fun2Pos)
    of Pos ->
        F#info_function{position = Pos}
    catch error:_Reason ->
        error_logger:info_msg("Unknown position of the function ~p.", [MFA]),
        F
    end.


filename_to_function_positions(FileName) ->
    case epp_dodger:parse_file(FileName, []) of
        {ok, Forms} ->
            ModForm = lists2:filter_head(is_attribute_form(module), Forms),
            ModName = erl_syntax:atom_value(hd(
                erl_syntax:attribute_arguments(ModForm))),
            Dict = dict:from_list(function_positions(Forms, ModName)),
            {ok, Dict};
        {error, _} = Error ->
            Error
    end.
    


function_positions(Forms, ModName) ->
    [{to_mfa(ModName, F), erl_syntax:get_pos(F)}
     || F <- Forms, erl_syntax:type(F) =:= function].


to_mfa(ModName, F) ->
    FunName = erl_syntax:atom_value(erl_syntax:function_name(F)),
    {ModName, FunName, erl_syntax:function_arity(F)}.


is_attribute_form(Name) ->
    fun(Form) -> is_attribute_form(Form, Name) end.

is_attribute_form(Form, Name) ->
    try
        erl_syntax:atom_value(erl_syntax:attribute_name(Form)) =:= Name
    catch error:_ ->
        false
    end.


% TODO: fix me
lib_module_positions_test() ->
    FileName = code:lib_dir(inferno) ++ "/src/inferno_lib.erl",
    {ok, MFA2PosDict} = filename_to_function_positions(FileName),
    io:format(user, "MFA2PosDict: ~p", [dict:to_list(MFA2PosDict)]),
    ok.

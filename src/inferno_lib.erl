%% @doc This module parses edoc info into records.
-module(inferno_lib).
-export([source_files/1,
         compiled_files/1,
         doc_files/1,
         set_positions/2,
         filename_to_function_positions/1]).

-include_lib("xmerl/include/xmerl.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("inferno/include/inferno.hrl").
-compile({parse_transform, seqbind}).




%% @doc Return a map from a module name to its source code's location.
-spec source_files(AppDir) -> [{ModuleName, FileName}] when
        AppDir :: filename:dirname(),
        ModuleName :: atom(),
        FileName :: file:filename().

source_files(AppDir) ->
    Fun = fun(FileName, Acc) ->
        ModuleName = list_to_atom(filename:basename(FileName, ".erl")),
        [{ModuleName, FileName} | Acc]
        end,

    RegExp = ".erl$",
    AccIn  = [],
    %% Handle files recursivelly in the directory.
    filelib:fold_files(AppDir, RegExp, true, Fun, AccIn).


%% @doc Return a map from a module name to its compiled file's location.
-spec compiled_files(AppDir) -> [{ModuleName, FileName}] when
        AppDir :: filename:dirname(),
        ModuleName :: atom(),
        FileName :: file:filename().

compiled_files(AppDir) ->
    Fun = fun(FileName, Acc) ->
        ModuleName = list_to_atom(filename:basename(FileName, ".beam")),
        [{ModuleName, FileName} | Acc]
        end,

    RegExp = ".beam$",
    AccIn  = [],
    %% Handle files recursivelly in the directory.
    filelib:fold_files(AppDir, RegExp, true, Fun, AccIn).


%% @doc Return a map from a module name to its Module.xml's location.
-spec doc_files(AppDir) -> [{ModuleName, FileName}] when
        AppDir :: filename:dirname(),
        ModuleName :: atom(),
        FileName :: file:filename().

doc_files(AppDir) ->
    SrcDocDir = filename:join([AppDir, doc, src]),
    case filelib:is_dir(SrcDocDir) of
        false -> []; %% There is no directory with data.
        true -> find_doc_files(SrcDocDir)
    end.


find_doc_files(SrcDocDir) ->
    Fun = fun(FileName, Acc) ->
        ModuleName = list_to_atom(filename:basename(FileName, ".xml")),
        [{ModuleName, FileName} | Acc]
        end,

    RegExp = ".xml$",
    AccIn  = [],
    %% Handle files recursivelly in the directory.
    filelib:fold_files(SrcDocDir, RegExp, true, Fun, AccIn).


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



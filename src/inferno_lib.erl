%% @doc This module parses edoc info into records.
-module(inferno_lib).
-export([source_files/1,
         compiled_files/1,
         handle_module/1,
         filename_to_edoc_xml/1,
         set_positions/2,
         filename_to_function_positions/1]).

-include_lib("xmerl/include/xmerl.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("inferno/include/inferno.hrl").
-compile({parse_transform, seqbind}).


filename_to_edoc_xml(FileName) ->
    try
        {_, XML} = edoc:get_doc(FileName,
                                [{private, true}, {hidden, true}]),
        {ok, XML}
    catch Type:Error ->
        {error, {Type, Error, erlang:get_stacktrace()}}
    end.




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


%% ------------------------------------------------------------------
%% Handle a module node
%% ------------------------------------------------------------------

%% @doc Transform a module XML element into a record.
handle_module(#xmlElement{name = module, attributes = Attrs, content = Con}) ->
    X@ = #info_module{},
    X@ = lists:foldl(fun handle_module_element/2, X@, Con),
    X@ = lists:foldl(fun handle_module_attribute/2, X@, Attrs),
    set_function_module_names(X@).

set_function_module_names(M=#info_module{name = ModuleName, functions = Funs}) ->
    NewFuns = [set_function_mfa(F#info_function{module_name = ModuleName})
               || F <- Funs],
    M#info_module{functions = NewFuns}.



handle_module_element(#xmlElement{name = description, content = Con}, X) ->
    lists:foldl(fun handle_module_description/2, X, Con);
handle_module_element(#xmlElement{name = functions, content = Con}, X) ->
    Functions = lists:map(fun handle_function/1, Con),
    X#info_module{functions = Functions};
%handle_module_element(#xmlElement{name = Name, content = Con}, X) ->
%    io:format(user, "Bad name: ~p~n", [Name]),
%    X;
handle_module_element(_, X) ->
    X.

handle_module_description(#xmlElement{name = briefDescription, content = Con}, X) ->
    X#info_module{title = elems_to_text(Con)};
handle_module_description(#xmlElement{name = fullDescription, content = Con}, X) ->
    X#info_module{description = elems_to_text(Con)}.



handle_module_attribute(#xmlAttribute{name = name, value = Value}, X) ->
    X#info_module{name = list_to_atom(Value)};
handle_module_attribute(_, X) ->
    X.


%% ------------------------------------------------------------------
%% Handle a function node
%% ------------------------------------------------------------------

handle_function(#xmlElement{name = function, attributes = Attrs, content = Con}) ->
    X@ = #info_function{},
    X@ = lists:foldl(fun handle_function_element/2, X@, Con),
    lists:foldl(fun handle_function_attribute/2, X@, Attrs).


set_function_mfa(X=#info_function{module_name = M, name = F, arity = A}) ->
    X#info_function{mfa = {M, F, A}}.


handle_function_element(#xmlElement{name = description, content = Con}, X) ->
    lists:foldl(fun handle_function_description/2, X, Con);
handle_function_element(_, X) ->
    X.

handle_function_description(#xmlElement{name = briefDescription, content = Con}, X) ->
    X#info_function{title = elems_to_text(Con)};
handle_function_description(#xmlElement{name = fullDescription, content = Con}, X) ->
    X#info_function{description = elems_to_text(Con)}.



handle_function_attribute(#xmlAttribute{name = name, value = Value}, X) ->
    X#info_function{name = list_to_atom(Value)};
handle_function_attribute(#xmlAttribute{name = arity, value = Value}, X) ->
    X#info_function{arity = list_to_integer(Value)};
handle_function_attribute(#xmlAttribute{name = exported, value = Value}, X) ->
    X#info_function{is_exported = attr_to_boolean(Value)};
handle_function_attribute(_, X) ->
    X.

%% ------------------------------------------------------------------
%% Utilities
%% ------------------------------------------------------------------

elems_to_text(XmlElems) ->
    unicode:characters_to_binary(elems_to_iolist(XmlElems)).

elems_to_iolist([#xmlText{value = Text}|T]) ->
    [Text|elems_to_iolist(T)];
elems_to_iolist([#xmlElement{content = Con}|T]) ->
    elems_to_iolist(Con) ++ elems_to_iolist(T);
elems_to_iolist([]) ->
    [].

attr_to_boolean("yes") -> true;
attr_to_boolean(_) -> false.


%% ------------------------------------------------------------------
%% Tests
%% ------------------------------------------------------------------

this_module_test() ->
    FileName = code:lib_dir(inferno) ++ "/src/inferno_lib.erl",
    {ok, XML} = filename_to_edoc_xml(FileName),
    ModRec = handle_module(XML),
    io:format(user, "ModRec: ~p", [ModRec]),
    ok.

this_module_positions_test() ->
    FileName = code:lib_dir(inferno) ++ "/src/inferno_lib.erl",
    {ok, MFA2PosDict} = filename_to_function_positions(FileName),
    io:format(user, "MFA2PosDict: ~p", [dict:to_list(MFA2PosDict)]),
    ok.




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



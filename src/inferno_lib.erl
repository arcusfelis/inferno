%% @doc This module parses edoc info into records.
-module(inferno_lib).
-export([handle_application/2, 
         source_files/2,
         handle_module/1,
         filename_to_edoc_xml/1]).

-include_lib("xmerl/include/xmerl.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("inferno/include/inferno.hrl").
-compile({parse_transform, seqbind}).



handle_application(AppDir, IsValidModule) ->
    [{ModuleName, handle_module(filename_to_edoc_xml(FileName))}
     || {ModuleName, FileName} <- source_files(AppDir), IsValidModule(ModuleName)].


filename_to_edoc_xml(FileName) ->
    {_, XML}  = edoc:get_doc(FileName,
                             [{private, true}, {hidden, true}]),
    XML.



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
    NewFuns = [F#info_function{module_name = ModuleName} || F <- Funs],
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
    X@ = set_function_mfa(X@),
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
    XML = filename_to_edoc_xml(FileName),
    ModRec = handle_module(XML),
    io:format(user, "ModRec: ~p", [ModRec]),
    ok.



this_application_test() ->
    AppDir = code:lib_dir(inferno),
    handle_application(AppDir, fun(ModuleName) -> ModuleName =:= inferno_lib end),
    ok.

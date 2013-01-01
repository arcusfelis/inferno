%% @doc This module parses edoc info into records.
-module(inferno_edoc_slow_reader).
-export([fill/1]).

-include_lib("xmerl/include/xmerl.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("inferno/include/inferno.hrl").
-compile({parse_transform, seqbind}).
-compile({parse_transform, rum}).


fill(InM=#info_module{source_filename = FileName}) ->
    case parse_file(FileName) of
        {ok, OutM} ->
            inferno_lib:merge_modules(InM, OutM);
        {error, Reason} ->
            lager:error("Parser error: ~p~n", [Reason]),
            InM
    end.


parse_file(FileName) ->
    case filename_to_edoc_xml(FileName) of
        {ok, XML} ->
            try_handle_module(FileName, XML);
        {error, Reason} ->
            {error, {Reason, FileName}}
    end.


try_handle_module(FileName, XML) ->
    try
        {ok, handle_module(XML)}
    catch error:Reason ->
        {error, {Reason, FileName, erlang:get_stacktrace()}}
    end.
        

filename_to_edoc_xml(FileName) ->
    try
        {_, XML} = edoc:get_doc(FileName,
                                [{private, true}, {hidden, true}]),
        {ok, XML}
    catch Type:Error ->
        {error, {Type, Error, erlang:get_stacktrace()}}
    end.
        


%% ------------------------------------------------------------------
%% Handle a module node
%% ------------------------------------------------------------------

%% @doc Transform a module XML element into a record.
handle_module(#xmlElement{name = module, attributes = Attrs, content = Con}) ->
    X@ = #info_module{},
    X@ = lists:foldl(fun handle_module_element/2, X@, Con),
    X@ = lists:foldl(fun handle_module_attribute/2, X@, Attrs),
    X@ = set_function_module_names(X@),
    sort_functions(X@).

set_function_module_names(M=#info_module{name = ModuleName, functions = Funs}) ->
    NewFuns = [set_function_mfa(F#info_function{module_name = ModuleName})
               || F <- Funs],
    M#info_module{functions = NewFuns}.

sort_functions(M) ->
    M#info_module{functions = lists:keysort(#info_function.mfa, old())}.



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
    empty_to_undef(unicode:characters_to_binary(elems_to_iolist(XmlElems))).

elems_to_iolist([#xmlText{value = Text}|T]) ->
    [Text|elems_to_iolist(T)];
elems_to_iolist([#xmlElement{content = Con}|T]) ->
    elems_to_iolist(Con) ++ elems_to_iolist(T);
elems_to_iolist([]) ->
    [].

attr_to_boolean("yes") -> true;
attr_to_boolean(_) -> false.

empty_to_undef(<<>>) -> undefined;
empty_to_undef(Bin)  -> Bin.

%% ------------------------------------------------------------------
%% Tests
%% ------------------------------------------------------------------

this_module_test() ->
    FileName = code:lib_dir(inferno) ++ "/src/inferno_lib.erl",
    F1 = fun() -> 
        {ok, XML} = filename_to_edoc_xml(FileName), handle_module(XML)
     end,
    F2 = fun(MicroSeconds) -> io:format(user, "Parsed for ~p.~n", [MicroSeconds]) end,
    ModRec = inferno_lib:measure_time(F1, F2),
    io:format(user, "ModRec: ~p", [ModRec]),
    ok.

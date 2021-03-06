%% @doc This module parses refman info into records.
-module(inferno_refman_slow_reader).
-export([fill/2]).

-include_lib("xmerl/include/xmerl.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("inferno/include/inferno.hrl").
-compile({parse_transform, seqbind}).
-compile({parse_transform, rum}).


fill(InM=#info_module{refman_filename = undefined}, _Cache) -> InM;
fill(InM=#info_module{refman_filename = FileName}, Cache) ->
    Hash = [inferno_lib:file_hash(FileName),
            inferno_lib:module_ast_hash(?MODULE)],
    Key = {edoc, FileName},
    case inferno_cache:get(Cache, Key) of
        {Hash, OutM} ->
            inferno_lib:merge_modules(InM, OutM);

        %% undefined or {_OtherHash, _OtherFun2Pos}
        _Other ->
            case parse_file(FileName) of
                {ok, OutM} ->
                    inferno_cache:put(Cache, Key, FileName, {Hash, OutM}),
                    inferno_lib:merge_modules(InM, OutM);
                {error, Reason} ->
                    lager:error("Parser error: ~p~n", [Reason]),
                    InM
            end
    end.


parse_file(FileName) ->
    case filename_to_xml(FileName) of
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
        



filename_to_xml(FileName) ->
    DocGenPrivDirName = code:priv_dir(erl_docgen),
    FetchPath = [filename:join(DocGenPrivDirName, dtd),
                 filename:join(DocGenPrivDirName, dtd_html_entities)],
    try
        xmerl_scan:file(FileName, [{fetch_path, FetchPath}, 
                                   {comments, false}, 
                                   {space, normalize}]) 
    of
        {error, Reason} ->
            {error, Reason};
        {Doc, _Rest} ->
            {ok, Doc}
    catch error:Reason ->
        {error, {error, Reason, erlang:get_stacktrace()}}
    end.
            


%% ------------------------------------------------------------------
%% Handle a module node
%% ------------------------------------------------------------------


%% @doc Transform a module XML element into a record.
handle_module(XML) ->
    handle_erlref(XML).


handle_erlref(#xmlElement{name = erlref, content = Con}) ->
    X@ = #info_module{},
    X@ = lists:foldl(fun handle_erlref_element/2, X@, Con),
    X@ = set_function_module_names(X@),
    X@ = sort_functions(X@),
    validate_functions(X@).


set_function_module_names(M=#info_module{name = ModuleName, functions = Funs}) ->
    NewFuns = [set_function_mfa(F#info_function{module_name = ModuleName})
               || F <- Funs],
    M#info_module{functions = NewFuns}.

sort_functions(M) ->
    M#info_module{functions = lists:keysort(#info_function.mfa, old())}.

validate_functions(M=#info_module{functions = Funs}) ->
    {ValidFuns, InvalidFuns} = lists:partition(fun is_valid_function/1, Funs),
    [error_logger:error_msg("Invalid function: ~p~n", [X]) || X <- InvalidFuns],
    M#info_module{functions = ValidFuns}.

is_valid_function(#info_function{module_name = M, name = F, arity = A}) ->
    not lists:member(undefined, [M, F, A]).

set_function_mfa(X=#info_function{module_name = M, name = F, arity = A}) ->
    X#info_function{mfa = {M, F, A}}.


handle_erlref_element(#xmlElement{name = description, content = Con}, X) ->
    X#info_module{description = elems_to_text(Con)};
handle_erlref_element(#xmlElement{name = modulesummary, content = Con}, X) ->
    X#info_module{title = elems_to_text(Con)};
handle_erlref_element(#xmlElement{name = module, content = Con}, X) ->
    X#info_module{name = elems_to_atom(Con)};
handle_erlref_element(#xmlElement{name = funcs, content = Con}, X) ->
    Functions = [handle_function(E) || E=#xmlElement{} <- Con],
    X#info_module{functions = Functions};
handle_erlref_element(_, X) ->
    X.


%% ------------------------------------------------------------------
%% Handle a function node
%% ------------------------------------------------------------------

handle_function(#xmlElement{name = func, content = Con}) ->
    X@ = #info_function{is_exported = true},
    lists:foldl(fun handle_function_element/2, X@, Con).



handle_function_element(#xmlElement{name = name, attributes = Attrs, content = Con}, X@) ->
    FunDefinition = elems_to_text(Con),
    X@ = handle_function_definition(FunDefinition, X@),
    lists:foldl(fun handle_function_name_attribute/2, X@, Attrs);
handle_function_element(#xmlElement{name = fsummary, content = Con}, X) ->
    X#info_function{title = elems_to_text(Con)};
handle_function_element(#xmlElement{name = desc, content = Con}, X) ->
    X#info_function{description = elems_to_text(Con)};
handle_function_element(_, X) ->
    X.

%% There are 2 cases:
%%
%% <func><name>set_trace(Targets) -> void()</name></func>
%% or
%% <func><name name="set_trace" arity="1"/></func>

handle_function_name_attribute(#xmlAttribute{name = name, value = Value}, X) ->
    X#info_function{name = list_to_atom(Value)};
handle_function_name_attribute(#xmlAttribute{name = arity, value = Value}, X) ->
    X#info_function{arity = list_to_integer(Value)};
handle_function_name_attribute(_, X) ->
    X.


%% `FunDefinition' is `set_trace(Targets) -> void()'.
handle_function_definition(<<>>, X) ->
    X;
handle_function_definition(FunDefinition, X) ->
    case binary:split(FunDefinition, [<<$(>>, <<$)>>], [global]) of
        [Name, ArgsBin|_] ->
            Args = binary:split(ArgsBin, <<$,>>, [global, trim]),
            X#info_function{name = bin_to_trimmed_atom(Name),
                            arity = length(Args)};
        _ ->
            X
    end.


%% ------------------------------------------------------------------
%% Utilities
%% ------------------------------------------------------------------

elems_to_text(XmlElems) ->
    binary2:trim(unicode:characters_to_binary(elems_to_iolist(XmlElems)), $ ).

elems_to_iolist([#xmlText{value = Text}|T]) ->
    [Text|elems_to_iolist(T)];
elems_to_iolist([#xmlElement{content = Con}|T]) ->
    elems_to_iolist(Con) ++ elems_to_iolist(T);
elems_to_iolist([]) ->
    [].


elems_to_atom(XmlElems) ->
    binary_to_atom(elems_to_text(XmlElems), latin1).


bin_to_trimmed_atom(Bin) ->
    binary_to_atom(binary2:trim(Bin, $ ), latin1).


%% ------------------------------------------------------------------
%% Tests
%% ------------------------------------------------------------------

filename_test() ->
    FileName = code:lib_dir(inferno) ++ "/test/data/filename.xml",
    io:format(user, "~n", []),
    F1 = fun() -> {ok, XML} = filename_to_xml(FileName), handle_module(XML) end,
    F2 = fun(MicroSeconds) -> io:format(user, "~n[refman,slow]Parsed for ~p.~n", [MicroSeconds]) end,
    ModRec = inferno_lib:measure_time(F1, F2),
    io:format(user, "ModRec: ~p", [ModRec]),
    ok.


handle_function_definition_test_() ->
    [?_assertEqual(handle_function_definition(<<"set_trace(Targets) -> void()">>, 
                                              #info_function{}),
                   #info_function{name = set_trace, arity = 1})
    ,?_assertEqual(handle_function_definition(<<"accept_send(Ip, Port) -> boolean()">>, 
                                              #info_function{}),
                   #info_function{name = accept_send, arity = 2})
    ].

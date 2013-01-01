%% @doc This module parses edoc info into records.
-module(inferno_lib).
-export([source_files/1,
         compiled_files/1,
         doc_files/1,
         measure_time/2,
         merge_modules/2]).

-include_lib("xmerl/include/xmerl.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("inferno/include/inferno.hrl").
-compile({parse_transform, seqbind}).
-compile({parse_transform, gin}).




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





measure_time(ExecFn, FormatFn) ->
    Start = now(),
    try
        ExecFn()
    after
        Stop = now(),
        MicroSeconds = timer:now_diff(Stop, Start),
        FormatFn(MicroSeconds)
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



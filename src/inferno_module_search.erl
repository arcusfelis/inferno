-module(inferno_module_search).
-export([search/1]).
-include_lib("inferno/include/inferno.hrl").

-spec search(#info_application{}) -> [#info_module{}].
search(A=#info_application{}) ->
    EbinDir = compiled_directory(A),
    SrcDir  = source_directory(A),
    SFs = lists:keysort(1, source_files(SrcDir)),
    CFs = lists:keysort(1, compiled_files(EbinDir)),
    DFs = lists:keysort(1, doc_files(SrcDir)),
    {SFKeys, SFVals} = lists2:rotate(SFs),
    {CFKeys, CFVals} = lists2:rotate(CFs),
    {DFKeys, DFVals} = lists2:rotate(DFs),
    ModNames = ordsets:union([SFKeys, CFKeys, DFKeys]),
    SFNames = lists2:align_ordset(SFKeys, SFVals, ModNames),
    CFNames = lists2:align_ordset(CFKeys, CFVals, ModNames),
    DFNames = lists2:align_ordset(DFKeys, DFVals, ModNames),
    lists2:zip_with4(fun (ModName, SF, CF, DF) ->
                #info_module{name = ModName,
                             source_filename = SF,
                             compiled_filename = CF,
                             refman_filename = DF}
        end, [ModNames, SFNames, CFNames, DFNames]).




compiled_directory(#info_application{compiled_directory = undefined,
                                     source_directory = SrcDir}) ->
    SrcDir;
compiled_directory(#info_application{compiled_directory = EbinDir}) ->
    EbinDir.

source_directory(#info_application{source_directory = SrcDir}) ->
    SrcDir.


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

    RegExp = "\.erl$",
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

    RegExp = "\.beam$",
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

    RegExp = "\.xml$",
    AccIn  = [],
    %% Handle files recursivelly in the directory.
    filelib:fold_files(SrcDocDir, RegExp, true, Fun, AccIn).

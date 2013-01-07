-module(inferno_app_discover).
-export([]).
-include_lib("inferno/include/inferno.hrl").
-include_lib("eunit/include/eunit.hrl").


add_application(AppDir) ->
    %% FileType to FileNames proplist.

    SFs = lists:keysort(1, source_files(AppDir)),
    CFs = lists:keysort(1, compiled_files(AppDir)),
    DFs = lists:keysort(1, doc_files(AppDir)),

    {SFKeys, SFVals} = lists2:rotate(2, SFs),
    {CFKeys, CFVals} = lists2:rotate(2, CFs),
    {DFKeys, DFVals} = lists2:rotate(2, DFs),

    ModNames = ordsets:union([SFKeys, CFKeys, DFKeys]),

    SFNames = lists2:align_ordset(SFKeys, SFVals, ModNames),
    CFNames = lists2:align_ordset(CFKeys, CFVals, ModNames),
    DFNames = lists2:align_ordset(DFKeys, DFVals, ModNames),

    lists2:zip_with4(fun (ModName, SF, CF, DF) ->
                #info_module{name = ModName,
                             source_filename = SF,
                             compiled_filename = CF,
                             refman_filename = DF}
        end, ModNames, SFNames, CFNames, DFNames).


%% @doc Return a map from a module name to its source code's location.
-spec source_files(AppDir) -> [{ModuleName, FileName}] when
        AppDir :: filename:dirname(),
        ModuleName :: atom(),
        FileName :: file:filename().

source_files(AppDir) ->
    Dir = filename:join([AppDir, src]),
    Fun = fun(FileName, Acc) ->
        ModuleName = list_to_atom(filename:basename(FileName, ".erl")),
        [{ModuleName, FileName} | Acc]
        end,

    RegExp = "\.erl$",
    AccIn  = [],
    %% Handle files recursivelly in the directory.
    filelib:fold_files(Dir, RegExp, true, Fun, AccIn).


%% @doc Return a map from a module name to its compiled file's location.
-spec compiled_files(AppDir) -> [{ModuleName, FileName}] when
        AppDir :: filename:dirname(),
        ModuleName :: atom(),
        FileName :: file:filename().

compiled_files(AppDir) ->
    Dir = filename:join([AppDir, ebin]),
    Fun = fun(FileName, Acc) ->
        ModuleName = list_to_atom(filename:basename(FileName, ".beam")),
        [{ModuleName, FileName} | Acc]
        end,

    RegExp = "\.beam$",
    AccIn  = [],
    filelib:fold_files(Dir, RegExp, false, Fun, AccIn).


%% @doc Return a map from a module name to its Module.xml's location.
-spec doc_files(AppDir) -> [{ModuleName, FileName}] when
        AppDir :: filename:dirname(),
        ModuleName :: atom(),
        FileName :: file:filename().

doc_files(AppDir) ->
    Dir = filename:join([AppDir, doc, src]),
    Fun = fun(FileName, Acc) ->
        ModuleName = list_to_atom(filename:basename(FileName, ".xml")),
        [{ModuleName, FileName} | Acc]
        end,

    RegExp = "\.xml$",
    AccIn  = [],
    filelib:fold_files(Dir, RegExp, false, Fun, AccIn).



add_application_test() ->
    Res = add_application(code:lib_dir(inferno)),
    io:format(user, "Res: ~p~n", [Res]),
    ok.

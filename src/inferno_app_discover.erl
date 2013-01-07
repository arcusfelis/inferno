-module(inferno_app_discover).
-export([]).
-include_lib("inferno/include/inferno.hrl").
-include_lib("eunit/include/eunit.hrl").


add_application(AppDir) ->
    #info_application{name = AppName} = CurrentApp = application(AppDir),
    CurrentMods = application_modules(AppDir),
    SavedApp    = inferno_db:application(AppName),
    SavedMods   = inferno_db:application_modules(AppName),
    ok.


remove_application(AppDir) ->
    ok.


application(AppDir) ->
    AppFile = find_app_file(AppDir),
    %% TODO: AppCfg can be `undefined', handle this situation.
    {ok, AppCfg} = file:consult(AppFile),
    fold_app_cfg(AppCfg, #info_application{directories = [AppDir]}).


fold_app_cfg([{application, Name, MetaPL}|T], I) ->
    Title = proplists:get_value(description, MetaPL, ""),
    I2 = I#info_application{name = Name, title = Title},
    fold_app_cfg(T, I2);
fold_app_cfg([_|T], I) ->
    fold_app_cfg(T, I);
fold_app_cfg([], I) ->
    I.


application_modules(AppDir) ->
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



-spec app_src_files(AppDir) -> [{AppName, FileName}] when
        AppDir :: filename:dirname(),
        AppName :: atom(),
        FileName :: file:filename().

app_src_files(AppDir) ->
    Dir = filename:join([AppDir, src]),
    Fun = fun(FileName, Acc) ->
        AppName = list_to_atom(filename:basename(FileName, ".app.src")),
        [{AppName, FileName} | Acc]
        end,

    RegExp = "\.app\.src$",
    AccIn  = [],
    %% Handle files recursivelly in the directory.
    filelib:fold_files(Dir, RegExp, true, Fun, AccIn).


-spec app_files(AppDir) -> [{AppName, FileName}] when
        AppDir :: filename:dirname(),
        AppName :: atom(),
        FileName :: file:filename().

app_files(AppDir) ->
    Dir = filename:join([AppDir, ebin]),
    Fun = fun(FileName, Acc) ->
        AppName = list_to_atom(filename:basename(FileName, ".app")),
        [{AppName, FileName} | Acc]
        end,

    RegExp = "\.app\.src$",
    AccIn  = [],
    filelib:fold_files(Dir, RegExp, false, Fun, AccIn).


-spec find_app_file(AppDir) -> FileName | undefined when
        AppDir :: filename:dirname(),
        FileName :: file:filename().

find_app_file(AppDir) ->
    case app_src_files(AppDir) of
        [] -> maybe_hd(app_files(AppDir));
        [X|_] -> X
    end.


maybe_hd([X|_]) -> X;
maybe_hd([]) -> undefined.


application_modules_test() ->
    Res = application_modules(code:lib_dir(inferno)),
    io:format(user, "Res: ~p~n", [Res]),
    ok.

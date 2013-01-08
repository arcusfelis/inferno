-module(inferno_app_discover).
-export([]).
-include_lib("inferno/include/inferno.hrl").
-include_lib("eunit/include/eunit.hrl").
-compile({parse_transform, seqbind}).


add_application(AppDir) ->
    #info_application{name = AppName} = CurrentApp = application(AppDir),
    CurrentMods = application_modules(AppDir),
    SavedApp    = inferno_store:application(AppName),
    SavedMods   = inferno_store:application_modules(AppName),
    %% Merge data from 2 directories.
    merge_application(SavedApp, SavedMods, CurrentApp, CurrentMods),
    ok.


remove_application(AppDir) ->
    ok.


-spec application(AppDir) -> #info_application{} when
    AppDir :: file:dirname().

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


%% TODO: write me
merge_application(SavedApp, SavedMods, CurrentApp, CurrentMods) ->
%   ordkeymerge_with(N, Zipper, L1, L2)
    NewMods = lists2:ordkeymerge_with(#info_module.name,
                                      fun merge_module/2,
                                      SavedMods,
                                      CurrentMods),
    NewApp = merge_application(SavedApp, CurrentApp),
    {NewApp, NewMods}.


merge_application(#info_application{directories = D1}, 
       CurrentApp=#info_application{directories = D2}) ->
    CurrentApp#info_application{directories = lists2:unique(D2 ++ D1)}.


%% Merge information about modules from different application directories.
%% This function checks, if new file pathes were added, and updates 
%% the `analyzed' time.
%%
%% A merged modules with new pathes can be updated, because 
%% the `analyzed' field is changed.
merge_module(SavedMod, CurrentMod) ->
    case inferno_lib:merge_modules(SavedMod, CurrentMod) of
        SavedMod  -> SavedMod;
        MergedMod -> MergedMod#info_module{analyzed = merged}
    end.


merge_module_test_() ->
    M1 = #info_module{source_filename = a},
    M2 = #info_module{refman_filename = b},
    M3 = #info_module{analyzed = merged,
                      source_filename = a,
                      refman_filename = b},

    M4 = #info_module{source_filename = c},
    M5 = #info_module{analyzed = merged, 
                      source_filename = c,
                      refman_filename = b},

    [?_assertEqual(M3, merge_module(M1, M2))
    ,?_assertEqual(M5, merge_module(M3, M4))

    ,?_assertEqual(M1, merge_module(M1, M1))
    ,?_assertEqual(M2, merge_module(M2, M2))
    ,?_assertEqual(M4, merge_module(M4, M4))
    ].

%   M1 = #info_module{analyzed = {{2013,1,15},{18,0,0}}},
%   M2 = #info_module{analyzed = {{2013,1,16},{18,0,0}}},
   



%update_application(SavedApp, SavedMods, CurrentApp, CurrentMods) ->
%    {SavedApp, SavedMods}.


%% `CurrentMod' contains only path information and no data from source files.
%%
%% `SavedMod' may contain some information from source files.
%%
%% The update condition is one of the module files was changed 
%% after last analyse.
update_module(SavedMod=#info_module{analyzed = AnalTime}, CurrentMod) ->
    HasNewer = lists:any(fun(FN) -> AnalTime < filelib:last_modified(FN) end,
                         data_filenames(CurrentMod)),
    case HasNewer of
        true  -> {updated, fill_module(CurrentMod)};
        false -> {same, SavedMod}
    end.


%% Extract a list of source files of the module.
data_filenames(CurrentMod) ->
    MaybeFNs = [#info_module.source_filename, #info_module.refman_filename],
    [FN || FN <- MaybeFNs, FN =/= undefined].


%update_module_test_() ->
%    M1 = #info_module{},
%    M2 = #info_module{analyzed = {{2013,1,16},{18,0,0}}},
%    [?_assertEqual(update_module()].

%   M1 = #info_module{analyzed = {{2013,1,15},{18,0,0}}},
%   M2 = #info_module{analyzed = {{2013,1,16},{18,0,0}}},
%

%% @doc Insert information from source files.
fill_module(M@) ->
    M@ = inferno_edoc_slow_reader:fill(M@),
    M@ = inferno_refman_slow_reader:fill(M@),
    M@ = inferno_pos_reader:fill(M@),
    M@.


-module(imferno_app_discover).
-export([]).


%% @doc Return a map from a module name to its source code's location.
-spec files(AppDir) -> [{FileType, FileName}] when
        AppDir :: filename:dirname(),
        FileType :: atom(),
        FileName :: file:filename().

files(AppDir) ->
    Fun = fun(FileName, Acc) ->
        Elem = case filename:extension(FileName) of
                ".beam"    -> {ebin, FileName};
                ".erl"     -> {erl,  FileName};
                ".xml"     -> {doc,  FileName};
                ".app"     -> {app,  FileName};
                ".src"     -> {app,  FileName}
        end,
        [Elem|Acc]
    end,
    RegExp = "(\.beam|\.erl|(doc/.*\.xml)|\.app\.src|\.app)$",
    AccIn  = [],
    %% Handle files recursivelly in the directory.
    lists2:group_pairs(filelib:fold_files(AppDir, RegExp, true, Fun, AccIn)).


mod_name_to_filename_pair(FileName) ->
    {list_to_atom(filename:rootname(FileName)), FileName}.


add_application(AppDir) ->
    %% FileType to FileNames proplist.
    Files   = files(AppDir),
    Beams   = proplists:get_value(beam, Files, []),
    Apps    = proplists:get_value(app, Files, []),
    DocXmls = proplists:get_value(doc, Files, []),
    Erls    = proplists:get_value(erl, Files, []),
    ok.

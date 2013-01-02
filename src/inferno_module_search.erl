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

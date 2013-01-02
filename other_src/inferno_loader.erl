-module(info_loader).
-compile({parse_transform, seqbind}).


-type mode() :: init | mod_title | functions | fun_positions.


-spec check(ReqMode, CurMode, InfoModule, DB) -> InfoModule when
    ReqMode :: Mode,
    CurMode :: Mode,
    Mode :: mode(),
    NewInfoModule :: InfoModule,
    InfoModule :: term(),
    DB :: term().

check(X, X, M, DB) ->
    M; %% already here.
check(fun_positions, CurMode, M@, DB) ->
    M@ = check(functions, CurMode, M@),
    M@ = inferno_pos_reader:fill(M@),
    M@;
check(functions, fun_positions, M) ->
    M; %% already high enough mode.
check(functions, _CurMode, M@) ->
    M@ = inferno_edoc_slow_reader:fill(M@),
    M@ = inferno_refman_slow_reader:fill(M@),
    {save, functions, M@};
check(mod_title, init, M@) ->
    M@ = inferno_edoc_fast_reader:fill(M@),
    M@ = inferno_refman_fast_reader:fill(M@),
    {save, mod_title, M@};
check(_, _, _) -> ok.
        

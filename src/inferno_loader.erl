-module(info_loader).
-compile({parse_transform, seqbind}).


-type mode() :: init | mod_title | functions | fun_positions.


%% @doc If it returns `{save, NewInfoModule}', than `ReqMode' is a new mode.
%% If it returns ok, than `CurMode' is active.
-spec check(ReqMode, CurMode, InfoModule) -> {save, NewInfoModule} | ok when
    ReqMode :: Mode,
    CurMode :: Mode,
    Mode :: mode(),
    NewInfoModule :: InfoModule,
    InfoModule :: term().

check(X, X, InfoModule) -> ok; %% already here.
check(fun_positions, CurMode, M@) ->
    M@ = require(functions, CurMode, M@),
    M@ = inferno_pos_reader:fill(M@),
    {save, fun_positions, M@};
check(functions, fun_positions, M@) ->
    ok; %% already high enough mode.
check(functions, _CurMode, M@) ->
    M@ = inferno_edoc_slow_reader:fill(M@),
    M@ = inferno_refman_slow_reader:fill(M@),
    {save, functions, M@};
check(mod_title, init, M@) ->
    M@ = inferno_edoc_fast_reader:fill(M@),
    M@ = inferno_refman_fast_reader:fill(M@),
    {save, mod_title, M@};
check(_, _, _) -> ok. %% already high enough mode.


require(ReqMode, CurMode, InfoModule) ->
    case check(ReqMode, CurMode, InfoModule) of
        ok                    -> InfoModule;
        {save, NewInfoModule} -> NewInfoModule
    end.
        

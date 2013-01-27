-module(inferno_behaviour).
-export([fill/2]).
-include_lib("inferno/include/inferno.hrl").
-include_lib("eunit/include/eunit.hrl").

fill(InM=#info_module{behaviours = []}, _Cache) ->
    InM;
fill(InM=#info_module{name=M, behaviours=Bs, functions=Fs}, _Cache) ->
    %% Callback's `{Fun, Arity}' to a behaviour's name
    C2B = [{{M,F,A}, B} || B <- Bs, {F, A} <- callbacks(B)],
    C2Bs = lists2:group_pairs(C2B),
    Fs1 = lists2:ordkeymerge_with(#info_function.mfa, 1, 
                                  fun set_behaviour/2, 
                                  Fs, C2Bs),
    Fs2 = filter_undefined(Fs1),
%   io:format(user, "C2Bs: ~p~n", [C2Bs]),
    InM#info_module{functions=Fs2}.


callbacks(Mod) ->
    try
        Mod:behaviour_info(callbacks)
    catch error:_ ->
        []
    end.

set_behaviour(M=#info_function{mfa=MFA}, {MFA, Bs}) ->
    M#info_function{behaviours=Bs};
set_behaviour(M, _) ->
    M.

filter_undefined(Xs) ->
    [X || X <- Xs, X =/= undefined].
    

inferno_server_set_function_callbacks_test() ->
    F1 = #info_function{mfa={inferno_server, handle_call, 3}, 
                        name=inferno_server,
                        arity=3},
    F2 = #info_function{mfa={inferno_server, handle_call, 3}, 
                        name=inferno_server,
                        arity=3,
                        behaviours=[gen_server]},
    M = #info_module{name=inferno_server, 
                     behaviours=[gen_server],
                     functions=[F1]},
    M1 = M#info_module{functions=[F2]},
    M2 = fill(M, undefined),
    ?assertEqual(M1#info_module.functions, 
                 M2#info_module.functions),
    ?assertEqual(M1, M2),
    ok.

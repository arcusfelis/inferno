-module(inferno_behaviour).
-export([callbacks/1]).

callbacks(Mod) ->
    try
        Mod:behaviour_info(callbacks)
    catch error:_ ->
        []
    end.


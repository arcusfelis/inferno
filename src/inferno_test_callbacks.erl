-module(inferno_test_callbacks).
-export([fill/2]).
-include_lib("inferno/include/inferno.hrl").

fill(IM=#info_module{name=Name, functions=Fs}, _Cache) ->
    IsTest = lists:suffix("_tests", atom_to_list(Name)),
    Fs1 = [handle_function(F) || F <- Fs],
    IM#info_module{is_test=IsTest,
                   functions=Fs1}.
    
handle_function(IF=#info_function{name=Name}) ->
    IsTest = lists:suffix("_test", atom_to_list(Name)),
    IF#info_function{is_test=IsTest}.

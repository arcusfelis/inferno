%% @doc Reads module attributes.
-module(inferno_attr_reader).
-export([fill/2]).
-include_lib("inferno/include/inferno.hrl").
-include_lib("eunit/include/eunit.hrl").


fill(InM=#info_module{}, Cache) ->
    try
    fill_from_beam(InM, Cache)
    catch error:Reason ->
        io:format(user, "An error occured during attribute parsing.~n"
                        "Source: ~ts~n"
                        "Reason: ~p~n"
                        "Trace: ~p~n",
                        [InM#info_module.compiled_filename, Reason, 
                         erlang:get_stacktrace()]),
        try
        fill_from_source(InM, Cache)
        catch error:Reason ->
            io:format(user, "An error occured during attribute parsing.~n"
                            "Source: ~ts~n"
                            "Reason: ~p~n"
                            "Trace: ~p~n",
                            [InM#info_module.source_filename, Reason, 
                             erlang:get_stacktrace()]),
            InM
        end
    end.


fill_from_beam(InM=#info_module{compiled_filename = FileName}, Cache)
    when FileName =/= undefined ->
    Attrs = beam_attributes(FileName, Cache),
    Behaviours = proplists:get_value(behaviour, Attrs, []),
    InM#info_module{behaviours=Behaviours}.


%% Read module attributes from BEAM.
beam_attributes(FileName, undefined) ->
    beam_attributes(FileName);
beam_attributes(FileName, Cache) ->
    Hash = [inferno_lib:file_hash(FileName)],
    Key = {attr, FileName},
    case inferno_cache:get(Cache, Key) of
        {Hash, Attrs} ->
            Attrs;
        _Other ->
            beam_attributes(FileName)
    end.
    
beam_attributes(FileName) ->
    {ok, {_ModuleName, [{attributes,Attrs}]}} = 
        beam_lib:chunks(FileName, [attributes]),
    Attrs.


fill_from_source(InM=#info_module{source_filename = FileName}, Cache) ->
    {ok, Fd} = file:open(FileName, []),
    Attrs = try parse_attributes(Fd) after file:close(Fd) end,
    Behaviours = lists:flatmap(fun find_behaviour_attr/1, Attrs),
    InM#info_module{behaviours=Behaviours}.


parse_attributes(Fd) ->
    F = fun is_attribute_form/1,
    parse_form_while(F, Fd, 1).

parse_form_while(F, Fd, StartLine) ->
    {ok, Tree, StartLine2} = epp_dodger:parse_form(Fd, StartLine),
    case F(Tree) of
        true -> [Tree|parse_form_while(F, Fd, StartLine2)];
        false -> []
    end.

is_attribute_form(Form) ->
    Type = erl_syntax:type(Form),
%   io:format(user, "~2nForm of type ~p: ~n~p", [Type, Form]),
    case Type of
        attribute -> true;
        _ -> false
    end.

is_attribute_form(Form, Name) ->                                       
    try                                                                
        erl_syntax:atom_value(erl_syntax:attribute_name(Form)) =:= Name
    catch error:_ ->                                                   
        false                                                          
    end.                                                               

as_list(X) when is_list(X) -> X;
as_list(X) -> [X].

as_elem([X]) -> X;
as_elem(X)   -> X.

to_term(Expr) ->
    {value, Value, _} = erl_eval:exprs(revert_tree(Expr), []),
    Value.

revert_tree(Tree) ->
    [erl_syntax:revert(T) || T <- lists:flatten(Tree)].



find_behaviour_attr(Form) ->
    case is_attribute_form(Form, behaviour) 
    orelse is_attribute_form(Form, behavior) of
        false -> [];
        true ->
            BehName = as_elem(to_term(erl_syntax:attribute_arguments(Form))),
            [BehName]
    end.



inferno_server_attr_reading_test() ->
    FileName = code:lib_dir(inferno) ++ "/src/inferno_server.erl",
    M = fill_from_source(#info_module{source_filename=FileName}, ok),
    ?assertEqual(M#info_module.behaviours, [gen_server]),
    ok.


inferno_server_attr_beam_reading_test() ->
    FileName = code:lib_dir(inferno) ++ "/ebin/inferno_server.beam",
    M = fill_from_beam(#info_module{compiled_filename=FileName}, undefined),
    ?assertEqual(M#info_module.behaviours, [gen_server]),
    ok.

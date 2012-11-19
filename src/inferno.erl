-module(inferno).
-compile(export_all).
-include_lib("xmerl/include/xmerl.hrl").
-compile({parse_transform, seqbind}).

-record(info_module, {
        name, 
        title,
        description
}).

-record(info_function, {
        name, 
        arity, 
        module_name, 
        title, 
        description, 
        is_exported :: boolean()
}).


run() ->
    {_, XML}  = edoc:get_doc(code:lib_dir(gexf) ++ "/src/ellipint.erl"),
    ok.


handle_module(#xmlElement{name = module, attributes = Attrs, content = Con}) ->
    X@ = #info_module{},
    X@ = lists:foldl(fun handle_module_element/2, X@, Con),
    lists:foldl(fun handle_module_attribute/2, X@, Attrs).


handle_module_element(#xmlElement{name = description, content = Con}, X) ->
    lists:foldl(fun handle_module_description/2, X, Con);
handle_module_element(_, X) ->
    X.

handle_module_description(#xmlElement{name = briefDescription, content = Con}, X) ->
    



handle_module_attribute(#xmlAttribute{name = name, value = Value}, X) ->
    X#info_module{name = Value}.

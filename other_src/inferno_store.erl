-module(inferno_store).
-export([new/0, save/2, restore/2]).
-compile({parse_transform, seqbind}).
-record(db, {module_table, function_table}).
-include_lib("inferno/include/inferno.hrl").


new() ->
    FunTable = ets:new(inferno_function_table, [{keypos, #info_function.mfa}]),
    ModTable = ets:new(inferno_module_table, [{keypos, #info_module.name}]),
    #db{module_table = ModTable, function_table = FunTable}.

save(DB, M@) ->
    M@ = save_functions(DB, M@),
    write(DB, M@),
    M@.

restore(DB, IM) ->
    ok.

save_functions(DB, M=#info_module{functions = [_|_]=Fns}) ->
    write(DB, Fns),
    M#info_module{functions = stored};
save_functions(_DB, M) ->
    M.

write(DB=#db{}, [_|_]=Xs) ->
    [write(DB, X) || X <- Xs],
    ok;
write(#db{module_table = T}, X=#info_module{}) ->
    ets:write(T, X),
    ok;
write(#db{function_table = T}, X=#info_function{}) ->
    ets:write(T, X),
    ok.


lookup_fields(Table, Key, FieldNames, N2P) ->
    case ets:lookup(Table, Key) of
        [] -> {error, {no_such_key, Key}};
        [Elem] -> 
            %% For each field, extract its value from `Elem'.
            try
                [element(dict:fetch(FieldName, N2P), Elem)
                 || FieldName <- FieldNames]

            of Values -> {ok, Values}
            catch error:Reason ->
                {error, {no_such_field, Reason}}
            end
    end.


fields_to_position_dict(Fields) ->
    dict:from_list(fields_to_position_pl(Fields)).


fields_to_position_pl(Fields) ->
    fields_to_position_pl(Fields, 2).

fields_to_position_pl([H|T], N) ->
    [{H, N} | fields_to_position_pl(T, N+1)];
fields_to_position_pl([], _N) ->
    [].

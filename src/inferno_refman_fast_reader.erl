%% @doc This module parses refman info into records.
%% It ia a simplified version of `inferno_refman_xml_reader'.
-module(inferno_refman_fast_reader).
-export([fill/1]).

-include_lib("xmerl/include/xmerl.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("inferno/include/inferno.hrl").
-compile({parse_transform, rum}).
-compile({parse_transform, seqbind}).

-record(state, {module, op = fun handle_root/3}).
-record(startElement, {uri, local_name, qualified_name, attributes}).
-record(endElement, {uri, local_name, qualified_name}).


fill(InM=#info_module{refman_filename = FileName}) ->
    case parse_file(FileName) of
        {ok, OutM} ->
            inferno_lib:merge_modules(InM, OutM);
        {error, Reason} ->
            lager:error("Parser error: ~p~n", [Reason]),
            InM
    end.


parse_file(FileName) ->
    try
        State = #state{module = #info_module{}},
        %% EventState is passed as a reason.
        {parsed, _Location, Reason, _EndTags, _EventState} =
            xmerl_sax_parser:file(FileName, [{event_fun, fun handle_event/3},
                                             {event_state, State},
                                             skip_external_dtd]),
        {ok, Reason#state.module}
    catch error:ErrorReason ->
        {error, {error, ErrorReason, erlang:get_stacktrace()}}
    end.


% Event: {startElement,[],"module",{[],"module"},[]}
% Event: {characters,"filename"}
% Event: {endElement,[],"module",{[],"module"}}
% Event: {ignorableWhitespace,"\n  "}
% Event: {startElement,[],"modulesummary",{[],"modulesummary"},[]}
% Event: {characters,"Filename Manipulation Functions"}
% Event: {endElement,[],"modulesummary",{[],"modulesummary"}}

handle_event(E, L, S=#state{op = Op}) ->
%%  io:format(user, "Event: ~p~n", [E]),
    Op(E, L, S).


handle_root(#startElement{local_name = "erlref"}, _, S) ->
    S#state{op = fun handle_erlref/3};
handle_root(#startElement{}=E, _, _S) ->
    throw({bad_root_element, E});
handle_root(_, _, S) ->
    S.


handle_erlref(#startElement{local_name = "module"}, _, S) ->
    handle_module(S);
handle_erlref(_, _, S) ->
    S.



handle_module(S) ->
    S#state{op = fun handle_module/3}.


handle_module({characters, Chars}, _, S) ->
    S#state{module = with(M, M#info_module{name = new_name(Chars)})};
handle_module(#startElement{local_name = "modulesummary"}, _, S) ->
    handle_module_summary(S);
handle_module(_, _, S) ->
    S.


handle_module_summary(S) ->
    S#state{op = fun handle_module_summary/3,
            module = with(M, M#info_module{title = new_chars()})}.


handle_module_summary({characters, Chars}, _, S) ->
    S#state{module = with(M, M#info_module{title = add_chars(Chars, old())})};
handle_module_summary(#endElement{local_name = "modulesummary"}, _, S@) ->
    S@ = S@#state{module = with(M, M#info_module{title = save_chars(old())})},
    throw({parsed, S@});
handle_module_summary(_, _, S) ->
    S.


new_chars() -> [].
add_chars(Old, Chars) -> [Old, Chars].
save_chars(Chars) -> unicode:characters_to_binary(Chars).

new_name(Chars) ->
    binary_to_atom(unicode:characters_to_binary(Chars), utf8).




%% ------------------------------------------------------------------
%% Tests
%% ------------------------------------------------------------------

filename_test() ->
    FileName = code:lib_dir(inferno) ++ "/test/data/filename.xml",
    F1 = fun() -> parse_file(FileName) end,
    F2 = fun(MicroSeconds) -> io:format(user, "~n[refman,fast]Parsed for ~p.~n", [MicroSeconds]) end,
    Res = inferno_lib:measure_time(F1, F2),
    io:format(user, "parse_file/1 returned ~p.~n", [Res]),
    ok.

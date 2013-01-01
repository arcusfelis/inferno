%% @doc This module parses edoc info into records.
%% It is a simplified (and dirty) version of `inferno_edoc_slow_reader'.
%% But for most cases it is a good approximation.
-module(inferno_edoc_fast_reader).
-export([parse_file/1]).

-include_lib("eunit/include/eunit.hrl").
-include_lib("inferno/include/inferno.hrl").
-compile({parse_transform, gin}).

parse_file(FileName) ->
    case file:open(FileName, [binary]) of
        {ok, FD} ->
            try
                parse_line(FD, #info_module{})
            catch error:Reason ->
                {error, Reason}
            after
                file:close(FD)
            end;
        Error -> Error
    end.


parse_line(FD, M=#info_module{}) ->
    case file:read_line(FD) of
        {ok, <<"-module(", Line/binary>>} ->
            ModuleName = hd(binary:split(Line, <<")">>)),
            M#info_module{name = ModuleName}; %% exit from the loop

        {ok, Line} ->
            %% Parse the doc directive.
            %% First line after the directive is a short module description.
            case drop_comment(Line) of
                <<"@doc ", Title/binary>> ->
                    M1 = M#info_module{title = validate_title(Title)},
                    parse_line(FD, M1);
                _ ->
                    parse_line(FD, M)
            end
    end.


drop_comment(<<H, T/binary>>) when in(H, "% ") -> drop_comment(T);
drop_comment(X) -> X.

validate_title(Title) ->
%   "\n ." = lists:sort("\n .") 
    binary2:trim(Title, "\n .").


%% ------------------------------------------------------------------
%% Tests
%% ------------------------------------------------------------------

this_module_test() ->
    FileName = code:lib_dir(inferno) ++ "/src/inferno_lib.erl",
    F1 = fun() -> 
        parse_file(FileName)
     end,
    F2 = fun(MicroSeconds) -> io:format(user, "Parsed for ~p.~n", [MicroSeconds]) end,
    ModRec = inferno_lib:measure_time(F1, F2),
    io:format(user, "ModRec: ~p", [ModRec]),
    ok.

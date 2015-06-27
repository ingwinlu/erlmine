-module(em_printer_json).

-behaviour(em_printer).

-export([print/2]).

print(Field, Moves) ->
    {ok, Width} = em_field:width(Field),
    {ok, Height} = em_field:height(Field),
    MoveList = [handle_selection(Selection) ||
                Selection <- queue:to_list(Moves)],
    Map = #{
        width => Width,
        height => Height,
        movelist => MoveList
    },
    jiffy:encode(Map).

handle_selection({{H,W}, Content}) ->
    #{
        h => H,
        w => W,
        content => Content
    }.

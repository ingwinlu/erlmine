%% -*- coding: utf-8 -*-
-module(em_printer_console).

-behaviour(em_printer).

-export([print/2]).

print(Field, Moves) ->
    FS = build_formatstring(Field),
    Content = build_content(Field, Moves),
    lists:flatten(io_lib:format(FS, Content)).

build_formatstring(Field) ->
    {ok, Width} = em_field:width(Field),
    {ok, Height} = em_field:height(Field),

    lists:flatten(build_header(Width)
    ++ lists:map(
        fun(Nr) -> build_formatstring_row(Nr, Width) end,
        lists:seq(0,Height-1))).

build_header(Width) ->
    Content = [format_string_edge()] ++
        lists:map(fun erlang:integer_to_list/1, lists:seq(0,Width-1)),
    FS = lists:flatten(
        ["~s"]
        ++ lists:duplicate(Width, format_string_element())
        ++ [format_string_line_end()]
    ),
    lists:flatten(io_lib:format(FS, Content)).

build_formatstring_row(RowNr, Width) ->
    [integer_to_list(RowNr)]
    ++ lists:duplicate(Width, format_string_element())
    ++ [format_string_line_end()].

process_moves({empty, _}, Content) ->
    Content;
process_moves({{value, Move = {Key, _}}, NewQueue}, Content) ->
    NewContent = lists:keyreplace(Key, 1, Content, Move),
    process_moves(queue:out(NewQueue), NewContent).
    

build_content(Field, Moves) ->
    {ok, Keys} = em_field:keys(Field),

    Content = lists:map(fun(Key) -> {Key, unknown} end, Keys),
    Content1 = process_moves(queue:out(Moves), Content),
    Content2 = lists:map(fun({_, Cont}) -> content_to_binary(Cont) end, Content1),
    Content2.

format_string_element() -> "~5s".
format_string_line_end() -> "~n".
format_string_edge() -> "#".

content_to_binary(bomb) -> <<"x">>;
content_to_binary(empty) -> <<"">>;
content_to_binary(unknown) -> <<"/">>;
content_to_binary(h_1) -> <<"1">>;
content_to_binary(h_2) -> <<"2">>;
content_to_binary(h_3) -> <<"3">>;
content_to_binary(h_4) -> <<"4">>;
content_to_binary(h_5) -> <<"5">>;
content_to_binary(h_6) -> <<"6">>;
content_to_binary(h_7) -> <<"7">>;
content_to_binary(h_8) -> <<"8">>;
content_to_binary(_) -> erlang:error(unknown_content).

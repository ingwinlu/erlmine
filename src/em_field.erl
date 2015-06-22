%% -*- coding: utf-8 -*-
-module(em_field).

-export([init/0, init/3, print_content/1]).

-record(fieldstate, {
    height = 9,
    width = 9,
    mine_count = 10,
    keys,
    field_dict
}).

% a field uses the following syntax
% # 0 1 2 3 W
% 0 x x x x
% 1 x x x x
% 2 x x x x
% 3 x x x x
% H


% API

init() ->
    init(9, 9, 10).

init(Height, Width, MineCount) ->
    State = init_state(Height, Width, MineCount),
    {ok, State}.

print_content(#fieldstate{width=Width, keys=Keys, field_dict=Field}) ->
    lists:flatten(lists:foldl(
        fun(Key = {_H, W}, Acc) ->
            {_Status, Content} = dict:fetch(Key, Field),
            ContentBinary = content_to_binary(Content),
            case W == Width-1 of
                true -> Acc ++ io_lib:format("~5s~n", [ContentBinary]);
                _    -> Acc ++ io_lib:format("~5s", [ContentBinary])
            end
        end,
        [],
        Keys)).


% priv

init_state(Height, Width, MineCount) ->
    Keys = generate_keys(Height, Width),
    Field = generate_field(Keys),
    Field1 = distribute_mines(Field, Keys, MineCount),
    Field2 = distribute_hints(Field1),
    #fieldstate{
        height = Height,
        width = Width,
        mine_count = MineCount,
        keys = Keys,
        field_dict = Field2
    }.

generate_keys(Height, Width) ->
    [{H, W} || H <- lists:seq(0, Height-1),
               W <- lists:seq(0, Width-1)].

generate_field(Keys) ->
    FieldList = [{Key, {unknown, empty}} || Key <- Keys],
    dict:from_list(FieldList).

distribute_mines(Field, Keys, MineCount) ->
    <<A:32, B:32, C:32>> = crypto:rand_bytes(12),
    random:seed({A, B, C}),

    RandomizedKeys = [X || {_, X} <- lists:sort(
            [{random:uniform(), K} || K <- Keys])
    ],
    MinePositions = lists:sublist(RandomizedKeys, MineCount),

    PlaceMine = fun({Status, _Occupation}) -> {Status, bomb} end,
    MineField = lists:foldl(
        fun(Key, FieldFold) ->
            dict:update(Key, PlaceMine, FieldFold)
        end,
        Field,
        MinePositions),
    MineField.

distribute_hints(Field) ->
    dict:map(
        fun
            (Key, {Status, empty}) ->
                Hint = generate_hint(Key, Field),
                HintAtom = hint_to_content(Hint),
                {Status, HintAtom};
            (_Key, Value) ->
                Value
        end,
        Field).

is_content(Key, Field, Content) ->
    case dict:find(Key, Field) of
        {ok, {_, FieldContent}} -> FieldContent =:= Content;
        _                       -> false
    end.

is_bomb(Key, Field) ->
    is_content(Key, Field, bomb).

generate_nbs({H, W}) ->
    [{H1, W1} || H1 <- lists:seq(H-1, H+1),
                 W1 <- lists:seq(W-1, W+1)].

generate_hint(Key, Field) ->
    NBs = generate_nbs(Key),
    lists:foldl(
        fun(NB, Acc) ->
            case is_bomb(NB, Field) of
                false -> Acc;
                true  -> Acc+1
            end
        end,
        0,
        NBs).

hint_to_content(0) -> empty;
hint_to_content(1) -> h_1;
hint_to_content(2) -> h_2;
hint_to_content(3) -> h_3;
hint_to_content(4) -> h_4;
hint_to_content(5) -> h_5;
hint_to_content(6) -> h_6;
hint_to_content(7) -> h_7;
hint_to_content(8) -> h_8;
hint_to_content(_) -> erlang:error(unknown_hint).

content_to_binary(empty) -> <<"">>;
content_to_binary(bomb) -> <<"x">>;
content_to_binary(h_1) -> <<"1">>;
content_to_binary(h_2) -> <<"2">>;
content_to_binary(h_3) -> <<"3">>;
content_to_binary(h_4) -> <<"4">>;
content_to_binary(h_5) -> <<"5">>;
content_to_binary(h_6) -> <<"6">>;
content_to_binary(h_7) -> <<"7">>;
content_to_binary(h_8) -> <<"8">>;
content_to_binary(_) -> erlang:errror(unknown_content).

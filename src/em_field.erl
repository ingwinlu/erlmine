%% -*- coding: utf-8 -*-
-module(em_field).

-export([init/0, init/3, height/1, width/1, mine_count/1, keys/1, solution/1,
         dict/1, seed/1, select/3]).
-export_type([key/0, field_content/0, selection/0, field/0]).


-record(field, {
    height = 9,
    width = 9,
    mine_count = 10,
    keys,
    solution,
    dict,
    seed
}).

-type key() :: {H :: non_neg_integer(), W :: non_neg_integer()}.
-type field_content() :: empty | bomb |
                         h_1 | h_2 | h_3 | h_4 | h_5 | h_6 | h_7 | h_8.
-type selection() :: {key(), field_content()}.
-type field() :: #field{}.

% a field uses the following syntax
% # 0 1 2 3 W
% 0 x x x x
% 1 x x x x
% 2 x x x x
% 3 x x x x
% H


% API

-spec init() -> {ok, field()}.
init() ->
    init(9, 9, 10).

-spec init(non_neg_integer(), non_neg_integer(), non_neg_integer()) -> {ok, field()}.
init(Height, Width, MineCount) ->
    State = init_state(Height, Width, MineCount),
    {ok, State}.

-spec height(field()) -> {ok, non_neg_integer()}.
height(#field{height=Height}) -> {ok, Height}.

-spec width(field()) -> {ok, non_neg_integer()}.
width(#field{width=Width}) -> {ok, Width}.

-spec mine_count(field()) -> {ok, non_neg_integer()}.
mine_count(#field{mine_count=MineCount}) -> {ok, MineCount}.

-spec keys(field()) -> {ok, list()}.
keys(#field{keys=Keys}) -> {ok, Keys}.

-spec solution(field()) -> {ok, list()}.
solution(#field{solution=Solution}) -> {ok, Solution}.

-spec dict(field()) -> {ok, dict:dict()}.
dict(#field{height=Dict}) -> {ok, Dict}.

-spec seed(field()) -> {ok, {binary(), binary(), binary()}}.
seed(#field{seed=Seed}) -> {ok, Seed}.

-spec select(key(), field(), [key()]) -> {ok, [selection()], [key()] } |
                                         {error, {key(), oob}}.
select(Key, #field{dict=Dict}, Candidates) ->
    handle_select([Key], Dict, [], Candidates).

handle_select([], _Dict, SelectionList, Candidates) ->
    {ok, SelectionList, Candidates};
handle_select([Key | Keys], Dict, SelectionList, Candidates) ->
    NewCandidates = lists:delete(Key, Candidates),
    case dict:find(Key, Dict) of
        {ok, empty} -> handle_select(
                            empty_unknown_nbs(Key, Candidates) ++ Keys,
                            Dict,
                            new_selection(Key, empty, SelectionList),
                            NewCandidates);
        {ok, Content} -> handle_select(
                            Keys,
                            Dict,
                            new_selection(Key, Content, SelectionList),
                            NewCandidates);
        error           -> {error, {Key, oob}}
    end.

new_selection(Key, Content, SelectionList) ->
    [{Key, Content} | SelectionList].

empty_unknown_nbs(Key, Candidates) ->
    NBs = generate_nbs(Key),
    lists:filter(fun(K) -> lists:member(K,Candidates) end, NBs).

% field generation
init_state(Height, Width, MineCount) ->
    Seed = generate_seed(),
    Keys = generate_keys(Height, Width),
    Field = generate_field(Keys),
    {Field1, Solution} = distribute_mines(Field, Keys, MineCount, Seed),
    Field2 = distribute_hints(Field1),
    #field{
        height = Height,
        width = Width,
        mine_count = MineCount,
        keys = Keys,
        solution = Solution,
        dict = Field2,
        seed = Seed
    }.

generate_seed() ->
    <<A:32, B:32, C:32>> = crypto:rand_bytes(12),
    {A, B, C}.

generate_keys(Height, Width) ->
    [{H, W} || H <- lists:seq(0, Height-1),
               W <- lists:seq(0, Width-1)].

generate_field(Keys) ->
    FieldList = [{Key, empty} || Key <- Keys],
    dict:from_list(FieldList).

distribute_mines(Field, Keys, MineCount, Seed) ->
    undefined = random:seed(Seed),

    RandomizedKeys = [X || {_, X} <- lists:sort(
        [{random:uniform(), K} || K <- Keys])
    ],

    {MinePositions, Solution} = lists:split(MineCount, RandomizedKeys),

    MineField = lists:foldl(
        fun(Key, FieldFold) ->
            dict:store(Key, bomb, FieldFold)
        end,
        Field,
        MinePositions),
    {MineField, Solution}.

distribute_hints(Field) ->
    dict:map(
        fun
            (Key, empty) ->
                Hint = generate_hint(Key, Field),
                int_to_content(Hint);
            (_Key, Value) ->
                Value
        end,
        Field).

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

% helpers

is_content(Key, Field, Content) ->
    case dict:find(Key, Field) of
        {ok, FieldContent} -> FieldContent =:= Content;
        _                  -> false
    end.

is_bomb(Key, Field) ->
    is_content(Key, Field, bomb).

generate_nbs({H, W}) ->
    [{H1, W1} || H1 <- lists:seq(H-1, H+1),
                 W1 <- lists:seq(W-1, W+1),
                 H1 >= 0,
                 W1 >= 0,
                 (H1/=H) orelse (W1/=W)].

% convert functions

int_to_content(0) -> empty;
int_to_content(1) -> h_1;
int_to_content(2) -> h_2;
int_to_content(3) -> h_3;
int_to_content(4) -> h_4;
int_to_content(5) -> h_5;
int_to_content(6) -> h_6;
int_to_content(7) -> h_7;
int_to_content(8) -> h_8;
int_to_content(_) -> erlang:error(unknown_hint).

-module(em_game).
-behaviour(gen_server).

%% API.
-export([start_link/0, debug/1]).

%% gen_server.
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

-record(state, {
    field_info,
    field
}).

%% API.

-spec start_link() -> {ok, pid()}.
start_link() ->
    gen_server:start_link(?MODULE, [], []).

debug(Pid) ->
    io:format("~s", [gen_server:call(Pid, debug)]).

%% gen_server.

init([]) ->
    {ok, FieldInfo, Field} = init_field(),
    {ok, #state{field_info=FieldInfo, field=Field}}.

handle_call(debug, _From, State = #state{field=Field, field_info=FieldInfo}) ->
    Output = print_content(Field, FieldInfo),
    {reply, Output, State};
handle_call(_Request, _From, State) ->
    {reply, ignored, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

% priv

% # 0 1 2 3
%
% 0 x x x x
%
% 1 x x x x


init_field() ->
    init_field(9, 9, 10).

init_field(Height, Width, NrMines) ->
    FieldInfo = [
        {height, Height},
        {width, Width},
        {mine_count, NrMines}
    ],
    Field = generate_field(FieldInfo),
    Field1 = distribute_mines(Field, FieldInfo),
    Field2 = distribute_hints(Field1),
    {ok, FieldInfo, Field2}.

generate_keys(FieldInfo) ->
    Height = proplists:get_value(height, FieldInfo),
    Width = proplists:get_value(width, FieldInfo),

    [{H, W} || H <- lists:seq(0, Height-1),
               W <- lists:seq(0, Width-1)].

generate_field(FieldInfo) ->
    Keys = generate_keys(FieldInfo),
    List = [{Key, {unknown, empty}} || Key <- Keys],
    dict:from_list(List).

distribute_mines(Field, FieldInfo) ->
    MineCount = proplists:get_value(mine_count, FieldInfo),

    <<A:32, B:32, C:32>> = crypto:rand_bytes(12),
    random:seed({A, B, C}),

    Keys = dict:fetch_keys(Field),
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
                {Status, Hint};
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
                 W1 <- lists:seq(W-1, W+1].

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

print_content(Field, FieldInfo) ->
    Width = proplists:get_value(width, FieldInfo),
    Keys = generate_keys(FieldInfo),

    lists:flatten(lists:foldl(
        fun(Key = {_H, W}, Acc) ->
            {_Status, Content} = dict:fetch(Key, Field),
            case W == Width-1 of
                true -> Acc ++ io_lib:format("~7w~n", [Content]);
                _    -> Acc ++ io_lib:format("~7w", [Content])
            end
        end,
        [],
        Keys)).

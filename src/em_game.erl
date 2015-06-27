%% -*- coding: utf-8 -*-
-module(em_game).
-behaviour(gen_server).

%% API.
-export([start_link/0, select/2, repr/2, print/1]).

%% gen_server.
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2 ,code_change/3]).

-record(state, {
    field,
    unknown,
    moves=queue:new()
}).

%% API.

-spec start_link() -> {ok, Game :: pid()}.
start_link() ->
    gen_server:start_link(?MODULE, [], []).

-spec select(pid(), em_field:key()) -> [em_field:selection(), ...].
select(Game, Key) ->
    gen_server:call(Game, {select, Key}).

-spec repr(pid(), atom()) -> iolist().
repr(Game, Printer) ->
    gen_server:call(Game, {print, Printer}).

-spec print(pid()) -> ok.
print(Game) ->
    io:format("~s", [gen_server:call(Game, {print, em_printer_console})]).

%% gen_server.

init([]) ->
    self() ! init_field,
    {ok, #state{}}.

handle_call({select, Key = {_,_}}, _From, State = #state{field=Field, unknown=Unknown}) ->
    handle_select(em_field:select(Key, Field, Unknown), State);
handle_call({print, Printer}, _From, State = #state{field=Field, moves=Moves}) ->
    Output = Printer:print(Field, Moves),
    {reply, Output, State};
handle_call(_Request, _From, State) ->
    {reply, ignored, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(init_field, State) ->
    {ok, Field} = em_field:init(),
    {ok, Solution} = em_field:solution(Field),
    {noreply, State#state{field=Field, unknown=Solution}};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

% priv

handle_select({error, Error}, State) ->
    {reply, Error, State};
handle_select({ok, MoveList, NewUnknown}, State = #state{moves=MovesQueue}) ->
    {ok, NewQueue} = handle_new_moves(MovesQueue, MoveList),
    NewState = State#state{moves=NewQueue, unknown=NewUnknown},
    handle_select_reply(MoveList, NewUnknown, NewState).

handle_select_reply(MoveList = [{_Key, bomb}], _Unknown, State) ->
    {stop, select_bomb, MoveList, State};
handle_select_reply(MoveList, [], State) ->
    {stop, won, MoveList, State};
handle_select_reply(MoveList, _Unknown, State) ->
    {reply, MoveList, State}.

handle_new_moves(Queue, []) ->
    {ok, Queue};
handle_new_moves(Queue, [Move | Moves]) ->
    Queue1 = queue:in(Move, Queue),
    handle_new_moves(Queue1, Moves).

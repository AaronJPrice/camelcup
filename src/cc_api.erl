-module(cc_api).
-behaviour(gen_server).

%% API
-export([
  start_link/0,
  new/0,
  new/1,
  save/0,
  load/1,
  make_finish_bet/1,
  move_camel/2,
  place_tile/3,
  take_turn_bet/1
]).

%% gen_server callbacks
-export([
  init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3
]).

-include_lib("cc.hrl").
-define(SERVER, ?MODULE).

%%%=====================================================================================================================
%%% API
%%%=====================================================================================================================
-spec start_link() -> {ok, Pid :: pid()} | ignore | {error, Reason :: term()}.
start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, _Args=[], _Opts=[]).

-spec new() -> ok | {error, term()}.
new() ->
  new(undefined).

-spec new(undefined | cc_game:board()) -> ok | {error, term()}.
new(Board) ->
  call({new, Board}).

-spec save() -> string().
save() ->
  call(save).

-spec load(string()) -> ok.
load(FileName) when is_list(FileName)->
  call({load, FileName}).

make_finish_bet(Camel) when ?IS_CAMEL(Camel) ->
  call({finish_bet, Camel}).

move_camel(Camel, Movement) when ?IS_CAMEL(Camel) and ?IS_MOVEMENT(Movement) ->
  call({move, Camel, Movement}).

place_tile(Actor, Position, Type) when ?IS_ACTOR(Actor) and ?IS_POSITION(Position) and ?IS_TILE(Type) ->
  call({tile, Actor, Position, Type}).

take_turn_bet(Camel) when ?IS_CAMEL(Camel) ->
  call({turn_bet, Camel}).


%%%=====================================================================================================================
%%% gen_server callbacks
%%%=====================================================================================================================
-spec init([]) -> {ok, cc_game:game()}.
init([]) ->
  {ok, State} = cc_game:new_game(undefined),
  {ok, State}.

-spec handle_call(term(), {pid(), term()}, cc_game:game()) -> {reply, term(), cc_game:game()}.
handle_call({new, Board}, _From, State) ->
  case cc_game:new_game(Board) of
    {ok, NewState}  -> {reply, ok, NewState};
    Error           -> {reply, Error, State}
  end;
handle_call({load, FileName}, _From, State) ->
  case cc_game:load(FileName) of
    {ok, NewState}  -> {reply, ok, NewState};
    Error           -> {reply, Error, State}
  end;
handle_call(save, _From, State) ->
  Reply = cc_game:save(State),
  {reply, Reply, State};
handle_call({finish_bet, Camel}, _From, State) ->
  case cc_game:finish_bet(State, Camel) of
    {ok, NewState}  -> {reply, ok, NewState};
    Error           -> {reply, Error, State}
  end;
handle_call({move, Camel, Movement}, _From, State) ->
  case cc_game:move(State, Camel, Movement) of
    {ok, NewState}  -> {reply, ok, NewState};
    Error           -> {reply, Error, State}
  end;
handle_call({tile, Actor, Position, Type}, _From, State) ->
  case cc_game:tile(State, Actor, Position, Type) of
    {ok, NewState}  -> {reply, ok, NewState};
    Error           -> {reply, Error, State}
  end;
handle_call({turn_bet, Camel}, _From, State) ->
  case cc_game:turn_bet(State, Camel) of
    {ok, NewState}  -> {reply, ok, NewState};
    Error           -> {reply, Error, State}
  end;
handle_call(_Request, _From, State) ->
  {reply, ok, State}.

-spec handle_cast(term(), cc_game:game()) -> {noreply, cc_game:game()}.
handle_cast(_Request, State) ->
  {noreply, State}.

-spec handle_info(term(), cc_game:game()) -> {noreply, cc_game:game()}.
handle_info(_Info, State) ->
  {noreply, State}.

-spec terminate(term(), cc_game:game()) -> ok.
terminate(_Reason, _State) ->
  ok.

-spec code_change(term(), cc_game:game(), term()) -> {ok, cc_game:game()}.
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.


%%%=====================================================================================================================
%%% Internal functions
%%%=====================================================================================================================
call(Message) ->
  gen_server:call(?SERVER, Message).

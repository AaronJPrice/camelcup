-module(cc_game).

%% API
-export([
  new_game/1,
  is_game/1,
  save/1,
  load/1,
  finish_bet/2,
  move/3,
  tile/4,
  turn_bet/2
]).

-include_lib("cc.hrl").

-export_type([game/0, board/0]).
-type game()  :: tuple().
-type board() :: map().

-record(game, {
  board       :: map(), %% 16 places
  dice        :: list(),
  has_tile    :: boolean(),
  finish_bets :: list(),
  blue_bets   :: list(),
  green_bets  :: list(),
  orange_bets :: list(),
  white_bets  :: list(),
  yellow_bets :: list()
}).

-define(DIE_MAX, 3).


%%%=====================================================================================================================
%%% API
%%%=====================================================================================================================
new_game(undefined) ->
  new_game(random_starting_board());
new_game(Board) ->
  case is_board(Board) of
    {false, Reason} -> {error, Reason};
    true            -> {ok, create_new_game(Board)}
  end.

is_game(Object) when is_record(Object, game)  -> true;
is_game(_)                                    -> false.

save(State) ->
  case is_game(State) of
    true  -> cc_save:save(State);
    false -> {error, {invalid_state, State}}
  end.

load(FileName) ->
  {ok, Data} = cc_save:load(FileName),
  case is_game(Data) of
    true  -> {ok, Data};
    false -> {error, {invalid_data, Data}}
  end.

finish_bet(Game, Camel) ->
  case is_finish_bet_valid(Game, Camel) of
    true  -> {ok, do_finish_bet(Game, Camel)};
    false -> {error, {bet_already_taken, Game#game.finish_bets}}
  end.

move(Game, Camel, Movement) ->
  case is_move_valid(Game, Camel, Movement) of
    true  -> {ok, do_move(Game, Camel, Movement)};
    false -> {error, camel_already_moved}
  end.

tile(Game, Actor, Position, Type) ->
  case is_tile_placement_valid(Game, Actor, Position, Type) of
    true -> {ok, do_tile_placement(Game, Actor, Position, Type)};
    {false, Reason} -> {error, Reason}
  end.

turn_bet(Game, Camel) ->
  case is_turn_bet_valid(Game, Camel) of
    true  -> {ok, do_turn_bet(Game, Camel)};
    false -> {error, no_bets_remaining}
  end.



%%%=====================================================================================================================
%%% Update functions
%%%=====================================================================================================================
is_finish_bet_valid(Game, Camel) ->
  lists:member(Camel, Game#game.finish_bets).

do_finish_bet(Game, Camel) ->
  NewFinishBets = lists:delete(Camel, Game#game.finish_bets),
  Game#game{finish_bets=NewFinishBets}.

is_turn_bet_valid(Game, Camel) ->
  Bets = camel_to_turn_bet(Game, Camel),
  length(Bets) > 0.

do_turn_bet(Game, Camel) ->
  Bets = camel_to_turn_bet(Game, Camel),
  [_ | NewBets] = Bets,
  case Camel of
    blue   -> Game#game{blue_bets=NewBets};
    green  -> Game#game{green_bets=NewBets};
    orange -> Game#game{orange_bets=NewBets};
    white  -> Game#game{white_bets=NewBets};
    yellow -> Game#game{yellow_bets=NewBets}
  end.

camel_to_turn_bet(Game, Camel) ->
  case Camel of
    blue   -> Game#game.blue_bets;
    green  -> Game#game.green_bets;
    orange -> Game#game.orange_bets;
    white  -> Game#game.white_bets;
    yellow -> Game#game.yellow_bets
  end.

is_tile_placement_valid(Game, self, Position, Type) ->
  case Game#game.has_tile of
    true  -> is_tile_placement_valid(Game, other, Position, Type);
    false -> {false, no_tile_available}
  end;
is_tile_placement_valid(Game, other, Position, _Type) ->
  Place = maps:get(Position, Game#game.board),
  case ?IS_TILE(last(Place)) of
    true  -> {false, already_tile_there};
    false -> true
  end.

do_tile_placement(Game, self, Position, Type) ->
  NewGame = Game#game{has_tile=false},
  do_tile_placement(NewGame, other, Position, Type);
do_tile_placement(Game, other, Position, Type) ->
  Board     = Game#game.board,
  Place     = maps:get(Position, Board),
  NewPlace  = Place ++ [Type],
  NewBoard  = maps:put(Position, NewPlace, Board),
  Game#game{board = NewBoard}.

is_move_valid(Game, Camel, _Movement) ->
  lists:member(Camel, Game#game.dice).

do_move(Game1, Camel, Movement) ->
  Game2 = move_camels(Game1, Camel, Movement),
  Game3 = remove_die(Game2, Camel),
  case are_dice_remaining(Game3) of
    true  -> Game3;
    false -> new_turn(Game3)
  end.

are_dice_remaining(Game) ->
  [] =/= Game#game.dice.

%%TODO Do tile movement
move_camels(Game1, Camel, Movement) ->
  Board1              = Game1#game.board,
  {ok, FromPosition}  = find_camel_position(Board1, Camel),
  Place               = maps:get(FromPosition, Board1),
  ToPosition          = FromPosition+Movement,
  ToPlace             = maps:get(ToPosition, Board1),

  {Moving, Remaining} = split_stack(Place, Camel),

  Board2 = maps:put(FromPosition, Remaining, Board1),
  Board3 = maps:put(ToPosition, Moving++ToPlace, Board2),

  Game1#game{board=Board3}.

split_stack(Stack, Camel) ->
  {ok, CamelIndex}  = get_index(Camel, Stack),
  Remaining         = lists:nthtail(CamelIndex, Stack),
  Moving            = lists:sublist(Stack, CamelIndex),
  {Moving, Remaining}.

remove_die(Game, Die) ->
  NewDice = lists:delete(Die, Game#game.dice),
  Game#game{dice=NewDice}.

get_index(Element, List) ->
  get_index(List, Element, 1).

get_index([], _Element, _Acc) ->
  {error, not_found};
get_index([Head | Tail], Element, Acc) ->
  case Head == Element of
    true -> {ok, Acc};
    false -> get_index(Tail, Element, Acc+1)
  end.

find_camel_position(Board, Camel) when is_map(Board) ->
  find_camel_position(maps:to_list(Board), Camel);
find_camel_position([], _Camel) ->
  {error, camel_missing};
find_camel_position([{Key, Value} | Tail], Camel) ->
  case lists:member(Camel, Value) of
    true  -> {ok, Key};
    false -> find_camel_position(Tail, Camel)
  end.


%%%=====================================================================================================================
%%% Internal functions
%%%=====================================================================================================================
base_turn_bets() -> [five, three, two].

base_dice()         -> camels().
base_finish_bets()  -> camels().
camels()            -> [blue, green, orange, white, yellow].

create_new_game(Board) ->
  #game{
    board       = Board,
    dice        = base_dice(),
    has_tile    = true,
    finish_bets = base_finish_bets(),
    blue_bets   = base_turn_bets(),
    green_bets  = base_turn_bets(),
    orange_bets = base_turn_bets(),
    yellow_bets = base_turn_bets(),
    white_bets  = base_turn_bets()
  }.

new_turn(Game) ->
  Game#game{
    board       = remove_tiles(Game#game.board),
    dice        = base_dice(),
    has_tile    = true,
    blue_bets   = base_turn_bets(),
    green_bets  = base_turn_bets(),
    orange_bets = base_turn_bets(),
    yellow_bets = base_turn_bets(),
    white_bets  = base_turn_bets()
  }.

remove_tiles(Board) ->
  Fun =
    fun({Key, Value}, Map) ->
      Last = last(Value),
      case ?IS_TILE(Last) of
        false -> Map;
        true  -> maps:put(Key, lists:droplast(Value), Map)
      end
    end,
  maps:fold(Fun, Board, Board).

place({Camel, Position}, Board) ->
  case maps:is_key(Position, Board) of
    true  ->
      CurrentStack = maps:get(Position, Board),
      NewStack = [Camel | CurrentStack],
      maps:put(Position, NewStack, Board);
    false ->
      maps:put(Position, [Camel], Board)
  end.

roll_all_dice([], Acc) ->
  lists:reverse(Acc);
roll_all_dice(Dice, Acc) ->
  {Result, NewDice} = roll(Dice),
  roll_all_dice(NewDice, [Result | Acc]).

roll(Dice) ->
  {Camel, NewDice}  = take_die(Dice),
  Number            = roll_die(),
  {{Camel, Number}, NewDice}.

roll_die() ->
  round(rand:uniform(?DIE_MAX)).

take_die(Dice) ->
  NumberToTake = round(random:uniform(length(Dice))),
  Camel = lists:nth(NumberToTake, Dice),
  NewDice = lists:delete(Camel, Dice),
  {Camel, NewDice}.


%%%=====================================================================================================================
%%% Board functions
%%%=====================================================================================================================
random_starting_board() ->
  Dice = base_dice(),
  Results = roll_all_dice(Dice, []),
  lists:foldl(fun place/2, empty_board(), Results).

empty_board() ->
  #{
    1=>[],2=>[],3=>[],4=>[],
    5=>[],6=>[],7=>[],8=>[],
    9=>[],10=>[],11=>[],12=>[],
    13=>[],14=>[],15=>[],16=>[]
  }.

is_board(Board) when is_map(Board) ->
  case lists:sort(maps:keys(Board)) =:= lists:seq(1,16) of
    true  -> check_values_loop(maps:values(Board), camels());
    false -> {false, bad_keys}
  end;
is_board(_) ->
  {false, not_map}.

check_values_loop([], []) ->
  true;
check_values_loop([], MissingCamels) ->
  {false, {missing_camels, MissingCamels}};
check_values_loop([Head | Tail], RemainingCamels) when is_list(Head) ->
  case check_board_space_loop(Head, RemainingCamels) of
    {ok, NewRemainingCamels} -> check_values_loop(Tail, NewRemainingCamels);
    Else -> Else
  end;
check_values_loop([Value | _], _) ->
  {false, {bad_space_definition, Value}}.

check_board_space_loop([], RemainingCamels) ->
  {ok, RemainingCamels};
check_board_space_loop([tile], RemainingCamels) ->
  {ok, RemainingCamels};
check_board_space_loop([Head | Tail], RemainingCamels) ->
  case lists:member(Head, RemainingCamels) of
    true  -> check_board_space_loop(Tail, lists:delete(Head, RemainingCamels));
    false -> {false, {invalid_object, Head}}
  end.


%%%=====================================================================================================================
%%% Utilities
%%%=====================================================================================================================
last([]) ->
  [];
last(List) when is_list(List) ->
  lists:last(List).

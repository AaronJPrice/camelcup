-module(cc_game).

%% API
-export([
  new_game/1,
  is_game/1,
  save/1,
  load/1
]).

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
  yellow_bets :: list(),
  white_bets  :: list()
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
  Data = cc_save:load(FileName),
  case is_game(Data) of
    true  -> {ok, Data};
    false -> {error, {invalid_data, Data}}
  end.


%%%=====================================================================================================================
%%% Internal functions
%%%=====================================================================================================================
base_turn_bets() -> [five, three, two].

base_dice()         -> camels().
base_finish_bets()  -> camels().
camels()            -> [blue, green, orange, yellow, white].

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

new_turn(State) ->
  State#game{
    dice        = base_dice(),
    has_tile    = true,
    blue_bets   = base_turn_bets(),
    green_bets  = base_turn_bets(),
    orange_bets = base_turn_bets(),
    yellow_bets = base_turn_bets(),
    white_bets  = base_turn_bets()
  }.

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

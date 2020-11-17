-define(SAVE_DIR,         "./saves/").
-define(SAVE_FILE_PREFIX, "cc_save_").

-define(IS_CAMEL(Camel), ((Camel == blue) or (Camel == green) or (Camel == orange) or (Camel == white) or (Camel == yellow))).
-define(IS_MOVEMENT(Movement), ((Movement =:= 1) or (Movement =:= 2) or (Movement =:= 3))).
-define(IS_ACTOR(Actor), ((Actor==self) or (Actor==other))).

-define(IS_POSITION(Position), (
  (Position =:= 1) or (Position =:= 2) or (Position =:= 3) or (Position =:= 4)
  or (Position =:= 5) or (Position =:= 6) or (Position =:= 7) or (Position =:= 8)
  or (Position =:= 9) or (Position =:= 10) or (Position =:= 11) or (Position =:= 12)
  or (Position =:= 13) or (Position =:= 14) or (Position =:= 15) or (Position =:= 16)
)).

-define(IS_TILE(Type), ((Type==forward) or (Type==backward))).

-module(cc_save_SUITE).

%% Common Test callbacks
-export([
  all/0,
  init_per_suite/1,
  end_per_suite/1
]).

%% Test cases
-export([
  save_and_load/1
]).

-include_lib("cc.hrl").
-include_lib("eunit/include/eunit.hrl").

%%%=====================================================================================================================
%%% Common Test callbacks
%%%=====================================================================================================================
all() ->
  [
    save_and_load
  ].

init_per_suite(Config) ->
  Config.

end_per_suite(_Config) ->
  ok.

%%%=====================================================================================================================
%%% Test cases
%%%=====================================================================================================================
save_and_load(_Config) ->
  Term      = {[], [], [[[1,2,3],4]], {}, #{a=><<"abc">>}}, %% arbitrary erlang term to test saving
  FileName  = cc_save:save(Term),
  {ok, Actual} = cc_save:load(FileName),
  ?assertEqual(Term, Actual).

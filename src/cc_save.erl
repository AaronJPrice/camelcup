-module(cc_save).

%% API
-export([
  save/1,
  load/1
]).

-include_lib("cc.hrl").


%%%=====================================================================================================================
%%% API
%%%=====================================================================================================================
save(Data) ->
  case file:make_dir(?SAVE_DIR) of
    ok              -> ok;
    {error, eexist} -> ok
  end,
  SaveData = io_lib:format("~p.", [Data]),
  {{Y, Mo, D}, {H, Mi, S}} = calendar:local_time(),
  DateTimeString = lists:flatten(io_lib:format("~p-~p-~p-~p-~p-~p", [Y, Mo, D, H, Mi, S])),
  FileName = ?SAVE_FILE_PREFIX ++ DateTimeString,
  {ok, File} = file:open(?SAVE_DIR ++ FileName, [write]),
  ok = file:write(File, SaveData),
  ok = file:close(File),
  FileName.

load(FileName) ->
  case file:consult(?SAVE_DIR ++ FileName) of
    {ok, [Data]}  -> {ok, Data};
    Else          -> Else
  end.

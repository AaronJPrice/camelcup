-module(cc_api).
-behaviour(gen_server).

%% API
-export([
  start_link/0,
  new/0,
  save/0,
  load/1
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

-define(SERVER, ?MODULE).
-define(SAVE_DIR, "./saves/").
-record(state, {
  blue :: integer(),
  green :: integer(),
  orange :: integer(),
  yellow :: integer(),
  white :: integer()
}).


%%%=====================================================================================================================
%%% API
%%%=====================================================================================================================
-spec start_link() -> {ok, Pid :: pid()} | ignore | {error, Reason :: term()}.
start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, _Args=[], _Opts=[]).

-spec new() -> ok.
new() ->
  gen_server:call(?SERVER, new).

-spec save() -> string().
save() ->
  gen_server:call(?SERVER, save).

-spec load(string()) -> ok.
load(FileName) when is_list(FileName)->
  gen_server:call(?SERVER, {load, FileName}).

%%%=====================================================================================================================
%%% gen_server callbacks
%%%=====================================================================================================================
-spec init([]) -> {ok, #state{}}.
init([]) ->
  {ok, new_state()}.


-spec handle_call(term(), {pid(), term()}, #state{}) -> {reply, ok, #state{}}.
handle_call(new, _From, _State) ->
  {reply, ok, new_state()};
handle_call(save, _From, State) ->
  Reply = save(State),
  {reply, Reply, State};
handle_call({load, FileName}, _From, State) ->
  case do_load(FileName) of
    {ok, NewState} -> {reply, ok, NewState};
    {error, Reason} -> {reply, {error, Reason}, State}
  end;
handle_call(_Request, _From, State) ->
  {reply, ok, State}.

-spec handle_cast(term(), #state{}) -> {noreply, #state{}}.
handle_cast(_Request, State) ->
  {noreply, State}.

-spec handle_info(term(), #state{}) -> {noreply, #state{}}.
handle_info(_Info, State) ->
  {noreply, State}.

-spec terminate(term(), #state{}) -> ok.
terminate(_Reason, _State) ->
  ok.

-spec code_change(term(), #state{}, term()) -> {ok, #state{}}.
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%=====================================================================================================================
%%% Internal functions
%%%=====================================================================================================================
new_state() ->
  #state{}.

save(State) ->
  case file:make_dir(?SAVE_DIR) of
    ok              -> ok;
    {error, eexist} -> ok
  end,
  {{Y, Mo, D}, {H, Mi, S}} = calendar:local_time(),
  DateTimeString = io_lib:format("~p-~p-~p-~p-~p-~p", [Y, Mo, D, H, Mi, S]),
  FilePath = ?SAVE_DIR ++"cc_save_" ++DateTimeString,
  SaveData = io_lib:format("~p.", [State]),
  {ok, File} = file:open(FilePath, [write]),
  ok = file:write(File, SaveData),
  ok = file:close(File),
  FilePath.

do_load(FileName) ->
  FilePath = ?SAVE_DIR ++ FileName,
  case file:consult(FilePath) of
    {ok, [NewState]} when is_record(NewState, state) ->
      lager:info("Loaded save data: ~p", [NewState]),
      {ok, NewState};
    {error, Reason} ->
      lager:info("Failed to load data from ~p due to ~p", [FilePath, Reason]),
      {error, Reason}
  end.

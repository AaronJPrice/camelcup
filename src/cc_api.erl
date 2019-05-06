-module(cc_api).
-behaviour(gen_server).

%% API
-export([
  start_link/0,
  new/1,
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
-record(svr_state, {
  game :: term()
}).


%%%=====================================================================================================================
%%% API
%%%=====================================================================================================================
-spec start_link() -> {ok, Pid :: pid()} | ignore | {error, Reason :: term()}.
start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, _Args=[], _Opts=[]).

-spec new(term()) -> ok.
new(Game) ->
  gen_server:call(?SERVER, {new, Game}).

-spec save() -> string().
save() ->
  gen_server:call(?SERVER, save).

-spec load(string()) -> ok.
load(FileName) when is_list(FileName)->
  gen_server:call(?SERVER, {load, FileName}).

%%%=====================================================================================================================
%%% gen_server callbacks
%%%=====================================================================================================================
-spec init([]) -> {ok, #svr_state{}}.
init([]) ->
  {ok, #svr_state{}}.

-spec handle_call(term(), {pid(), term()}, #svr_state{}) -> {reply, term(), #svr_state{}}.
handle_call({new, Game}, _From, State) ->
  {reply, ok, State#svr_state{game=Game}};
handle_call({load, FileName}, _From, State) ->
  {Reply, NewState} = do_load(FileName, State),
  {reply, Reply, NewState};
handle_call(_, _From, #svr_state{game=undefined}=State) ->
  {reply, {error, undefined_game_state}, State};
handle_call(save, _From, State) ->
  Reply = cc_save:save(State#svr_state.game),
  {reply, Reply, State};
handle_call(_Request, _From, State) ->
  {reply, ok, State}.

-spec handle_cast(term(), #svr_state{}) -> {noreply, #svr_state{}}.
handle_cast(_Request, State) ->
  {noreply, State}.

-spec handle_info(term(), #svr_state{}) -> {noreply, #svr_state{}}.
handle_info(_Info, State) ->
  {noreply, State}.

-spec terminate(term(), #svr_state{}) -> ok.
terminate(_Reason, _State) ->
  ok.

-spec code_change(term(), #svr_state{}, term()) -> {ok, #svr_state{}}.
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.


%%%=====================================================================================================================
%%% Internal functions
%%%=====================================================================================================================
do_load(FileName, State) ->
  case cc_save:load(FileName) of
    {ok, NewGameState}  -> {ok, State#svr_state{game =NewGameState}};
    Error               -> {Error, State}
  end.

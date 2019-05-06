-module(cc_api).
-behaviour(gen_server).

%% API
-export([
  start_link/0,
  new/0,
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
  gen_server:call(?SERVER, {new, Board}).

-spec save() -> string().
save() ->
  gen_server:call(?SERVER, save).

-spec load(string()) -> ok.
load(FileName) when is_list(FileName)->
  gen_server:call(?SERVER, {load, FileName}).

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

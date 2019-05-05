-module(cc_sup).
-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================
start_link() ->
  supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================
init([]) ->
  SupervisorFlags = #{strategy=>one_for_all, intensity=>0, period=>1},

  ApiSpec       = child(cc_api, brutal_kill, worker),
%%  WorkerSupSpec = child(cc_worker_sup, infinity, supervisor),
  ChildSpecs    = [ApiSpec],

  {ok, {SupervisorFlags, ChildSpecs}}.

%%====================================================================
%% Internal functions
%%====================================================================
child(Module, Shutdown, Type) ->
  #{
    id        => Module,
    start     => {Module, _Fun=start_link, _Args=[]},
    restart   => permanent, %% Always restart children
    shutdown  => Shutdown,
    type      => Type,
    modules   => [Module]
  }.

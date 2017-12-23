%%%-------------------------------------------------------------------
%% @doc tcp_perf public API
%% @end
%%%-------------------------------------------------------------------

-module('tcp_perf_app').

-behaviour(application).

-include("../include/metrics.hrl").

%% Application callbacks
-export([start/2
        ,stop/1]).

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
  {ok, Type} = application:get_env(type),
  lager:info("Starting tcp perf ~p", [Type]),
  oneup_metrics:init_from_config(?METRICS_CONFIG),
  tcp_perf_sup:start_link(Type).


%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================

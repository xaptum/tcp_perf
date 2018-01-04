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
  application:ensure_all_started(lager),
  lager:info("Starting tcp perf ~p", [Type]),
  oneup_metrics:add_multiple(?METRICS_CONFIG),
  {ok, _SocketSupPid } = tcp_perf_socket_sup:start_link(),
  {ok, SupPid} = tcp_perf_sup:start_link(Type),
  start_clients(Type),
  {ok, SupPid}.


start_clients(server)->ok;
start_clients(client)->
  {ok, SendHosts} = application:get_env(send_hosts),
  [tcp_perf_sup:start_client(Host) || Host <- SendHosts],
  ok.

%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================

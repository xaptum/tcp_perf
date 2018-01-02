%%%-------------------------------------------------------------------
%%% @author iguberman
%%% @copyright (C) 2017, Xaptum, Inc.
%%% @doc
%%%
%%% @end
%%% Created : 22. Dec 2017 3:28 PM
%%%-------------------------------------------------------------------
-module(tcp_perf_socket_sup).
-author("iguberman").

-behaviour(supervisor).

%% API
-export([start_link/0, start_socket/1]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================
start_link() ->
  supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

init([]) ->
  RestartStrategy = {simple_one_for_one, 60, 3600},

  Metrics = oneup_metrics:initial_get_config(),

  TcpPerfSocketSpec =
    #{id => tcp_perf_socket,
      start => {tcp_perf_socket, start_link, [Metrics]},
      restart => temporary,
      shutdown => 5000},

  {ok, {RestartStrategy, [TcpPerfSocketSpec]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

start_socket(Socket) when is_port(Socket) ->
  supervisor:start_child(?MODULE, [Socket]).
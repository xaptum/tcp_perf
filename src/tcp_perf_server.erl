%%%-------------------------------------------------------------------
%%% @author iguberman
%%% @copyright (C) 2017, Xaptum, Inc.
%%% @doc
%%%
%%% @end
%%% Created : 20. Dec 2017 10:39 AM
%%%-------------------------------------------------------------------
-module(tcp_perf_server).
-author("iguberman").

-behaviour(gen_server).
-behaviour(tcp_perf_socket).

%% API
-export([start_link/1,
  on_packet_received/1]).

%% gen_server callbacks
-export([init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {listen_socket}).

%%%===================================================================
%%% API
%%%===================================================================

start_link(ListenSocket) ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [ListenSocket], []).


%%%===================================================================
%%% tcp_perf_socket callback
%%%===================================================================


on_packet_received(_Packet)->
  ok.


%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([ListenSocket]) ->
  Metrics = oneup_metrics:initial_get_config(),
  oneup_metrics:enable(Metrics),
  gen_server:cast(self(), accept),
  {ok, #state{listen_socket = ListenSocket}}.

handle_call(_Request, _From, State) ->
  {reply, ok, State}.

handle_cast(accept, #state{listen_socket = ListenSocket} = State) ->
  {ok, Socket} = gen_tcp:accept(ListenSocket),
  SocketPid = tcp_perf_socket_sup:start_socket(Socket),
  gen_tcp:cast(SocketPid, {recv, ?MODULE}),
  gen_tcp:cast(self(), accept),
  {noreply, State};
handle_cast(UnknownRequest, _State)->
  lager:warning("Don't know how to handle cast ~p", [UnknownRequest]).

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================


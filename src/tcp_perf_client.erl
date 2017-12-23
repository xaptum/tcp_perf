%%%-------------------------------------------------------------------
%%% @author iguberman
%%% @copyright (C) 2017, Xaptum, Inc.
%%% @doc
%%%
%%% @end
%%% Created : 22. Dec 2017 5:13 PM
%%%-------------------------------------------------------------------
-module(tcp_perf_client).
-author("iguberman").

-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3]).

-define(SERVER, ?MODULE).

-define(SEND_OPTS, [ binary, {packet, 0}, {keepalive, true}, {nodelay, true}]).


-record(state, {host, port, conn_interval, num_sockets, packet_interval, num_packets}).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([Host,  Port, ConnInterval, NumSockets, PacketInterval, NumPackets]) ->
  gen_server:cast(self(), socket),
  {ok, #state{
    host = Host, port = Port,
    conn_interval = ConnInterval, num_sockets = NumSockets,
    packet_interval = PacketInterval, num_packets = NumPackets}}.

handle_call(_Request, _From, State) ->
  {reply, ok, State}.

handle_cast(socket, #state{host = Host, port = Port} = State) ->
  gen_tcp:connect(Host, Port, ?SEND_OPTS),
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

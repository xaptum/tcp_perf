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

-include("../include/metrics.hrl").

%% API
-export([start_link/7]).

%% gen_server callbacks
-export([init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3]).

-define(SERVER, ?MODULE).

-define(SEND_OPTS, [ binary, {packet, 0}, {keepalive, true}, {nodelay, true}]).


-record(state, {host, port, conn_interval, num_sockets, packet_rate, num_packets, packet}).

%%%===================================================================
%%% API
%%%===================================================================

start_link(Port, ConnInterval, NumSockets, PacketInterval, NumsPackets, Packet, Host) when is_atom(Host) ->
  gen_server:start_link({local, Host}, ?MODULE,
    [atom_to_list(Host), Port,
      ConnInterval, NumSockets,
      PacketInterval, NumsPackets, Packet], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([Host,  Port, ConnInterval, NumSockets, PacketInterval, NumPackets, Packet]) ->
  gen_server:cast(self(), send_to_sockets),
  {ok, #state{
    host = Host, port = Port,
    conn_interval = ConnInterval, num_sockets = NumSockets,
    packet_rate = PacketInterval, num_packets = NumPackets, packet = Packet}}.

handle_call(_Request, _From, State) ->
  {reply, ok, State}.

handle_cast(send_to_sockets, #state{host = Host, port = Port,
  conn_interval = ConnInterval, num_sockets = NumSockets,
  packet_rate = PPS, num_packets = NumPackets, packet = Packet} = State) ->
  lager:info("Starting ~b sockets on ~p:~b sending ~b packets at ~b pps ...", [NumSockets, Host, Port, NumPackets, PPS]),
  [start_socket(Host, Port, PPS, NumPackets, Packet, ConnInterval) || _X <- lists:seq(1, NumSockets)],
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

start_socket(Host, Port, PPS, NumPackets, Packet, ConnInterval)->
  lager:debug("Connecting ~p:~b", [Host, Port]),
  {ok, Socket} = gen_tcp:connect(Host, Port, ?SEND_OPTS),
  {ok, SocketPid} = tcp_perf_socket_sup:start_socket(Socket),
  tcp_perf_socket:send(SocketPid, Packet, NumPackets, PPS),
  timer:sleep(ConnInterval).




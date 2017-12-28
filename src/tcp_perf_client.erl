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


-record(state, {host, port, conn_interval, num_sockets, packet_interval, num_packets, packet}).

%%%===================================================================
%%% API
%%%===================================================================

start_link(Host,  Port, ConnInterval, NumSockets, PacketInterval, NumsPackets, Packet) ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [Host,  Port, ConnInterval, NumSockets, PacketInterval, NumsPackets, Packet], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([Host,  Port, ConnInterval, NumSockets, PacketInterval, NumPackets, Packet]) ->
  gen_server:cast(self(), send_to_sockets),
  {ok, #state{
    host = Host, port = Port,
    conn_interval = ConnInterval, num_sockets = NumSockets,
    packet_interval = PacketInterval, num_packets = NumPackets, packet = Packet}}.

handle_call(_Request, _From, State) ->
  {reply, ok, State}.

handle_cast(send_to_sockets, #state{host = Host, port = Port,
  conn_interval = ConnInterval, num_sockets = NumSockets,
  packet_interval = PacketInterval, num_packets = NumPackets, packet = Packet} = State) ->
  Socket = gen_tcp:connect(Host, Port, ?SEND_OPTS),
  SocketPid = tcp_perf_socket_sup:start_socket(Socket),
  gen_tcp:cast(SocketPid, {recv, ?MODULE}),
  gen_tcp:cast(self(), accept),
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

start_socket(Host, Port, PacketInterval, NumPackets)->
  Socket = gen_tcp:connect(Host, Port, ?SEND_OPTS),
  SocketPid = tcp_perf_socket_sup:start_socket(Socket),
  tcp_perf_socket:send_loop().



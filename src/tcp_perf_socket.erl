%%%-------------------------------------------------------------------
%%% @author iguberman
%%% @copyright (C) 2017, Xaptum, Inc.
%%% @doc
%%%
%%% @end
%%% Created : 21. Dec 2017 3:15 PM
%%%-------------------------------------------------------------------
-module(tcp_perf_socket).
-author("iguberman").

-behaviour(gen_server).

-include("../include/metrics.hrl").

%% API
-export([start_link/2,
  send/2,
  send/4,
  send_loop/5,
  recv_loop/3]).

%% gen_server callbacks
-export([
  init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {socket}).

-callback on_packet_received(Socket :: port(), Packet :: binary(), Metrics :: map()) -> Response :: term().

%%%===================================================================
%%% API
%%%===================================================================

start_link(Metrics, Socket) ->
  gen_server:start_link(?MODULE, [Socket, Metrics], []).

send(SocketPid, Packet, N, PPS)->
  gen_server:cast(SocketPid, {send, Packet, N, PPS}).

send(SocketPid, Packet) ->
  gen_server:cast(SocketPid, {send, Packet}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([Socket, Metrics]) ->
  process_flag(trap_exit, true),
  oneup_metrics:enable(Metrics),
  {ok, #state{socket = Socket}}.

handle_call(_Request, _From, State) ->
  {reply, ok, State}.

handle_cast({send, Packet}, #state{socket = Socket} = State) when is_binary(Packet)->
  ok = gen_tcp:send(Socket, Packet),
  {noreply, State};
handle_cast({send, Packet, N, PPS}, #state{socket = Socket} = State) when is_binary(Packet), is_integer(PPS) ->
  Interval = round(1000/PPS),
  spawn_link(?MODULE, send_loop, [Socket, Packet, N, Interval, oneup_metrics:config()]),
  {noreply, State};
handle_cast({recv, Handler}, #state{socket = Socket} = State) ->
  spawn_link(?MODULE, recv_loop, [Socket, Handler, oneup_metrics:config()]),
  {noreply, State};
handle_cast(recv_reply, #state{socket = Socket} = State) ->
  spawn_link(?MODULE, recv_loop, [Socket, self(), oneup_metrics:config()]),
  {noreply, State}.

handle_info(ReplyPacket, #state{socket = Socket} = State) when is_binary(ReplyPacket)->
  gen_tcp:send(Socket, ReplyPacket),
  {noreply, State};
handle_info(_Req, State)->
  {noreply, State}.

terminate(Reason, State) ->
  lager:warning("Terminating with reason ~p, state ~p", [Reason, State]),
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

send_loop(Socket, Message, N, Interval, MetricsMap)->
  oneup_metrics:enable(MetricsMap),
  oneup_metrics:increment(?SOCKETS_SEND_STARTED),
  lager:debug("Starting send_loop of socket ~p", [Socket]),
  send_loop(Socket, Message, N, Interval).

send_loop(_Socket, _Message, 0, _Interval)->
  oneup_metrics:increment(?SOCKETS_SEND_COMPLETE),
  oneup_metrics:increment(?SOCKETS_SEND_STARTED, -1);
send_loop(Socket, Message, N, Interval)->
  case gen_tcp:send(Socket, Message) of
    ok ->
      oneup_metrics:increment(?SEND_PACKETS_SUCCESS),
      timer:sleep(Interval),
      send_loop(Socket, Message, N - 1, Interval);
    {error, Error} ->
      lager:error("Error ~p sending remaining ~b packets from ~p", [Error, N, Socket]),
      oneup_metrics:increment(?SOCKETS_SEND_STARTED, -1),
      oneup_metrics:increment(?SOCKETS_SEND_FAILED),
      oneup_metrics:increment(?SEND_PACKETS_FAILURE, N)
  end.

recv_loop(Socket, Handler, MetricsMap) when is_atom(Handler); is_pid(Handler)->
  Start = current_time(),
  oneup_metrics:enable(MetricsMap),
  HandlerFun =
    if is_pid(Handler) -> fun(Packet) -> Handler ! Packet end;
      true -> fun(Packet) -> Handler:on_packet_received(Socket, Packet, MetricsMap) end
    end,
  oneup_metrics:increment(?SOCKETS_RECV_STARTED),
  case inner_recv_loop(Socket, HandlerFun, 0) of
    {error, timeout} -> oneup_metrics:increment(?SOCKETS_RECV_TIMEOUT);
    {error, closed} -> oneup_metrics:increment(?SOCKETS_RECV_CLOSED);
    {ok, NumPackets} ->
      Stop = current_time(),
      PPS = NumPackets/((Stop - Start) / 1000000),
      oneup_metrics:increment(?SOCKETS_RECV_COMPLETE),
      SocketsComplete = oneup_metrics:get(?SOCKETS_RECV_COMPLETE),
      PrevRate = oneup_metrics:get(?RECV_PACKETS_RATE),
      oneup_metrics:set(?RECV_PACKETS_RATE, round(((PrevRate * (SocketsComplete - 1) + PPS)/SocketsComplete) ) )
  end,
  oneup_metrics:increment(?SOCKETS_RECV_STARTED, -1).


inner_recv_loop(Socket, HandlerFun, NumPackets)->
  case gen_tcp:recv(Socket, 2) of
    {ok, <<Size:16>>} ->
      case gen_tcp:recv(Socket, Size, 2000) of
        {ok, Packet} ->
          oneup_metrics:increment(?RECV_PACKETS_SUCCESS),
          HandlerFun(Packet),
          inner_recv_loop(Socket, HandlerFun, NumPackets + 1);
        {error, timeout} ->
          lager:error("Receiver ~p: Timed out receiving expected ~b bytes", [Socket, Size]),
          {error, timeout};
        {error, Error} ->
          lager:error("Receiver ~p: Error receiving expected ~b bytes: ~p~n", [Socket, Size, Error]),
          {error, Error}
      end;
    {error, Error} ->
      lager:error("Exiting ~p inner_recv_loop after receiving ~b packets due to error ~p", [Socket, NumPackets, Error]),
      {ok, NumPackets}
  end.


current_time() ->
  {Mega, Sec, Micro} = os:timestamp(),
  (Mega * 1000000 + Sec) * 1000000 + Micro.


%%%-------------------------------------------------------------------
%%% @author iguberman
%%% @copyright (C) 2017, Xaptum, Inc.
%%% @doc
%%%
%%% @end
%%% Created : 22. Dec 2017 2:50 PM
%%%-------------------------------------------------------------------
-author("iguberman").

-define(SOCKETS_SEND_STARTED, [send_sockets, started]).
-define(SOCKETS_SEND_COMPLETE, [send_sockets, complete]).
-define(SOCKETS_SEND_FAILED, [send_sockets, failed]).

-define(SEND_PACKETS_SUCCESS, [send_packets, success]).
-define(SEND_PACKETS_FAILURE, [send_packets, failure]).

-define(SOCKETS_RECV_STARTED, [recv_sockets, started]).
-define(SOCKETS_RECV_COMPLETE, [recv_sockets, complete]).
-define(SOCKETS_RECV_TIMEOUT, [recv_sockets, timeout]).
-define(SOCKETS_RECV_CLOSED, [recv_sockets, closed]).

-define(RECV_PACKETS_SUCCESS, [recv_packets, success]).
-define(RECV_PACKETS_FAILED, [recv_packets, failed]).
-define(RECV_PACKETS_RATE, [recv_packets, rate]).


-define(METRICS_CONFIG, [
  ?SOCKETS_SEND_STARTED,
  ?SOCKETS_SEND_COMPLETE,
  ?SOCKETS_SEND_FAILED,
  ?SOCKETS_RECV_STARTED,
  ?SOCKETS_RECV_COMPLETE,
  ?SOCKETS_RECV_TIMEOUT,
  ?SOCKETS_RECV_CLOSED,
  ?SEND_PACKETS_SUCCESS,
  ?SEND_PACKETS_FAILURE,
  ?RECV_PACKETS_SUCCESS,
  ?RECV_PACKETS_FAILED,
  ?RECV_PACKETS_RATE
]).



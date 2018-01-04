%%%-------------------------------------------------------------------
%% @doc tcp_perf top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module('tcp_perf_sup').

-behaviour(supervisor).

%% API
-export([
    start_link/1,
    start_client/1]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link(Type) ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, [Type]).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

init([server]) ->
    RestartStrategy = {one_for_all, 60, 3600},

    {ok, ListenPort} = application:get_env(listen_port),
    {ok, ListenSocket } = listen_socket(ListenPort),

    TcpPerfServerSpec =
        #{id => tcp_perf_server,
            start => {tcp_perf_server, start_link, [ListenSocket]},
            restart => permanent,
            shutdown => 1000},

    {ok, { RestartStrategy, [TcpPerfServerSpec]} };

init([client]) ->
    RestartStrategy = {simple_one_for_one, 60, 3600},

    {ok, SendPort} = application:get_env(send_port),
    {ok, PacketRate}  = application:get_env(packet_rate),
    {ok, ConnInterval} = application:get_env(conn_interval),
    {ok, NumSockets} = application:get_env(num_sockets_per_host),
    {ok, NumPackets} = application:get_env(num_packets_per_socket),
    {ok, PacketSize} = application:get_env(packet_size),
    Packet = create_packet(PacketSize),

    TcpPerfClientSpec =
        #{id => tcp_perf_client,
            start => {tcp_perf_client, start_link, [SendPort, ConnInterval, NumSockets, PacketRate, NumPackets, Packet]},
            restart => temporary,
            shutdown => 1000},

    {ok, {RestartStrategy, [TcpPerfClientSpec]} }.


%%====================================================================
%% Internal functions
%%====================================================================

start_client(SendHost)->
    supervisor:start_child(?MODULE, [SendHost]).

create_packet(PacketSize)->
    Packet = list_to_binary(lists:seq(32, 31 + PacketSize rem 95) ++ lists:duplicate(PacketSize div 95, lists:seq(32, 126))),
    PacketSize = size(Packet), %% sanity check
    lager:info("Created test packet ~p", [Packet]),
    <<PacketSize:16,Packet/binary>>.

listen_socket(Port)->
    case gen_tcp:listen(Port, [binary, {packet, 0}, {active, false} ]) of
        {ok, ListenSocket} ->
            lager:info("SUCCESS gen_tcp:listen got listen socket ~p with recbuf ~p ~n", [ListenSocket, inet:getopts(ListenSocket, [recbuf])]),
            {ok, Port} = inet:port(ListenSocket),
            {ok, ListenSocket};
        {error, Reason} ->
            lager:error("ERROR gen_tcp:listen on port ~b Reason ~p~n", [Port, Reason]),
            {error, {listen_port, Port, Reason} }
    end.
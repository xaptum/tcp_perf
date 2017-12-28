%%%-------------------------------------------------------------------
%% @doc tcp_perf top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module('tcp_perf_sup').

-behaviour(supervisor).

%% API
-export([start_link/1]).

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

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([server]) ->
    RestartStrategy = {one_for_all, 60, 3600},

    TcpPerfServerSpec =
        #{id => tcp_perf_server,
            start => {tcp_perf_server, start_link, []},
            restart => permanent,
            shutdown => 1000},

    {ok, { RestartStrategy, [TcpPerfServerSpec]} };

init([client]) ->
    RestartStrategy = {simple_one_for_one, 60, 3600},

    {ok, SendHosts} = application:get_env(send_hosts),
    {ok, SendPort} = application:get_env(send_port),
    {ok, PacketInterval}  = application:get_env(packet_interval),
    {ok, ConnInterval} = application:get_env(conn_interval),
    {ok, NumSockets} = application:get_env(num_sockets_per_host),
    {ok, NumPackets} = application:get_env(num_packets_per_socket),
    {ok, PacketSize} = appication:get_env(packet_size),
    Packet = create_packet(PacketSize),

    TcpPerfClientSpec =
        #{id => tcp_perf_client,
            start => {tcp_perf_client, start_link, [SendPort, ConnInterval, NumSockets, PacketInterval, NumPackets, Packet]},
            restart => transient,
            shutdown => 1000},

    [start_client(atom_to_list(Host)) || Host <- SendHosts],

    {ok, RestartStrategy, [TcpPerfClientSpec]}.


%%====================================================================
%% Internal functions
%%====================================================================

start_client(SendHost)->
    supervisor:start_child(?MODULE, [SendHost]).


create_packet(PacketSize)->
    Packet = list_to_binary(lists:seq(32, 31 + PacketSize rem 95) ++ lists:duplicate(PacketSize div 95, lists:seq(32, 126))),
    PacketSize = size(Packet), %% sanity check
    lager:info("Created message ~p", [Packet]),
    <<PacketSize:16,Packet/binary>>.
-module(syrup_protocol).
-behaviour(ranch_protocol).

-include("syrup.hrl").

-export([start_link/4]).
-export([init/4]).

start_link(Ref, Socket, Transport, Opts) ->
    Pid = spawn_link(?MODULE, init, [Ref, Socket, Transport, Opts]),
    {ok, Pid}.

init(Ref, ServerSocket, Transport, Opts) ->
    ok = ranch:accept_ack(Ref),
    {ok, ClientSocket} = gen_tcp:connect(Opts#syrup_options.host,
                                         Opts#syrup_options.port,
                                         [{active,false}, {packet,2}]),
    try loop(ServerSocket, Transport, ClientSocket)
    after ok = gen_tcp:close(ClientSocket),
          ok = Transport:close(ServerSocket)
    end.


loop(ServerSocket, Transport, ClientSocket) ->
    case Transport:recv(ServerSocket, 0, 1000) of
        {ok, Request} ->
            error_logger:info_msg("Relaying request: ~p~n", [Request]),
            ok = gen_tcp:send(ClientSocket, Request),
            {ok, Response} = gen_tcp:recv(ClientSocket, 0),

            error_logger:info_msg("Relaying response: ~p~n", [Response]),
            Transport:send(ServerSocket, Response),

            loop(ServerSocket, Transport, ClientSocket);
        _ -> ok
    end.

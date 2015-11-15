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
    try loop(ServerSocket, Transport, ClientSocket, Opts)
    after ok = gen_tcp:close(ClientSocket),
          ok = Transport:close(ServerSocket)
    end.


loop(ServerSocket, Transport, ClientSocket, Opts) ->
    case Transport:recv(ServerSocket, 0, 1000) of
        {ok, Request} ->
            % Relay the request
            ok = gen_tcp:send(ClientSocket, Request),
            {ok, Response} = gen_tcp:recv(ClientSocket, 0),

            % If we're asked to, wait a while
            case Opts#syrup_options.latency of
                0 -> ok;
                Time -> timer:sleep(Time)
            end,

            % Relay the response
            Transport:send(ServerSocket, Response),

            % Iterate!
            loop(ServerSocket, Transport, ClientSocket, Opts);
        _ -> ok
    end.

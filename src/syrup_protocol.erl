-module(syrup_protocol).
-behaviour(ranch_protocol).

-include("syrup.hrl").

-export([start_link/4]).
-export([init/4]).

start_link(Ref, Socket, Transport, Opts) ->
    Pid = spawn_link(?MODULE, init, [Ref, Socket, Transport, Opts]),
    {ok, Pid}.

init(Ref, ServerSocket, Transport, Opts) ->
    process_flag(trap_exit, true),

    ok = ranch:accept_ack(Ref),
    {ok, ClientSocket} = gen_tcp:connect(Opts#syrup_options.host,
                                         Opts#syrup_options.port,
                                         [{active,false}, {reuseaddr, true}]),

    try RequestPid = spawn_link(fun() -> relayRequest(ServerSocket, Transport, ClientSocket, Opts) end),
        ResponsePid = spawn_link(fun() -> relayResponse(ServerSocket, Transport, ClientSocket, Opts) end),
        receive
            {'EXIT', RequestPid, normal} -> ok;
            {'EXIT', ResponsePid, normal} -> ok;
            UnknownMessage -> error_logger:error_msg("Received unknown message: ~p~n", [UnknownMessage]),
                              error({unknown_message, UnknownMessage})
        end
    after ok = gen_tcp:close(ClientSocket),
          ok = Transport:close(ServerSocket)
    end.


relayRequest(ServerSocket, Transport, ClientSocket, Opts) ->
    case Transport:recv(ServerSocket, 0, 1000) of
        {ok, Request} ->
            ok = gen_tcp:send(ClientSocket, Request),

            % Iterate!
            relayRequest(ServerSocket, Transport, ClientSocket, Opts);
        _ -> ok
    end.

relayResponse(ServerSocket, Transport, ClientSocket, Opts) ->
    case gen_tcp:recv(ClientSocket, 0) of
        {ok, Response} ->

            % If we're asked to, wait a while
            case Opts#syrup_options.latency of
                0 -> ok;
                Time -> timer:sleep(Time)
            end,

            % Relay the response
            Transport:send(ServerSocket, Response),

            % Iterate!
            relayResponse(ServerSocket, Transport, ClientSocket, Opts);
        _ -> ok
    end.

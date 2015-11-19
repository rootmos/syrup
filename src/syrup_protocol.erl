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
    TcpOpts = [{active,once}, {sndbuf, 100000}, {recbuf, 100000}],
    {ok, ClientSocket} = gen_tcp:connect(Opts#syrup_options.host,
                                         Opts#syrup_options.port,
                                         []),
    inet:setopts(ServerSocket, TcpOpts),
    inet:setopts(ClientSocket, TcpOpts),

    try loop(ServerSocket, Transport, ClientSocket, Opts)
    after ok = gen_tcp:close(ClientSocket),
          ok = Transport:close(ServerSocket)
    end.

loop(ServerSocket, Transport, ClientSocket, Opts) ->
    receive
        {inet_reply, _, ok} ->
            loop(ServerSocket, Transport, ClientSocket, Opts);
        {inet_reply, _, Status} ->
            exit(Status);

        {tcp, ServerSocket, Request} ->
            try erlang:port_command(ClientSocket, Request)
            catch error:Error -> exit(Error)
            end,
            %%ok = gen_tcp:send(ClientSocket, Request),
            inet:setopts(ServerSocket, [{active, once}]),
            loop(ServerSocket, Transport, ClientSocket, Opts);
        {tcp, ClientSocket, Response} ->
            % If we're asked to, wait a while
            case Opts#syrup_options.latency of
                0 -> ok;
                Time -> timer:sleep(Time)
            end,

            try erlang:port_command(ServerSocket, Response)
            catch error:Error -> exit(Error)
            end,

            inet:setopts(ClientSocket, [{active, once}]),
            loop(ServerSocket, Transport, ClientSocket, Opts);
        _ -> ok
    end.

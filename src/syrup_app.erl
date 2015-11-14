-module(syrup_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

-record(state, {restServer}).

start(_StartType, _StartArgs) ->
    {ok, Pid} = syrup_sup:start_link(),

    Dispatch = cowboy_router:compile([
                                      {'_', [
                                              {"/tcp/:port", syrup_rest_tcp, []}
                                            ]
                                      }
                                     ]),
    {ok, RestServer} = cowboy:start_http(http, 1, [{port, syrup_rest_port()}],
                                         [ {env, [{dispatch, Dispatch}]}
                                         ]),
    {ok, Pid, #state{restServer = RestServer}}.

stop(State) ->
    ok = cowboy:stop_listener(State#state.restServer).

syrup_rest_port() ->
    case os:getenv("SYRUP_REST_PORT") of
        false -> 8080;
        Value -> list_to_integer(Value)
    end.

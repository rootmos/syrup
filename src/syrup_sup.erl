-module(syrup_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).
-export([start_listener/1]).
-export([stop_listener/1]).
-export([listener_exists/1]).

%% Supervisor callbacks
-export([init/1]).

-define(LISTENER(Port), list_to_atom(integer_to_list(Port))).
-define(LISTENER_ID(Port), {ranch_listener_sup, ?LISTENER(Port)}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start_listener(Port) -> do_start_listener(Port).
stop_listener(Port) -> do_stop_listener(Port).
listener_exists(Port) -> do_listener_exist(Port).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) -> {ok, { {one_for_one, 5, 10}, []} }.

%% ===================================================================
%% Internal functions
%% ===================================================================

do_start_listener(Port) ->
    ListenerSpec = ranch:child_spec(?LISTENER(Port), 100, ranch_tcp,
                                    [{port, Port}], syrup_protocol, []),
    Result = case supervisor:start_child(?MODULE, ListenerSpec) of
                 {ok, Pid} -> {ok, Pid};
                 {ok, Pid, _} -> {ok, Pid};
                 {error, {already_started, Pid}} -> {ok, Pid};
                 {error, Error} -> {error, Error}
             end,

    case Result of
        {ok, ListenerPid} ->
            error_logger:info_msg("Started listener. Port: ~p. Pid: ~p~n", [Port, ListenerPid]);
        {error, ListenerError} ->
            error_logger:error_msg("Failed to start worker. Port: ~p.~nReason: ~p~n", [Port, ListenerError])
    end,

    Result.

do_stop_listener(Port) ->
    case supervisor:terminate_child(?MODULE, ?LISTENER_ID(Port)) of
        ok -> ok = supervisor:delete_child(?MODULE, ?LISTENER_ID(Port)),
              error_logger:info_msg("Stopped listener. Port: ~p~n", [Port]),
              ok;
        {error, not_found} ->
            error_logger:error_msg("Unable to stop listener: Port: ~p.", [Port]),
            {error, not_found}
    end.

do_listener_exist(Port) ->
    Children = supervisor:which_children(?MODULE),
    case proplists:lookup(?LISTENER_ID(Port), Children) of
        none -> false;
        _Value -> true
    end.


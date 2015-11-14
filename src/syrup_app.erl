-module(syrup_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    {ok, Pid} = syrup_sup:start_link(),
    {ok, _} = syrup_sup:start_listener(5000),
    {ok, _} = syrup_sup:start_listener(6000),
    {ok, Pid}.

stop(_State) ->
    ok.

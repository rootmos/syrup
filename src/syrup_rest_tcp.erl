-module(syrup_rest_tcp).

-export([init/3]).
-export([content_types_accepted/2]).
-export([allowed_methods/2]).
-export([delete_resource/2]).
-export([resource_exists/2]).

-export([from_url/2]).
-export([from_anything/2]).

%% ===================================================================
%% Callbacks
%% ===================================================================

init({tcp, http}, _Req, _Opts) ->
    {upgrade, protocol, cowboy_rest}.

content_types_accepted(Req, State) ->
        {[ {{<<"application">>, <<"x-www-form-urlencoded">>, []}, from_url}
         , {'*', from_anything}
         ],
         Req, State}.

allowed_methods(Req, State) -> {[<<"PUT">>, <<"DELETE">>], Req, State}.

from_url(Req, State) ->
    case cowboy_req:binding(port, Req) of
        {undefined, _Req2} -> error(no_port);
        {Port, Req2} -> ok = do_create(binary_to_integer(Port)),
                        {true, Req2, State}
    end.

from_anything(Req, State) ->
    case cowboy_req:binding(port, Req) of
        {undefined, _Req2} -> error(no_port);
        {Port, Req2} -> ok = do_create(binary_to_integer(Port)),
                        {true, Req2, State}
    end.

delete_resource(Req, State) ->
    case cowboy_req:binding(port, Req) of
        {undefined, _Req2} -> error(no_port);
        {Port, Req2} -> ok = do_delete(binary_to_integer(Port)),
                        {true, Req2, State}
    end.

resource_exists(Req, State) ->
    case cowboy_req:binding(port, Req) of
        {undefined, _Req2} -> error(no_port);
        {Port, Req2} -> {syrup_sup:listener_exists(binary_to_integer(Port)), Req2, State}
    end.

%% ===================================================================
%% Internal functions
%% ===================================================================

do_create(Port) ->
    {ok, _} = syrup_sup:start_listener(Port),
    ok.

do_delete(Port) ->
    ok = syrup_sup:stop_listener(Port).

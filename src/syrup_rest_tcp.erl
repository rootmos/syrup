-module(syrup_rest_tcp).

-include("syrup.hrl").

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
    from_anything(Req, State).

from_anything(Req, State) ->
    case cowboy_req:binding(port, Req) of
        {undefined, _Req2} -> error(no_port);
        {Port, Req2} ->
            {Opts, Req3} = extract_options(Req2),
            ok = do_create(binary_to_integer(Port), Opts),
            {true, Req3, State}
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

do_create(Port, Opts) ->
    {ok, _} = syrup_sup:start_listener(Port, Opts),
    ok.

do_delete(Port) ->
    ok = syrup_sup:stop_listener(Port).

extract_options(Req) ->
    {List, Req2} = cowboy_req:qs_vals(Req),

    Host = case proplists:lookup(<<"host">>, List) of
               none -> error(no_host);
               {_Key1, Value1} -> binary_to_list(Value1)
           end,

    Port = case proplists:lookup(<<"port">>, List) of
               none -> error(no_port);
               {_Key2, Value2} -> binary_to_integer(Value2)
           end,

    MandatoryOpts = #syrup_options{host = Host, port = Port},

    LatencyOpts = case proplists:lookup(<<"latency">>, List) of
                      none -> MandatoryOpts;
                      {_Key3, Value3} ->
                          MandatoryOpts#syrup_options{latency = binary_to_integer(Value3)}
                  end,

    {LatencyOpts, Req2}.
    

%%%-------------------------------------------------------------------
%% @doc dpserv public API
%% @end
%%%-------------------------------------------------------------------

-module(dpserv_app).

-behaviour(application).
-include("common_functions.hrl").

%% Application callbacks
-export([start/2, stop/1, output_hook/4]).

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
    Dispatch = cowboy_router:compile([
        {'_', [{"/", test_handler, []}
              % Idea: use coll name as var, use constraint to orient
              ,{"/avis/:number",        dpserv_handler,#{collection => adv, lang => fr}}
              ,{"/adviezen/:number",    dpserv_handler,#{collection => adv, lang => nl}}
              ,{"/gutachten/:number",   dpserv_handler,#{collection => adv, lang => de}}

              ,{"/avis/:number/projet",        dpserv_handler,#{collection => adv_proj, lang => fr}}
              ,{"/adviezen/:number/ontwerp",    dpserv_handler,#{collection => adv_proj, lang => nl}}
              ,{"/gutachten/:number/entwurf",   dpserv_handler,#{collection => adv_proj, lang => de}}

              ]}
    ]),

    {ok, _} = cowboy:start_clear(http_listener, 100,
        [{port,9088}],
        #{env => #{dispatch => Dispatch}, onresponse => fun output_hook/4}
    ),

    dpserv_sup:start_link().

%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%--------------------------------------------------------------------
output_hook(404, Headers, <<>>, Req) ->
    Body = <<"404 Not Found">>,
    ?DBG([headers],[Headers]),
    HeadersOut = lists:keyreplace(<<"content-length">>, 1, Headers,
                    {<<"content-length">>, integer_to_list(byte_size(Body))}),
    cowboy_req:reply(404, HeadersOut, Body, Req);

output_hook(C, _, _, Req) ->
    io:format("Output hook, status code: ~p\n",[C]),
    Req.

%%====================================================================
%% Internal functions
%%====================================================================

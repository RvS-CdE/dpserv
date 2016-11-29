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
              %% Single Documents
              ,{"/1/:ln/:number",        dpserv_h_doc,      #{base => <<"/1">>, store => adv}}
              ,{"/1/:ln/:number/meta",   dpserv_h_meta, #{base => <<"/1">>, store => adv}}
              ,{"/1/:ln/:number/projet", dpserv_h_doc,      #{base => <<"/1">>, store => adv_proj, lang => fr}}
              ,{"/1/:ln/:number/ontwerp", dpserv_h_doc,      #{base => <<"/1">>, store => adv_proj, lang => nl}}
              ,{"/1/:ln/:number/entwurf", dpserv_h_doc,      #{base => <<"/1">>, store => adv_proj, lang => de}}

              %% Document Collections
              ,{"/1/:ln/:number/col/all",   dpserv_collection_handler, #{base => <<"/1">>, collection => all}}

              %,{"/avis/:number",        dpserv_handler,#{collection => adv, lang => fr}}
              %,{"/adviezen/:number",    dpserv_handler,#{collection => adv, lang => nl}}
              %,{"/gutachten/:number",   dpserv_handler,#{collection => adv, lang => de}}

              %,{"/avis/:number/meta",        dpserv_meta_handler,#{collection => adv, lang => fr}}
              %,{"/adviezen/:number/meta",    dpserv_meta_handler,#{collection => adv, lang => nl}}
              %,{"/gutachten/:number/meta",   dpserv_meta_handler,#{collection => adv, lang => de}}

              %,{"/avis/:number/projet",        dpserv_handler,#{collection => adv_proj, lang => fr}}
              %,{"/adviezen/:number/ontwerp",   dpserv_handler,#{collection => adv_proj, lang => nl}}
              %,{"/gutachten/:number/entwurf",  dpserv_handler,#{collection => adv_proj, lang => de}}

              ]}
    ]),
    {ok, _} = cowboy:start_clear(http_listener, 100,
        [{port,9088}],
        #{env => #{dispatch => Dispatch, onresponse => fun ?MODULE:output_hook/4}, onresponse => fun ?MODULE:output_hook/4}
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
    lager:debug("Output sent by output_hook"),
    io:format("Output hook, status code: ~p\n",[C]),
    Req.

%%====================================================================
%% Internal functions
%%====================================================================
